use std;
use std::result;
use std::thread;
use std::sync::mpsc::{Receiver, Sender, SendError, RecvError};
use std::sync::mpsc;

use std::fmt::Write;

use std::fs::File;
use std::io::Read;
use std::io::Write as IoWrite;

use std::thread::JoinHandle;
use super::ControlMessage;
use super::command::*;
use super::Gameboy;
use super::EventEntry;
use cogboy_core::cpu;
use cogboy_core::cpu::Cpu;


use rustyline::error::ReadlineError;
use rustyline::Editor;

use serde_json;

#[derive(Debug)]
pub struct SystemDebugSummary {
    regs: cpu::Regs,
    cycles: u64,
    break_cycle: u64,
    rom_bank: u8,
}

impl SystemDebugSummary {
    pub fn new(gb: &Gameboy) -> SystemDebugSummary {
        SystemDebugSummary {
            regs: gb.cpu.regs.clone(),
            cycles: gb.cpu.cycles,
            rom_bank: gb.cpu.bus.cartridge.rom_bank,
            break_cycle: gb.break_cycle
        }
    }
}

#[derive(Debug)]
pub enum DebugResponse {
    Summary(SystemDebugSummary),
    Disassembly(String),
    CpuSnapshot(Box<Cpu>),
    Step(u16),
    Breakpoints(Vec<u16>),
}

pub enum DebugRequest {
    GetSummary,
    GetDisassembly(Option<u16>, Option<u16>),
    GetCpuSnapshot,
    SetCpuSnapshot(Box<Cpu>),
    Step,
    Continue,
    SetBreakpoints(Vec<Breakpoint>),
    ListBreakpoints
}

impl DebugRequest {
    fn disassemble(cpu: &Cpu, base: u16, len: u16) -> String {
        let mut offset = 0;
        let mut output = String::with_capacity(256);
        while offset < len {
            let addr = base.wrapping_add(offset);
            if let Ok((inst,_)) = cpu::Instruction::decode(addr, &cpu.bus) {
                writeln!(output, "0x{:04x}: {}", addr, inst);
                offset += inst.length() as u16;
            }
            else {
                writeln!(output, "0x{:04x}: Unrecognized opcode", addr);
                offset += 1;
            }
        }

        output
    }

    pub fn invoke(&self, gameboy: &mut Gameboy) -> DebugResponse {
        match *self {
            DebugRequest::GetSummary => {
                DebugResponse::Summary(SystemDebugSummary::new(gameboy))
            }
            DebugRequest::GetDisassembly(addr, len) => {
                let cpu = &gameboy.cpu;
                DebugResponse::Disassembly(DebugRequest::disassemble(cpu, addr.unwrap_or(cpu.regs.pc), len.unwrap_or(10)))
            },
            DebugRequest::GetCpuSnapshot => DebugResponse::CpuSnapshot(Box::new(gameboy.cpu.clone())),
            DebugRequest::SetCpuSnapshot(ref cpu) => {
                gameboy.cpu.clone_from(cpu);
                gameboy.target_cycles = gameboy.cpu.cycles;
                while let Some(ev) = gameboy.passed_events.pop() {
                    if ev.time > gameboy.cpu.cycles {
                        gameboy.pending_events.push_front(ev);
                    }
                    else {
                        gameboy.passed_events.push(ev);
                        break;
                    }
                }
                DebugResponse::Summary(SystemDebugSummary::new(gameboy))
            }
            DebugRequest::Step => {
                // TODO: Better error handling
                if gameboy.running {
                    println!("Pausing!");
                    gameboy.pause();
                }
                else
                {
                    println!("Stepping!");
                    let pre_inst_count = gameboy.cpu.instruction_counter;
                    while pre_inst_count == gameboy.cpu.instruction_counter {
                        gameboy.cpu.step().unwrap();
                    }
                }
                DebugResponse::Step(gameboy.cpu.regs.pc)
            }
            DebugRequest::Continue => {
                gameboy.play();
                DebugResponse::Summary(SystemDebugSummary::new(gameboy))
            },
            DebugRequest::SetBreakpoints(ref breakpoints) => {
                gameboy.breakpoints.clear();
                gameboy.break_cycle = std::u64::MAX;

                for bp in breakpoints {
                    match *bp {
                        Breakpoint::Code(addr) => {
                            gameboy.breakpoints.insert(addr);
                        }
                        Breakpoint::Cycle(cycle) => {
                            if gameboy.break_cycle != std::u64::MAX {
                                panic!("TODO: Only one break cycle supported for now")
                            }
                            gameboy.break_cycle = cycle;
                        }
                    }
                }
                let addrs = (&gameboy.breakpoints).into_iter().map(|x| *x).collect();
                DebugResponse::Breakpoints(addrs)
            },
            DebugRequest::ListBreakpoints => {
                let addrs = (&gameboy.breakpoints).into_iter().map(|x| *x).collect();
                DebugResponse::Breakpoints(addrs)
            },
        }
    }
}

#[derive(Debug)]
enum Error {
    Send(SendError<ControlMessage>),
    Receive(RecvError),
    UnexpectedResponse,
}

type Result<T> = result::Result<T, Error>;

struct Debugger {
    message_tx: Sender<ControlMessage>,
    response_rx: Receiver<DebugResponse>,
    response_tx: Sender<DebugResponse>,
    cursor: u16,
    breakpoints: Vec<Breakpoint>,
    snapshot: Option<Box<Cpu>>
}

impl Debugger {
    pub fn new(message_tx: Sender<ControlMessage>) -> Debugger
    {
        let (response_tx, response_rx) = mpsc::channel();
        let snapshot: Option<Box<Cpu>> = File::open("gameboy.state")
                        .ok()
                        .map(|f| serde_json::from_reader(f).ok())
                        .unwrap_or(None);
        Debugger {
            message_tx: message_tx,
            response_rx: response_rx,
            response_tx: response_tx,
            breakpoints: Vec::new(),
            cursor: 0x100,
            snapshot: snapshot
        }
    }

    fn handle_response(&mut self, response: DebugResponse) {
        match response {
            DebugResponse::Step(pc) => {
                self.cursor = pc;
            }
            DebugResponse::Summary(summary) => {
                println!("Regs are {:?}", summary);
            }
            DebugResponse::Disassembly(dis) => {
                println!("{}", dis);
            }
            DebugResponse::CpuSnapshot(cpu) => {
                println!("Received system snapshot");
                if let Ok(mut f) = File::create("gameboy.state") {
                    if let Ok(serialized) = serde_json::to_vec(&cpu) {
                        f.write(serialized.as_slice());
                    }
                }
                self.snapshot = Some(cpu);
            }
            DebugResponse::Breakpoints(ref addresses) => {
                for addr in addresses {
                    println!("{:04x}", addr);
                }
            }
        }
    }

    pub fn send(&mut self, req: DebugRequest) -> Result<DebugResponse> {
        self.message_tx
            .send(ControlMessage::Debug(req, self.response_tx.clone()))
            .map_err(|err| Error::Send(err))?;

        self.response_rx.recv().map_err(|err| Error::Receive(err))
    }

    pub fn send_and_handle(&mut self, req: DebugRequest) -> Result<()> {
        match self.send(req) {
            Ok(response) => {
                self.handle_response(response);
                Ok(())
            },
            Err(error) => Err(error)
        }
    }

    fn process_command(&mut self, command: Command) {
        match command {
            Command::Exit => {
                self.message_tx.send(ControlMessage::Quit);
                panic!("TODO: Clean exit")
            }
            Command::Save => {
                self.send_and_handle(DebugRequest::GetCpuSnapshot).unwrap()
            }
            Command::Load => {
                let maybe_snap = self.snapshot.clone();
                if let Some(cpu_snapshot) = maybe_snap {
                    self.send_and_handle(DebugRequest::SetCpuSnapshot(cpu_snapshot)).unwrap()
                } else {
                    println!("No snapshot to load");
                }
            }
            Command::ShowRegs => {
                self.send_and_handle(DebugRequest::GetSummary).unwrap()
            }
            Command::Disassemble(count) => {
                let cursor = self.cursor;
                self.send_and_handle(DebugRequest::GetDisassembly(Some(cursor), Some(count.unwrap_or(10) as u16))).unwrap()
            }
            Command::Goto(addr) => {
                self.cursor = addr as u16;
            }
            Command::Step => {
                self.send_and_handle(DebugRequest::Step).unwrap();
                self.process_command(Command::Disassemble(None))
            }
            Command::Continue => {
                self.send_and_handle(DebugRequest::Continue).unwrap();
            }
            Command::AddBreakpoint(bp) => {
                self.breakpoints.push(bp);
                let breakpoints_cloned = self.breakpoints.clone();
                self.send_and_handle(DebugRequest::SetBreakpoints(breakpoints_cloned)).unwrap();
            }
            Command::ListBreakpoints => {
                for bp in self.breakpoints.as_slice() {
                    // TODO: Proper printing of breakpoints
                    println!("{:?}", bp);
                }
            }
            c => {
                println!("Unhandled command {:?}", c)
            }
        }
    }

    fn process_debug_line(&mut self, line: &str) {
        match line.parse() as result::Result<Command, _> {
            Ok(command) => self.process_command(command),
            Err(err) => println!("TODO: Err handle match {}", err)
        }
    }

    pub fn get_summary(&mut self) -> Result<SystemDebugSummary> {
        match self.send(DebugRequest::GetSummary) {
            Ok(DebugResponse::Summary(summary)) => Ok(summary),
            Ok(_) => Err(Error::UnexpectedResponse),
            Err(err) => Err(err)
        }
    }

    pub fn pause(&mut self) -> Result<()> {
        self.message_tx.send(ControlMessage::Pause).map_err(|err| Error::Send(err))
    }
}


pub fn start(message_tx: Sender<ControlMessage>, debug_response_rx: Receiver<DebugResponse>) -> JoinHandle<()> {
    let handle = thread::spawn(move || {
        let mut debugger = Debugger::new(message_tx);

        let mut rl = Editor::<()>::new();
        if let Err(_) = rl.load_history("history.txt") {
            println!("No previous history.");
        }

        loop {
            let summary = debugger.get_summary().expect("Unable to get debug summary");

            let prompt = format!("(0x{:04x}) {} >", debugger.cursor, summary.cycles);
            if let Ok(line) = rl.readline(prompt.as_str()) {
                rl.add_history_entry(&line);
                debugger.process_debug_line(line.as_str())
            } else {
                break
            }
        }
        rl.save_history("history.txt").unwrap();
    });
    handle
}


