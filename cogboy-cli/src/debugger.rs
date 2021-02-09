use crate::GameboyRunContext;
use std;
use std::collections::HashSet;
use std::result;
use std::sync::mpsc;
use std::sync::mpsc::{Receiver, RecvError, SendError, Sender};
use std::thread;
use std::u64;

use std::fmt::Write;

use std::fs::File;
use std::io::Write as IoWrite;

use super::command::{Breakpoint, Command};
use super::ControlMessage;
use super::Gameboy;
use cogboy_core::cpu;
use cogboy_core::cpu::Cpu;
use std::thread::JoinHandle;

use rustyline::Editor;

use bincode::{deserialize_from, serialize};
use std::io::BufReader;

#[derive(Debug)]
pub struct SystemDebugSummary {
    regs: cpu::Regs,
    cycles: u64,
    instruction_counter: u64,
    break_cycle: u64,
    rom_bank: u8,
}

impl SystemDebugSummary {
    pub fn new(gb: &Gameboy) -> SystemDebugSummary {
        SystemDebugSummary {
            regs: gb.system.cpu.regs.clone(),
            cycles: gb.system.cpu.cycles,
            instruction_counter: gb.system.cpu.instruction_counter,
            rom_bank: gb.system.cpu.bus.cartridge.rom_bank,
            break_cycle: gb.break_cycle,
        }
    }
}

#[derive(Debug)]
pub enum DebugResponse {
    Summary(SystemDebugSummary),
    Disassembly(String),
    CpuSnapshot(Box<Cpu>),
    Step(u16),
    Breakpoints(Vec<Breakpoint>),
    Watchpoints(Vec<Breakpoint>),
    ReadMem(u16, Vec<u8>),
}

pub enum DebugRequest {
    GetSummary,
    GetDisassembly(Option<u16>, Option<u16>),
    GetCpuSnapshot,
    SetCpuSnapshot(Box<Cpu>),
    Step,
    RevStep,
    Continue,
    SetBreakpoints(Vec<Breakpoint>),
    ListBreakpoints,
    ReadMem(u16, u16),
}

impl DebugRequest {
    fn disassemble(cpu: &Cpu, base: u16, len: u16) -> String {
        let mut offset = 0;
        let mut output = String::with_capacity(256);
        while offset < len {
            let addr = base.wrapping_add(offset);
            if let Ok((inst, _)) = cpu::Instruction::decode(addr, &cpu.bus) {
                writeln!(output, "0x{:04x}: {}", addr, inst).unwrap();
                offset += inst.length() as u16;
            } else {
                writeln!(output, "0x{:04x}: Unrecognized opcode", addr).unwrap();
                offset += 1;
            }
        }

        output
    }

    fn get_breakpoints(gameboy: &Gameboy) -> Vec<Breakpoint> {
        gameboy
            .breakpoints
            .iter()
            .map(|x| Breakpoint::Code(*x))
            .chain(gameboy.watchpoints.iter().map(|x| Breakpoint::Watch(*x)))
            .collect()
    }

    pub fn invoke(&self, gameboy: &mut Gameboy) -> DebugResponse {
        match *self {
            DebugRequest::GetSummary => DebugResponse::Summary(SystemDebugSummary::new(gameboy)),
            DebugRequest::GetDisassembly(addr, len) => {
                let cpu = &gameboy.system.cpu;
                DebugResponse::Disassembly(DebugRequest::disassemble(
                    cpu,
                    addr.unwrap_or(cpu.regs.pc),
                    len.unwrap_or(10),
                ))
            }
            DebugRequest::GetCpuSnapshot => DebugResponse::CpuSnapshot(gameboy.system.cpu.clone()),
            DebugRequest::SetCpuSnapshot(ref cpu) => {
                gameboy.system.cpu.clone_from(cpu);
                gameboy.target_cycles = gameboy.system.cpu.cycles;
                while let Some(ev) = gameboy.system.passed_events.pop() {
                    if ev.time > gameboy.system.cpu.cycles {
                        gameboy.system.pending_events.push_front(ev);
                    } else {
                        gameboy.system.passed_events.push(ev);
                        break;
                    }
                }
                DebugResponse::Summary(SystemDebugSummary::new(gameboy))
            }
            DebugRequest::RevStep => {
                // TODO: Better error handling
                if gameboy.running {
                    println!("Pausing!");
                    gameboy.pause();
                } else {
                    println!("Reverse stepping!");
                    let mut ctx = GameboyRunContext {
                        break_cycle: u64::MAX,
                        breakpoints: &HashSet::new(),
                        watchpoints: &HashSet::new(),
                        watchpoint_triggered: None,
                    };
                    let target_inst_count =
                        gameboy.system.cpu.instruction_counter.saturating_sub(1);

                    // First rewind to a bit before instruction count
                    let some_cycles_before_prev_instruction =
                        gameboy.system.cpu.cycles.saturating_sub(32);
                    gameboy
                        .system
                        .run_to_cycle(some_cycles_before_prev_instruction, &mut ctx);

                    // Now step forward until our instruction counter hits prev_inst_count
                    while gameboy.system.cpu.instruction_counter < target_inst_count {
                        // TODO: STEP EXACTLY TO INSTRUCTION COUNT - 1
                        let target_cycles = gameboy.system.cpu.cycles + 1;
                        gameboy.system.run_to_cycle(target_cycles, &mut ctx);
                    }
                }
                DebugResponse::Step(gameboy.system.cpu.regs.pc)
            }
            DebugRequest::Step => {
                // TODO: Better error handling
                if gameboy.running {
                    println!("Pausing!");
                    gameboy.pause();
                } else {
                    println!("Stepping!");
                    let mut ctx = GameboyRunContext {
                        break_cycle: u64::MAX,
                        breakpoints: &HashSet::new(),
                        watchpoints: &HashSet::new(),
                        watchpoint_triggered: None,
                    };
                    let start_inst_count = gameboy.system.cpu.instruction_counter;
                    while start_inst_count == gameboy.system.cpu.instruction_counter {
                        let target_cycles = gameboy.system.cpu.cycles + 1;
                        gameboy.system.run_to_cycle(target_cycles, &mut ctx);
                    }
                }
                DebugResponse::Step(gameboy.system.cpu.regs.pc)
            }
            DebugRequest::Continue => {
                gameboy.play();
                DebugResponse::Summary(SystemDebugSummary::new(gameboy))
            }
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
                        Breakpoint::Watch(addr) => {
                            gameboy.watchpoints.insert(addr);
                        }
                    }
                }
                DebugResponse::Breakpoints(Self::get_breakpoints(gameboy))
            }
            DebugRequest::ListBreakpoints => {
                DebugResponse::Breakpoints(Self::get_breakpoints(gameboy))
            }
            DebugRequest::ReadMem(addr, len) => {
                let mut vec = Vec::new();
                for i in addr..addr.saturating_add(len) {
                    vec.push(gameboy.system.cpu.bus.read(i));
                }
                DebugResponse::ReadMem(addr, vec)
            }
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
    snapshot: Option<Box<Cpu>>,
    repeated_command: Option<Command>,
}

impl Debugger {
    pub fn new(message_tx: Sender<ControlMessage>) -> Debugger {
        let (response_tx, response_rx) = mpsc::channel();
        let snapshot: Option<Box<Cpu>> = File::open("gameboy.state")
            .ok()
            .map(|f| deserialize_from(&mut BufReader::new(f)).ok())
            .unwrap_or(None);
        Debugger {
            message_tx: message_tx,
            response_rx: response_rx,
            response_tx: response_tx,
            breakpoints: Vec::new(),
            cursor: 0x100,
            snapshot: snapshot,
            repeated_command: None,
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
                    if let Ok(serialized) = serialize(&cpu) {
                        f.write(serialized.as_slice())
                            .expect("Unable to write cpu snapshot");
                    }
                }
                self.snapshot = Some(cpu);
            }
            DebugResponse::Breakpoints(ref bps) | DebugResponse::Watchpoints(ref bps) => {
                for bp in bps {
                    match bp {
                        Breakpoint::Code(addr) => {
                            println!("{:04x}", addr);
                        }
                        Breakpoint::Watch(addr) => {
                            println!("w{:04x}", addr);
                        }
                        Breakpoint::Cycle(cycle) => {
                            println!("@{}", cycle);
                        }
                    }
                }
            }
            DebugResponse::ReadMem(mut addr, ref data) => {
                for x in data.chunks(16) {
                    print!("0x{:04x}:", addr);
                    for b in x {
                        print!(" {:02x}", b);
                    }
                    println!("");
                    addr += x.len() as u16;
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
            }
            Err(error) => Err(error),
        }
    }

    fn process_command(&mut self, command: Command) {
        let command = match (command, self.repeated_command.clone()) {
            (Command::Repeat, Some(repeated_command)) => repeated_command,
            (c, _) => c,
        };
        self.repeated_command = None;
        match command {
            Command::Exit => {
                self.message_tx.send(ControlMessage::Quit).unwrap();
                panic!("TODO: Clean exit")
            }
            Command::Save => self.send_and_handle(DebugRequest::GetCpuSnapshot).unwrap(),
            Command::Load => {
                let maybe_snap = self.snapshot.clone();
                if let Some(cpu_snapshot) = maybe_snap {
                    self.send_and_handle(DebugRequest::SetCpuSnapshot(cpu_snapshot))
                        .unwrap()
                } else {
                    println!("No snapshot to load");
                }
            }
            Command::ShowRegs => {
                self.repeated_command = Some(command.clone());
                self.send_and_handle(DebugRequest::GetSummary).unwrap()
            }
            Command::Disassemble(count) => {
                let cursor = self.cursor;
                self.send_and_handle(DebugRequest::GetDisassembly(
                    Some(cursor),
                    Some(count.unwrap_or(10) as u16),
                ))
                .unwrap()
            }
            Command::Goto(addr) => {
                self.cursor = addr as u16;
            }
            Command::Step => {
                self.repeated_command = Some(command.clone());
                self.send_and_handle(DebugRequest::Step).unwrap();
                let cursor = self.cursor;
                self.send_and_handle(DebugRequest::GetDisassembly(Some(cursor), Some(10 as u16)))
                    .unwrap()
            }
            Command::RevStep => {
                self.repeated_command = Some(command.clone());
                self.send_and_handle(DebugRequest::RevStep).unwrap();
                let cursor = self.cursor;
                self.send_and_handle(DebugRequest::GetDisassembly(Some(cursor), Some(10 as u16)))
                    .unwrap()
            }
            Command::Continue => {
                self.repeated_command = Some(command.clone());
                self.send_and_handle(DebugRequest::Continue).unwrap();
            }
            Command::AddBreakpoint(bp) => {
                self.breakpoints.push(bp);
                let breakpoints_cloned = self.breakpoints.clone();
                self.send_and_handle(DebugRequest::SetBreakpoints(breakpoints_cloned))
                    .unwrap();
            }
            // TODO: Proper printing of breakpoints
            Command::ListBreakpoints | Command::ListWatchpoints => {
                for bp in self.breakpoints.as_slice() {
                    println!("{:?}", bp);
                }
            }
            Command::ShowMem(addr) => {
                let cursor = self.cursor as u32;
                self.send_and_handle(DebugRequest::ReadMem(addr.unwrap_or(cursor) as u16, 0x80))
                    .unwrap()
            }
            Command::Repeat => {
                // Do nothing, as there's nothing to repeat
            }
            c => {
                println!("Unhandled command {:?}", c)
            }
        }
    }

    fn process_debug_line(&mut self, line: &str) {
        match line.parse() as result::Result<Command, _> {
            Ok(command) => self.process_command(command),
            Err(err) => println!("TODO: Err handle match {}", err),
        }
    }

    pub fn get_summary(&mut self) -> Result<SystemDebugSummary> {
        match self.send(DebugRequest::GetSummary) {
            Ok(DebugResponse::Summary(summary)) => Ok(summary),
            Ok(_) => Err(Error::UnexpectedResponse),
            Err(err) => Err(err),
        }
    }
}

pub fn start(message_tx: Sender<ControlMessage>) -> JoinHandle<()> {
    let handle = thread::spawn(move || {
        let mut debugger = Debugger::new(message_tx);

        let mut rl = Editor::<()>::new();
        if let Err(_) = rl.load_history("history.txt") {
            println!("No previous history.");
        }

        loop {
            let summary = debugger.get_summary().expect("Unable to get debug summary");

            let prompt = format!(
                "(0x{:04x}) {} {} >",
                debugger.cursor, summary.cycles, summary.instruction_counter
            );
            if let Ok(line) = rl.readline(prompt.as_str()) {
                rl.add_history_entry(&line);
                debugger.process_debug_line(line.as_str())
            } else {
                break;
            }
        }
        rl.save_history("history.txt").unwrap();
    });
    handle
}
