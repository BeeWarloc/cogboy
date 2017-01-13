use std::result;
use std::thread;
use std::sync::mpsc::{Receiver, Sender, SendError};

use std::fmt::Write;

use std::thread::JoinHandle;
use super::ControlMessage;
use gb::cpu;
use gb::cpu::Cpu;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use serde_json;

#[derive(Debug)]
pub enum DebugResponse {
    Regs(cpu::Regs),
    Disassembly(String),
    CpuSnapshot(Cpu),
}

pub enum DebugRequest {
    GetRegisters,
    GetDisassembly,
    GetCpuSnapshot
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

    pub fn invoke(&self, cpu: &mut Cpu) -> DebugResponse {
        match *self {
            DebugRequest::GetRegisters => DebugResponse::Regs(cpu.regs.clone()),
            DebugRequest::GetDisassembly => DebugResponse::Disassembly(DebugRequest::disassemble(cpu, cpu.regs.pc, 10)),
            DebugRequest::GetCpuSnapshot => DebugResponse::CpuSnapshot(cpu.clone())
        }
    }
}

#[derive(Debug)]
enum Error {
    Send(SendError<ControlMessage>),
    UnexpectedResponse,
}

type Result<T> = result::Result<T, Error>;

struct Debugger {
    message_tx: Sender<ControlMessage>,
    response_rx: Receiver<DebugResponse>
}

impl Debugger {
    pub fn new(message_tx: Sender<ControlMessage>, response_rx: Receiver<DebugResponse>) -> Debugger
    {
        Debugger {
            message_tx: message_tx,
            response_rx: response_rx
        }
    }

    pub fn send(&mut self, req: DebugRequest) -> Result<DebugResponse> {
        self.message_tx.send(ControlMessage::Debug(req)).map_err(|err| Error::Send(err));
        match self.response_rx.recv() {
            Ok(response) => Ok(response),
            _ => unimplemented!()
        }
    }

    pub fn pause(&mut self) -> Result<()> {
        self.message_tx.send(ControlMessage::Pause).map_err(|err| Error::Send(err))
    }
}


pub fn start(message_tx: Sender<ControlMessage>, debug_response_rx: Receiver<DebugResponse>) -> JoinHandle<()> {
    let handle = thread::spawn(move || {
        let mut debugger = Debugger::new(message_tx, debug_response_rx);

        let mut rl = Editor::<()>::new();
        if let Err(_) = rl.load_history("history.txt") {
            println!("No previous history.");
        }
        loop {
            let readline = rl.readline(">> ");
            match readline {
                Ok(line) => {
                    let line = line.trim();
                    rl.add_history_entry(&line);
                    println!("Line: {}", line);
                    match &line as &str {
                        "pause" => {
                            debugger.pause();
                        }
                        "r" => {
                            let response = debugger.send(DebugRequest::GetRegisters).unwrap();
                            if let DebugResponse::Regs(r) = response {
                                println!("Regs are {:?}", r);
                            }
                        }
                        "d" => {
                            let response = debugger.send(DebugRequest::GetDisassembly).unwrap();
                            if let DebugResponse::Disassembly(s) = response {
                                println!("{}", s);
                            }
                        }
                        "snap" => {
                            let response = debugger.send(DebugRequest::GetCpuSnapshot).unwrap();
                            if let DebugResponse::CpuSnapshot(cpu) = response {
                                println!("Json serialized: \n{}", serde_json::to_string(&cpu).unwrap());
                            }
                        }
                        _ => println!("Unrecognized command {}", line)
                    };
                },
                Err(ReadlineError::Interrupted) => {
                    println!("CTRL-C");
                    break
                },
                Err(ReadlineError::Eof) => {
                    println!("CTRL-D");
                    break
                },
                Err(err) => {
                    println!("Error: {:?}", err);
                    break
                }
            }
        }
        rl.save_history("history.txt").unwrap();
    });
    handle
}


