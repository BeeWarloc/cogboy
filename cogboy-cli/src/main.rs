extern crate rustyline;

#[macro_use]
extern crate nom;

extern crate bincode;

extern crate portaudio;
#[macro_use]
extern crate log;
extern crate env_logger;

extern crate cogboy_core;

use cogboy_core::System;
use cogboy_core::RunContext;
use cogboy_core::cpu::Cpu;

mod audio_driver;
mod debugger;

mod command;

use std::thread;
use std::thread::JoinHandle;
use std::sync::mpsc;
use std::sync::mpsc::{Receiver, Sender};

use std::collections::HashSet;
use std::collections::VecDeque;

use std::cmp;
use std::env;
extern crate minifb;
use minifb::{Key, KeyRepeat, Scale, ScaleMode, Window, WindowOptions};

const LCD_WIDTH: usize = 160;
const LCD_HEIGHT: usize = 144;


struct GameboyThread {
    gfx_rx: Receiver<Vec<u8>>,
    snd_rx: Receiver<SoundMessage>,
    handle: JoinHandle<()>,
}

pub enum SoundMessage {
    Play,
    Pause,
    Buffer(Vec<(i8,i8)>)
}

pub enum ControlMessage {
    Tick(i32),
    Joypad(u8),
    ToggleSpeedLimit,
    Play,
    Pause,
    Replay,
    Debug(debugger::DebugRequest, Sender<debugger::DebugResponse>),
    LcdToggleBackground,
    LcdToggleSprites,
    LcdToggleWindow,
    Quit
}

pub struct Gameboy {
    system: System,
    limit_speed: bool,
    running: bool,
    message_rx: Receiver<ControlMessage>,
    snd_tx: Sender<SoundMessage>,
    gfx_tx: Sender<Vec<u8>>,
    target_cycles: u64,
    breakpoints: HashSet<u16>,
    watchpoints: HashSet<u16>,
    break_cycle: u64,
}

struct GameboyRunContext<'a> {
    watchpoint_triggered: Option<(u16, u8)>,
    breakpoints: &'a HashSet<u16>,
    watchpoints: &'a HashSet<u16>,
    break_cycle: u64,
}

impl<'a> RunContext for GameboyRunContext<'a> {
    #[inline]
    fn check_watchpoint(&mut self, addr: u16, value: u8) {
        if self.watchpoints.contains(&addr) {
            self.watchpoint_triggered = (addr, value).into()
        }
    }

    #[inline]
    fn after_step(&mut self, cpu: &Cpu, passed_cycles: usize) -> bool {
        let pre_cycles = cpu.cycles - passed_cycles as u64;
        if pre_cycles < self.break_cycle && cpu.cycles >= self.break_cycle {
            println!("At or passed break cycle {}, breaking at {}", self.break_cycle, cpu.cycles);
            return true
        }
        if self.breakpoints.contains(&cpu.regs.pc) {
            println!("At breakpoint!");
            return true
        }
        if let Some((watchpoint_addr, value)) = self.watchpoint_triggered {
            println!("Watchpoint hit: {:02x} written to {:04x}, breaking at cycle {}", value, watchpoint_addr, cpu.cycles);
            self.watchpoint_triggered = None;
            return true;
        }

        return false;
    }
}

impl Gameboy {
    pub fn pause(&mut self) {
        self.running = false;
        self.target_cycles = self.system.cpu.cycles;
    }

    pub fn play(&mut self) {
        self.running = true;
        self.target_cycles = self.system.cpu.cycles;
    }

    fn replay(&mut self) {
        self.system.rewind_to_closest_snapshot(0);
        self.target_cycles = self.system.cpu.cycles;
    }

    fn event_loop(&mut self) {
        loop {
            let message =
                if self.limit_speed {
                    self.message_rx.recv().expect("Failed while receiving message")
                } else {
                    self.message_rx.try_recv().unwrap_or(ControlMessage::Tick(9000))
                };

            match message {
                ControlMessage::Tick(cycles) => {
                    self.target_cycles += cycles as u64;
                }
                ControlMessage::Joypad(buttons) => {
                    self.system.update_joypad(buttons)
                }
                ControlMessage::ToggleSpeedLimit => {
                    self.limit_speed = !self.limit_speed;
                }
                ControlMessage::Play => {
                    self.play();
                }
                ControlMessage::Pause => {
                    self.pause();
                }
                ControlMessage::Replay => {
                    self.replay();
                }
                ControlMessage::Debug(req, response_sender) => {
                    let resp = req.invoke(self);
                    response_sender.send(resp).expect("Debug channel closed");
                }
                ControlMessage::LcdToggleBackground => {
                    self.system.cpu.bus.lcd.show_bg = !self.system.cpu.bus.lcd.show_bg;
                }
                ControlMessage::LcdToggleSprites => {
                    self.system.cpu.bus.lcd.show_sprites = !self.system.cpu.bus.lcd.show_sprites;
                }
                ControlMessage::LcdToggleWindow => {
                    self.system.cpu.bus.lcd.show_window = !self.system.cpu.bus.lcd.show_window;
                }
                ControlMessage::Quit => break
            }

            if self.running {
                let target_cycles =
                    if self.system.cpu.cycles > self.break_cycle {
                        self.target_cycles
                    } else {
                        cmp::min(self.target_cycles, self.break_cycle)
                    };

                let mut ctx = GameboyRunContext {
                    watchpoints: &self.watchpoints,
                    breakpoints: &self.breakpoints,
                    watchpoint_triggered: None,
                    break_cycle: self.break_cycle,
                };

                // Never rewind here
                if target_cycles > self.system.cpu.cycles {
                    self.system.run_to_cycle(target_cycles, &mut ctx);
                    let samples = self.system.cpu.bus.dequeue_samples();

                    if self.limit_speed {
                        self.snd_tx.send(SoundMessage::Buffer(samples)).expect("Sound output channel closed");
                    }
                }
            }


            /*

            for s in cpu.bus.dequeue_samples() {
                let s = ((s as i8) as f32) * u8::max_value() as f32;
                snd_tx.send(s).unwrap();
            }*/
            
            match self.system.cpu.bus.lcd.try_get_buffer() {
                Some(buffer) => {
                    // Snapshot here periodically for back stepping
                    if self.system.cycles_since_last_snapshot() > cogboy_core::constants::CPU_FREQ as u64 {
                        self.system.take_snapshot();
                        println!("NOCOMMIT!!!! SNAPSHOT SAVED!!! total size of snapshot store is {}KiB", (self.system.snapshots_size() as f64) / 1024.0);
                    }
                    //self.history.push(self.cpu.clone().into());
                    self.gfx_tx.send(buffer).expect("Graphics output channel closed");
                }
                _ => ()
            }
        }
    }
}

fn start_gameboy(cpu: Box<Cpu>, message_rx: Receiver<ControlMessage>) -> GameboyThread {
    let (gfx_tx, gfx_rx) = mpsc::channel();
    let (snd_tx, snd_rx) = mpsc::channel();

    let handle = thread::spawn(move || {
        let mut gameboy = Gameboy {
            system: System {
                cpu,
                passed_events: Vec::new(),
                pending_events: VecDeque::new(),
                snapshots: Vec::new(),
            },
            running: true,
            limit_speed: true,
            message_rx: message_rx,
            snd_tx: snd_tx,
            gfx_tx: gfx_tx,
            target_cycles: 0,
            breakpoints: HashSet::new(),
            watchpoints: HashSet::new(),
            break_cycle: std::u64::MAX,
        };
        gameboy.event_loop();
    });

    GameboyThread {
        gfx_rx: gfx_rx,
        handle: handle,
        snd_rx: snd_rx,
    }
}

fn map_buttons(window: &mut Window) -> u8 {
    let buttons = if window.is_key_down(Key::Right) {
        0
    } else {
        1 << 0
    } |
    if window.is_key_down(Key::Left) {
        0
    } else {
        1 << 1
    } |
    if window.is_key_down(Key::Up) {
        0
    } else {
        1 << 2
    } |
    if window.is_key_down(Key::Down) {
        0
    } else {
        1 << 3
    } |
    if window.is_key_down(Key::S) {
        0
    } else {
        1 << 4
    } |
    if window.is_key_down(Key::A) {
        0
    } else {
        1 << 5
    } |
    if window.is_key_down(Key::Z) {
        0
    } else {
        1 << 6
    } |
    if window.is_key_down(Key::Enter) {
        0
    } else {
        1 << 7
    };
    buttons
}

fn start_window_thread(message_tx: Sender<ControlMessage>, gfx_rx: Receiver<Vec<u8>>) {
    let mut window = Window::new("Test - ESC to exit",
                                 160,
                                 144,
                                 WindowOptions {
                                     borderless: false,
                                     title: true,
                                     resize: true,
                                     scale: Scale::X1,
                                     scale_mode: ScaleMode::Stretch,
                                     topmost: true,
                                     transparency: false,
                                     none: false,
                                 })
        .unwrap_or_else(|e| {
            panic!("{}", e);
        });

    let palette: [u32; 4] = [0xff9cbd0f, 0xff8cad0f, 0xff306230, 0xff0f380f];
    let mut buffer = vec![0u32; LCD_WIDTH * LCD_HEIGHT];

    // let clock_thread_handle = thread::spawn(move || {
    // loop {
    // clock_tx.send((1 << 22) * 2 / 60).unwrap();
    // std::thread::sleep_ms(1000 / 60);
    // }
    // });
    //
    let mut paused = true;
    let mut last_buttons = 0xffu8;

    'outer: while window.is_open() && !window.is_key_down(Key::Escape) {

        let mut frame_received = false;
        loop {
            match gfx_rx.try_recv() {
                Ok(source_buffer) => {
                    if frame_received {
                        trace!("Dropped frame");
                    }
                    for offset in 0..(LCD_WIDTH * LCD_HEIGHT) {
                        buffer[offset] = palette[source_buffer[offset] as usize & 0x03];
                    }
                    frame_received = true;
                }
                Err(mpsc::TryRecvError::Disconnected) =>
                {
                    break 'outer;
                }
                _ => break
            }
        }

        window.update_with_buffer(&buffer, LCD_WIDTH, LCD_HEIGHT).unwrap();

        if window.is_key_pressed(Key::P, KeyRepeat::No) {
            message_tx.send(if paused { ControlMessage::Play } else { ControlMessage::Pause }).unwrap();
            paused = !paused;
            continue;
        }

        if window.is_key_pressed(Key::R, KeyRepeat::No) {
            message_tx.send(ControlMessage::Replay).unwrap();
        }

        if window.is_key_pressed(Key::Q, KeyRepeat::No) {
            message_tx.send(ControlMessage::ToggleSpeedLimit).unwrap();
        }

        if window.is_key_pressed(Key::Key8, KeyRepeat::No) {
            message_tx.send(ControlMessage::LcdToggleBackground).unwrap();
        }

        if window.is_key_pressed(Key::Key9, KeyRepeat::No) {
            message_tx.send(ControlMessage::LcdToggleSprites).unwrap();
        }

        if window.is_key_pressed(Key::Key0, KeyRepeat::No) {
            message_tx.send(ControlMessage::LcdToggleWindow).unwrap();
        }

        let buttons = map_buttons(&mut window);
        if last_buttons != buttons {
            message_tx.send(ControlMessage::Joypad(buttons)).unwrap();
            last_buttons = buttons;
        }
    }

    println!("Exiting window message loop");
}

fn main() {
    env_logger::init();
    let cpu = Cpu::new(&env::args().nth(1).unwrap_or(String::from("rom.gb"))).into();

    println!("sizeof(Cpu) is {}", std::mem::size_of::<Cpu>());

    let (message_tx, message_rx) = mpsc::channel();
    let gb_thread = start_gameboy(cpu, message_rx);

    let message_tx_joypad = message_tx.clone();
    let message_tx_debugger = message_tx.clone();
    let message_tx_audio = message_tx.clone();

    let mut stream = audio_driver::init(gb_thread.snd_rx, message_tx_audio)
        .expect("Unable to open audio out stream");

    let debugger_handle = debugger::start(message_tx_debugger);
    start_window_thread(message_tx_joypad, gb_thread.gfx_rx);

    message_tx.send(ControlMessage::Quit).unwrap();

    stream.close().unwrap();
    println!("Closed audio stream");

    debugger_handle.join().unwrap();
    gb_thread.handle.join().unwrap();

    // let mut buffer: Vec<u32> = vec![0; 160 * 144];
}
