extern crate rustyline;

#[macro_use]
extern crate nom;

extern crate bincode;

extern crate portaudio;
#[macro_use]
extern crate log;
extern crate env_logger;

extern crate cogboy_core;

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
use minifb::{Key, KeyRepeat, WindowOptions, Window, Scale};

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

#[derive(Clone,Copy,Debug)]
pub struct EventEntry {
    time: u64,
    joypad: u8
}

pub struct Gameboy {
    pub cpu: Cpu,
    passed_events: Vec<EventEntry>,
    pending_events: VecDeque<EventEntry>,
    limit_speed: bool,
    running: bool,
    message_rx: Receiver<ControlMessage>,
    snd_tx: Sender<SoundMessage>,
    gfx_tx: Sender<Vec<u8>>,
    target_cycles: u64,
    breakpoints: std::collections::HashSet<u16>,
    break_cycle: u64
}

impl Gameboy {
    pub fn pause(&mut self) {
        self.running = false;
        self.target_cycles = self.cpu.cycles;
    }

    pub fn play(&mut self) {
        self.running = true;
        self.target_cycles = self.cpu.cycles;
    }

    fn replay(&mut self) {
        self.cpu.reset();
        self.pending_events.clear();
        for ev in self.passed_events.iter() {
            self.pending_events.push_back(*ev);
        }   
        self.passed_events.clear();
        self.target_cycles = self.cpu.cycles;
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
                    if self.cpu.bus.io.joypad_all_buttons != buttons {
                        if self.pending_events.len() > 0 {
                            println!("Overriding pending events!");
                            self.pending_events.clear();
                        }
                        self.pending_events.push_front(EventEntry { time: self.cpu.cycles, joypad: buttons });
                    }
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
                    self.cpu.bus.lcd.show_bg = !self.cpu.bus.lcd.show_bg;
                }
                ControlMessage::LcdToggleSprites => {
                    self.cpu.bus.lcd.show_sprites = !self.cpu.bus.lcd.show_sprites;
                }
                ControlMessage::LcdToggleWindow => {
                    self.cpu.bus.lcd.show_window = !self.cpu.bus.lcd.show_window;
                }
                ControlMessage::Quit => break
            }

            if self.running {
                let target_cycles =
                    if self.cpu.cycles > self.break_cycle {
                        self.target_cycles
                    } else {
                        cmp::min(self.target_cycles, self.break_cycle)
                    };

                while self.running && self.cpu.cycles <= target_cycles {
                    while let Some(ev) = self.pending_events.pop_front() {
                        if ev.time > self.cpu.cycles {
                            self.pending_events.push_front(ev);
                            break;
                        }
                        self.cpu.bus.io.joypad_all_buttons = ev.joypad;
                        self.passed_events.push(ev);
                    }

                    let next_event_cycle = cmp::min(target_cycles, self.pending_events.front().map(|ev| ev.time).unwrap_or(std::u64::MAX));

                    while self.cpu.cycles <= next_event_cycle {
                        let pre_cycles = self.cpu.cycles;
                        self.cpu.step().unwrap();
                        if pre_cycles < self.break_cycle && self.cpu.cycles >= self.break_cycle {
                            println!("At or passed break cycle {}, breaking at {}", self.break_cycle, self.cpu.cycles);
                            self.pause();
                            break;
                        }
                        if self.breakpoints.contains(&self.cpu.regs.pc) {
                            println!("At breakpoint!");
                            self.pause();
                            break;
                        }
                    }
                    // trace!("Cpu state {}: {:?} IME {} IE {:05b} {:05b}", cpu.cycles, cpu.regs, cpu.interrupts_enabled,
                    //       cpu.bus.io.interrupt_enable, cpu.bus.io.interrupt_flags );;
                }
                let samples = self.cpu.bus.dequeue_samples();

                if self.limit_speed {
                    self.snd_tx.send(SoundMessage::Buffer(samples)).expect("Sound output channel closed");
                }
            }


            /*

            for s in cpu.bus.dequeue_samples() {
                let s = ((s as i8) as f32) * u8::max_value() as f32;
                snd_tx.send(s).unwrap();
            }*/
            
            match self.cpu.bus.lcd.try_get_buffer() {
                Some(buffer) => {
                    let w = LCD_WIDTH + 256;
                    let h = 256;

                    let bg = self.cpu.bus.lcd.get_full_bg();
                    let mut b = Vec::with_capacity(w * h);
                    for y in 0..h {
                        for x in 0..w {
                            let c =
                                if x < LCD_WIDTH {
                                    if y < LCD_HEIGHT {
                                        buffer[y * LCD_WIDTH + x]
                                    } else {
                                        0
                                    }
                                } else {
                                    bg[y * 256 + (x - LCD_WIDTH)]
                                };
                            b.push(c);
                        }
                    }
                    self.gfx_tx.send(b).expect("Graphics output channel closed");
                }
                _ => ()
            }
        }
    }
}

fn start_gameboy(cpu: Cpu, message_rx: Receiver<ControlMessage>) -> GameboyThread {
    let (gfx_tx, gfx_rx) = mpsc::channel();
    let (snd_tx, snd_rx) = mpsc::channel();

    let handle = thread::spawn(move || {
        let mut gameboy = Gameboy {
            cpu: cpu,
            passed_events: Vec::new(),
            pending_events: VecDeque::new(),
            running: true,
            limit_speed: true,
            message_rx: message_rx,
            snd_tx: snd_tx,
            gfx_tx: gfx_tx,
            target_cycles: 0,
            breakpoints: HashSet::new(),
            break_cycle: std::u64::MAX
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
    let width = 160 + 256;
    let height = 256;
    let mut window = Window::new("Test - ESC to exit",
                                 width,
                                 height, //144,
                                 WindowOptions {
                                     borderless: false,
                                     title: true,
                                     resize: true,
                                     scale: Scale::X2,
                                 })
        .unwrap_or_else(|e| {
            panic!("{}", e);
        });

    let palette: [u32; 4] = [0xff9cbd0f, 0xff8cad0f, 0xff306230, 0xff0f380f];
    let mut buffer = vec![0u32; width * height];

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
                    for offset in 0..(width * height) {
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

        window.update_with_buffer(&buffer).unwrap();

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
    env_logger::init().unwrap();
    let cpu = Cpu::new(&env::args().nth(1).unwrap_or(String::from("rom.gb")));

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
