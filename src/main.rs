extern crate rustyline;

#[macro_use]
extern crate nom;

#[macro_use]
extern crate serde_derive;

extern crate serde_json;

extern crate portaudio;
#[macro_use]
extern crate enum_primitive;
extern crate num;
#[macro_use]
extern crate log;
extern crate env_logger;


mod gb;
use gb::cpu::Cpu;

mod audio_driver;
mod debugger;

mod command;

use std::thread;
use std::thread::JoinHandle;
use std::sync::mpsc;
use std::sync::mpsc::{Receiver, Sender};

use std::collections::HashSet;

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
    Play,
    Pause,
    Debug(debugger::DebugRequest, Sender<debugger::DebugResponse>),
    Quit
}

pub struct Gameboy {
    pub cpu: Cpu,
    running: bool,
    message_rx: Receiver<ControlMessage>,
    snd_tx: Sender<SoundMessage>,
    gfx_tx: Sender<Vec<u8>>,
    pending_cycles: i32,
    breakpoints: std::collections::HashSet<u16>
}

impl Gameboy {
    pub fn pause(&mut self) {
        self.running = false;
        self.pending_cycles = 0;
    }

    pub fn play(&mut self) {
        self.running = true;
        self.pending_cycles = 0;
    }

    fn event_loop(&mut self) {
        loop {
            let message = self.message_rx.recv().expect("Failed while receiving message");
            match message {
                ControlMessage::Tick(cycles) => {
                    self.pending_cycles += cycles;
                }
                ControlMessage::Joypad(buttons) => {
                    self.cpu.bus.io.joypad_all_buttons = buttons;
                }
                ControlMessage::Play => {
                    self.play();
                }
                ControlMessage::Pause =>  {
                    self.pause();
                }
                ControlMessage::Debug(req, response_sender) => {
                    let resp = req.invoke(self);
                    response_sender.send(resp).expect("Debug channel closed");
                }
                ControlMessage::Quit => break
            }

            if self.running {
                while self.pending_cycles > 0 {
                    let ticks = self.cpu.step().unwrap() as i32;
                    self.pending_cycles -= ticks;
                    if self.breakpoints.contains(&self.cpu.regs.pc) {
                        println!("At breakpoint!");
                        self.pause();
                    }
                    // trace!("Cpu state {}: {:?} IME {} IE {:05b} {:05b}", cpu.cycles, cpu.regs, cpu.interrupts_enabled,
                    //       cpu.bus.io.interrupt_enable, cpu.bus.io.interrupt_flags );;
                }
                let samples = self.cpu.bus.dequeue_samples();
                self.snd_tx.send(SoundMessage::Buffer(samples)).expect("Sound output channel closed");
            }


            /*

            for s in cpu.bus.dequeue_samples() {
                let s = ((s as i8) as f32) * u8::max_value() as f32;
                snd_tx.send(s).unwrap();
            }*/
            
            match self.cpu.bus.lcd.try_get_buffer() {
                Some(buffer) => {
                    self.gfx_tx.send(buffer).expect("Graphics output channel closed");
                }
                _ => ()
            }
        }
    }
}

fn start_gameboy(cpu: Cpu, message_rx: Receiver<ControlMessage>, debug_response_tx: Sender<debugger::DebugResponse>) -> GameboyThread {
    let (gfx_tx, gfx_rx) = mpsc::channel();
    let (snd_tx, snd_rx) = mpsc::channel();
    let mut paused = false;

    let handle = thread::spawn(move || {
        let mut gameboy = Gameboy {
            cpu: cpu,
            running: true,
            message_rx: message_rx,
            snd_tx: snd_tx,
            gfx_tx: gfx_tx,
            pending_cycles: 0,
            breakpoints: HashSet::new()
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
                                     scale: Scale::X4,
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

        window.update_with_buffer(&buffer);

        if window.is_key_pressed(Key::P, KeyRepeat::No) {
            message_tx.send(if paused { ControlMessage::Play } else { ControlMessage::Pause }).unwrap();
            paused = !paused;
            continue;
        }


        let buttons = map_buttons(&mut window);
        message_tx.send(ControlMessage::Joypad(buttons)).unwrap();
    }

    println!("Exiting window message loop");
}

fn main() {
    env_logger::init().unwrap();
    let cpu = Cpu::new(&env::args().nth(1).unwrap_or(String::from("rom.gb")));



    let (debug_response_tx, debug_response_rx) = mpsc::channel();
    let (message_tx, message_rx) = mpsc::channel();
    let gb_thread = start_gameboy(cpu, message_rx, debug_response_tx);

    let message_tx_joypad = message_tx.clone();
    let message_tx_debugger = message_tx.clone();
    let message_tx_audio = message_tx.clone();

    let mut stream = audio_driver::init(gb_thread.snd_rx, message_tx_audio)
        .expect("Unable to open audio out stream");

    let debugger_handle = debugger::start(message_tx_debugger, debug_response_rx);
    start_window_thread(message_tx_joypad, gb_thread.gfx_rx);

    message_tx.send(ControlMessage::Quit).unwrap();

    stream.close().unwrap();
    println!("Closed audio stream");

    debugger_handle.join().unwrap();

    // let mut buffer: Vec<u32> = vec![0; 160 * 144];


}
