#![feature(proc_macro)]

extern crate rustyline;

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

use std::thread;
use std::thread::JoinHandle;
use std::sync::mpsc;
use std::sync::mpsc::{Receiver, Sender};

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
    Debug(debugger::DebugRequest)
}

fn start_gameboy(cpu: Cpu, message_rx: Receiver<ControlMessage>, debug_response_tx: Sender<debugger::DebugResponse>) -> GameboyThread {
    let (gfx_tx, gfx_rx) = mpsc::channel();
    let (snd_tx, snd_rx) = mpsc::channel();
    let mut paused = false;

    let handle = thread::spawn(move || {
        let mut cpu = cpu;
        let mut pending_cycles: i32 = 0;
        loop {
            let message = message_rx.recv().expect("Failed while receiving message");
            match message {
                ControlMessage::Tick(cycles) => {
                    pending_cycles += cycles;
                }
                ControlMessage::Joypad(buttons) => {
                    cpu.bus.io.joypad_all_buttons = buttons;
                }
                ControlMessage::Play => {
                    pending_cycles = 0;
                    paused = false;
                    snd_tx.send(SoundMessage::Play).unwrap();
                }
                ControlMessage::Pause =>  {
                    pending_cycles = 0;
                    paused = true;
                    snd_tx.send(SoundMessage::Pause).unwrap();
                }
                ControlMessage::Debug(req) => debug_response_tx.send(req.invoke(&mut cpu)).unwrap(),
            }

            if paused {
                continue;
            }


            while pending_cycles > 0 {
                pending_cycles -= cpu.step().unwrap() as i32;
                // trace!("Cpu state {}: {:?} IME {} IE {:05b} {:05b}", cpu.cycles, cpu.regs, cpu.interrupts_enabled,
                //       cpu.bus.io.interrupt_enable, cpu.bus.io.interrupt_flags );;
            }

            snd_tx.send(SoundMessage::Buffer(cpu.bus.dequeue_samples()))
                .expect("Sound output channel closed");
            /*

            for s in cpu.bus.dequeue_samples() {
                let s = ((s as i8) as f32) * u8::max_value() as f32;
                snd_tx.send(s).unwrap();
            }*/
            
            match cpu.bus.lcd.try_get_buffer() {
                Some(buffer) => {
                    gfx_tx.send(buffer).expect("Graphics output channel closed");
                }
                _ => ()
            }
        }
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


    while window.is_open() && !window.is_key_down(Key::Escape) {

        // if window.is_key_pressed(Key::O, KeyRepeat::No) {
        // cpu.bus.dump_audio_buffer("audio.dump");
        // }


        let mut frame_received = false;
        while let Ok(source_buffer) = gfx_rx.try_recv() {
            if frame_received {
                trace!("Dropped frame");
            }
            for offset in 0..(LCD_WIDTH * LCD_HEIGHT) {
                buffer[offset] = palette[source_buffer[offset] as usize & 0x03];
            }
            frame_received = true;
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
}

fn main() {
    env_logger::init().unwrap();
    let cpu = Cpu::new(&env::args().nth(1).unwrap_or(String::from("rom.gb")));



    let (debug_response_tx, debug_response_rx) = mpsc::channel();
    let (message_tx, message_rx) = mpsc::channel();
    let gb_thread = start_gameboy(cpu, message_rx, debug_response_tx);

    let message_tx_joypad = message_tx.clone();
    let message_tx_debugger = message_tx.clone();

    let stream = audio_driver::init(gb_thread.snd_rx, message_tx)
        .expect("Unable to open audio out stream");

    let debugger_handle = debugger::start(message_tx_debugger, debug_response_rx);
    start_window_thread(message_tx_joypad, gb_thread.gfx_rx);

    debugger_handle.join().unwrap();

    // let mut buffer: Vec<u32> = vec![0; 160 * 144];


}
