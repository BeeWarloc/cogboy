extern crate serde;

#[macro_use]
extern crate serde_derive;

#[macro_use]
extern crate log;

#[macro_use]
extern crate enum_primitive;
extern crate num;

use std::cmp;
use std::collections::VecDeque;

mod romfile;
pub mod cpu;
pub mod bus;
pub mod cartridge;
pub mod sound;
pub mod lcd;

use self::cpu::Cpu;

#[derive(Clone,Copy,Debug)]
pub struct EventEntry {
    time: u64,
    joypad: u8
}

#[derive(Clone)]
pub struct System {
    pub cpu: Box<Cpu>,
    passed_events: Vec<EventEntry>,
    pending_events: VecDeque<EventEntry>,
}

impl System {
    pub fn new(path: &str) -> System {
        System {
            cpu: Box::new(Cpu::new(path)),
            passed_events: Vec::new(),
            pending_events: VecDeque::new()
        }
    }

    pub fn drain_serial_out(&mut self) -> String {
        self.cpu.bus.io.serial_out.drain(..).map(|x| x as char).collect::<String>()
    }

    pub fn run_to_cycle(&mut self, target_cycles: u64) {
        if self.cpu.cycles > target_cycles {
            panic!("Rewinding not yet supported");
        }
        while self.cpu.cycles <= target_cycles {
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
                // let pre_cycles = self.cpu.cycles;

                self.cpu.step().unwrap();

                /*
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
                */
            }
            // trace!("Cpu state {}: {:?} IME {} IE {:05b} {:05b}", cpu.cycles, cpu.regs, cpu.interrupts_enabled,
            //       cpu.bus.io.interrupt_enable, cpu.bus.io.interrupt_flags );;
        }
    }
}

pub mod constants {
    pub const MEM_CARTRIDGE_ROM_START: u16 = 0x0000;
    pub const MEM_CARTRIDGE_ROM_END: u16 = 0x7fff;

    pub const MEM_CARTRIDGE_RAM_START: u16 = 0xa000;
    pub const MEM_CARTRIDGE_RAM_END: u16 = 0xbfff;

    pub const MEM_LCD_IO_START: u16 = 0xff40;
    pub const MEM_LCD_IO_LCDC: u16 = 0xff40;
    pub const MEM_LCD_IO_STAT: u16 = 0xff41;
    pub const MEM_LCD_IO_SCY: u16 = 0xff42;
    pub const MEM_LCD_IO_SCX: u16 = 0xff43;
    pub const MEM_LCD_IO_LY: u16 = 0xff44;
    pub const MEM_LCD_IO_LYC: u16 = 0xff45;
    pub const MEM_LCD_IO_DMA: u16 = 0xff46;
    pub const MEM_LCD_IO_BGP: u16 = 0xff47;
    pub const MEM_LCD_IO_OBP0: u16 = 0xff48;
    pub const MEM_LCD_IO_OBP1: u16 = 0xff49;
    pub const MEM_LCD_IO_WY: u16 = 0xff4a;
    pub const MEM_LCD_IO_WX: u16 = 0xff4b;
    pub const MEM_LCD_IO_END: u16 = 0xff4b;
    pub const MEM_LCD_VRAM_START: u16 = 0x8000;
    pub const MEM_LCD_VRAM_END: u16 = 0x9fff;
    pub const MEM_LCD_VRAM_TILE_DATA_START: u16 = 0x8000;
    pub const MEM_LCD_VRAM_TILE_DATA_END: u16 = 0x97ff;
    pub const MEM_LCD_VRAM_TILE_MAP_START: u16 = 0x9800;
    pub const MEM_LCD_VRAM_TILE_MAP_END: u16 = 0x9fff;
    pub const MEM_LCD_OAM_START: u16 = 0xfe00;
    pub const MEM_LCD_OAM_END: u16 = 0xfe9f;

    pub const MEM_IE: u16 = 0xffff;

    pub const CPU_FREQ: usize = 4_194_304;

    pub const LCD_WIDTH: usize = 160;
    pub const LCD_HEIGHT: usize = 144;

    pub const LCDC_BG_TILE_MAP_DISPLAY_SELECT: u8 = 0x08;
    pub const LCDC_WINDOW_DISPLAY_ENABLE: u8 = 0x20;
    pub const LCDC_WINDOW_TILE_MAP_DISPLAY_SELECT: u8 = 0x40;

    pub const INT_VBLANK: usize = 0;
    pub const INT_MASK_VBLANK: u8 = 1u8 << INT_VBLANK;
    pub const INT_LCDSTAT: usize = 1;
    pub const INT_MASK_LCDSTAT: u8 = 1u8 << INT_LCDSTAT;
    pub const INT_TIMER: usize = 2;
    pub const INT_MASK_TIMER: u8 = 1u8 << INT_TIMER;
    pub const INT_SERIAL: usize = 3;
    pub const INT_MASK_SERIAL: u8 = 1u8 << INT_SERIAL;
    pub const INT_JOYPAD: usize = 4;
    pub const INT_MASK_JOYPAD: u8 = 1u8 << INT_JOYPAD;
}

