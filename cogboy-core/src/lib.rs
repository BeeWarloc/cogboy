extern crate serde;
extern crate bincode;
extern crate flate2;

#[macro_use]
extern crate serde_derive;

#[macro_use]
extern crate log;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate enum_primitive;
extern crate num;

use zlob::Zlob;
use std::cmp;
use std::collections::VecDeque;

mod romfile;
pub mod zlob;
pub mod cpu;
pub mod bus;
pub mod cartridge;
pub mod sound;
pub mod lcd;

use self::cpu::Cpu;


#[derive(Clone,Copy,Debug)]
pub struct EventEntry {
    pub time: u64,
    pub joypad: u8
}

#[derive(Clone)]
pub struct System {
    pub cpu: Box<Cpu>,
    pub passed_events: Vec<EventEntry>,
    pub pending_events: VecDeque<EventEntry>,
    pub snapshots: Vec<(u64, Zlob<Cpu>)>
}


pub trait RunContext {
    fn check_watchpoint(&mut self, addr: u16, value: u8);

    #[inline]
    fn after_step(&mut self, _cpu: &Cpu, _passed_cycles: usize) -> bool { false }
}

impl System {
    pub fn new(path: &str) -> System {
        System {
            cpu: Box::new(Cpu::new(path)),
            passed_events: Vec::new(),
            pending_events: VecDeque::new(),
            snapshots: Vec::new(),
        }
    }

    pub fn cycles_since_last_snapshot(&self) -> u64 {
        self.cpu.cycles.saturating_sub(self.snapshots.iter().last().map(|x| x.0).unwrap_or(0))
    }

    pub fn update_joypad(&mut self, buttons: u8) {
        if self.cpu.bus.io.joypad_all_buttons != buttons {
            if self.pending_events.len() > 0 {
                println!("Overriding pending events!");
                self.pending_events.clear();
            }
            self.pending_events.push_front(EventEntry { time: self.cpu.cycles, joypad: buttons });
        }
    }

    pub fn drain_serial_out(&mut self) -> String {
        self.cpu.bus.io.serial_out.drain(..).map(|x| x as char).collect::<String>()
    }

    pub fn snapshots_size(&self) -> usize {
        self.snapshots.iter().map(|(_, snapshot)| snapshot.size()).sum()
    }

    pub fn rewind_to_closest_snapshot(&mut self, target_cycle: u64) {
        if target_cycle >= self.cpu.cycles {
            return;
        }

        println!("Rewinding {} cycles to {}", self.cpu.cycles - target_cycle, target_cycle);
        
        let mut closest_snapshot = None;

        println!("    there are {} snapshots to pick from", self.snapshots.len());
        while let Some((snapshot_cycle, snapshot)) = self.snapshots.pop() {
            if target_cycle >= snapshot_cycle {
                closest_snapshot = Box::new(snapshot.deserialize()).into();
                // Push the used snapshot back in place
                self.snapshots.push((snapshot_cycle, snapshot));
                break;
            }
        }
        match closest_snapshot {
            Some(snapshot) => {
                println!("   closest snapshot is at cycle {}, which means we must forward {} cycles", snapshot.cycles, target_cycle - snapshot.cycles);
                self.cpu = snapshot;
            }
            None => {
                self.cpu.reset();
            }
        }
        while let Some(ev) = self.passed_events.pop() {
            if ev.time > self.cpu.cycles {
                self.pending_events.push_front(ev)
            } else {
                self.passed_events.push(ev);
                break
            }
        }
    }

    pub fn run_to_cycle(&mut self, target_cycle: u64, ctx: &mut impl RunContext) {
        if self.cpu.cycles == target_cycle {
            return;
        }

        self.rewind_to_closest_snapshot(target_cycle);
        
        'outer: while self.cpu.cycles < target_cycle {
            while let Some(ev) = self.pending_events.pop_front() {
                if ev.time > self.cpu.cycles {
                    self.pending_events.push_front(ev);
                    break;
                }
                self.cpu.bus.io.joypad_all_buttons = ev.joypad; 
                self.passed_events.push(ev);
            }

            let next_event_cycle = cmp::min(target_cycle, self.pending_events.front().map(|ev| ev.time).unwrap_or(std::u64::MAX));

            while self.cpu.cycles <= next_event_cycle {  
                let passed_cycles = self.cpu.step(ctx).unwrap();
                if ctx.after_step(&self.cpu, passed_cycles) {
                    break 'outer
                }
            }
            // trace!("Cpu state {}: {:?} IME {} IE {:05b} {:05b}", cpu.cycles, cpu.regs, cpu.interrupts_enabled,
            //       cpu.bus.io.interrupt_enable, cpu.bus.io.interrupt_flags );;
        }
    }

    pub fn take_snapshot(&mut self) {
        let snapshot: Zlob<Cpu> = self.cpu.as_ref().into();
        // if let Some(&(_, ref prev_snapshot)) = self.snapshots.iter().last() {
        //     println!("Different bytes from last snapshot: {}", prev_snapshot.count_diff_bytes(&snapshot))
        // }
        self.snapshots.push((self.cpu.cycles, snapshot))
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

    pub const LCDC_BG_ENABLED: u8 = 1 << 0;
    pub const LCDC_SPRITES_ENABLED: u8 = 1 << 1;
    pub const LCDC_SPRITES_SIZE: u8 = 1 << 2;
    pub const LCDC_BG_TILE_MAP_DISPLAY_SELECT: u8 = 1 << 3;
    pub const LCDC_BG_AND_WINDOW_TILE_DATA_SELECT: u8 = 1 << 4;
    pub const LCDC_WINDOW_DISPLAY_ENABLE: u8 = 1 << 5;
    pub const LCDC_WINDOW_TILE_MAP_DISPLAY_SELECT: u8 = 1 << 6;
    pub const LCDC_DISPLAY_ENABLE: u8 = 1 << 7;

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

