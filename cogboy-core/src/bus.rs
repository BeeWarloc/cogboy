use std::mem;

use super::cartridge::Cartridge;
use super::sound::Sound;
use super::lcd::Lcd;
use super::constants::*;

use num::FromPrimitive;
use std::io;
use std::collections::VecDeque;

use std::fs::File;
use std::io::Write;


#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct Bus {
    pub cartridge: Cartridge,
    // TODO: Expose in better way
    pub lcd: Lcd,
    pub sound: Sound,
    wram: Vec<u8>,
    hram: Vec<u8>,
    pub io: IoPorts,
}

enum_from_primitive! {
#[derive(Debug,PartialEq)]
enum IoAddress {
    Joypad = 0xff00,
    Sb = 0xff01,
    Sc = 0xff02,
    Div = 0xff04,
    Tima = 0xff05,
    Tma = 0xff06,
    Tac = 0xff07,
    If = 0xff0f,
    Ie = 0xffff 
}
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct Timers {
    div: u8,
    tima: u8,
    tma: u8,
    tac: u8,

    div_counter: isize,
    tima_counter: isize,
    tima_running: bool,
}

impl Timers {
    fn new() -> Timers {
        let mut timers = Timers {
            div: 0,
            tima: 0,
            tma: 0,
            tac: 0,
            div_counter: 0,
            tima_counter: 0,
            tima_running: false,
        };
        timers.write_tac(0);
        timers
    }

    pub fn tick(&mut self, cycles: usize) -> u8 {
        let mut interrupt = 0u8;

        if cycles > 12 {
            interrupt |= self.tick(12);
            interrupt |= self.tick(cycles - 12);
            return interrupt;
        }

        self.div_counter -= cycles as isize;
        if self.div_counter <= 0 {
            self.div_counter += (CPU_FREQ / 16384) as isize;
            self.div = self.div.wrapping_add(1);
        }

        if self.tima_running {
            self.tima_counter -= cycles as isize;
            if self.tima_counter <= 0 {
                self.tima_counter += (CPU_FREQ / self.get_tac_freq()) as isize;
                self.tima = self.tima.wrapping_add(1);
                if self.tima == 0 {
                    interrupt |= INT_MASK_TIMER;
                    self.tima = self.tma;
                }
            }
        }
        interrupt
    }

    fn get_tac_freq(&self) -> usize {
        match self.tac & 0x03 {
            0b00 => 4096,
            0b01 => 262144,
            0b10 => 65536,
            0b11 => 16384,
            _ => unreachable!(),
        }
    }
    fn write_div(&mut self) {
        self.div = 0;
        self.div_counter = 0;
    }
    fn write_tima(&mut self, value: u8) {
        self.tima = value;
        // self.tima_counter = (CPU_FREQ / self.get_tac_freq()) as isize - 4;
    }
    fn write_tma(&mut self, value: u8) {
        self.tma = value;
    }
    fn write_tac(&mut self, value: u8) {
        self.tac = value & 0x07;
        self.tima_running = (self.tac & 0x04) != 0;
        trace!("Setting tima freq to {}", self.get_tac_freq());
    }
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct IoPorts {
    joypad: u8,
    serial_data: u8,
    serial_transfer_control: u8,
    pub timers: Timers,

    // TODO expose through other means
    pub interrupt_enable: u8,
    pub interrupt_flags: u8,

    pub joypad_all_buttons: u8,

    pub serial_out: VecDeque<u8>,
}

impl IoPorts {
    fn new() -> IoPorts {
        IoPorts {
            joypad: 0xff,
            serial_data: 0,
            serial_transfer_control: 0,
            timers: Timers::new(),
            interrupt_enable: 0,
            interrupt_flags: 0,
            joypad_all_buttons: 0xff,
            serial_out: VecDeque::new()
        }
    }

    fn write(&mut self, addr: u16, value: u8) {
        match IoAddress::from_u16(addr) {
            Some(x) => {
                trace!("Writing 0x{:02X} to {:?} (0x{:04X})", value, x, addr);
                match x {
                    IoAddress::Joypad => {
                        self.joypad = value | !0x30;
                    }
                    IoAddress::Sb => self.serial_data = value,
                    IoAddress::Sc => {
                        if value & 0x80 == 0x80 {
                            self.serial_out.push_back(self.serial_data);
                        }
                    }
                    IoAddress::Div => self.timers.write_div(),
                    IoAddress::Tima => self.timers.write_tima(value),
                    IoAddress::Tma => self.timers.write_tma(value),
                    IoAddress::Tac => self.timers.write_tac(value),
                    IoAddress::Ie => self.interrupt_enable = value & 0x1f,
                    IoAddress::If => self.interrupt_flags = value & 0x1f,
                }
            }
            None => {
                debug!("WRITE 0x{:02X} to IOPORT 0x{:04X} not recognized (ignoring)",
                       value,
                       addr);
            }
        }
    }


    fn read(&self, addr: u16) -> u8 {
        match IoAddress::from_u16(addr) {
            Some(x) => {
                let value = match x {
                    IoAddress::Sb => self.serial_data,
                    IoAddress::Joypad => {
                        (if self.joypad & 0x10 == 0 {
                            self.joypad_all_buttons & 0xf
                        } else {
                            0
                        }) |
                        (if self.joypad & 0x20 == 0 {
                            self.joypad_all_buttons >> 4
                        } else {
                            0
                        }) | self.joypad & 0xf0
                    }
                    IoAddress::Sc => self.serial_transfer_control,
                    IoAddress::Div => self.timers.div,
                    IoAddress::Tima => self.timers.tima,
                    IoAddress::Tma => self.timers.tma,
                    IoAddress::Tac => self.timers.tac,
                    IoAddress::Ie => self.interrupt_enable,
                    IoAddress::If => self.interrupt_flags,
                    // _ => panic!("IOPORT {:02X} NOT MAPPED FOR READING", addr)
                };
                value
            }
            None => {
                warn!("READ from IOPORT 0x{:04X} not recognized (ignoring)", addr);
                0
            }
        }
    }
}

impl Bus {
    pub fn new(cartridge: Cartridge) -> Bus {
        Bus {
            hram: vec![0u8; 0x7f],
            wram: vec![0u8; 0x2000],
            lcd: Lcd::new(),
            sound: Sound::new(),
            cartridge: cartridge,
            io: IoPorts::new(),
        }
    }

    pub fn dequeue_samples(&mut self) -> Vec<(i8, i8)> {
        self.sound.dump_audio()
    }

    pub fn dump_wram(&mut self) -> io::Result<()> {
        let mut f = try!(File::create("wram.dump"));
        try!(f.write_all(&self.wram));
        Ok(())
    }

    pub fn read(&self, addr: u16) -> u8 {
        let value = match addr {
            MEM_LCD_IO_DMA => 0xff,
            MEM_CARTRIDGE_ROM_START...MEM_CARTRIDGE_ROM_END =>
                self.cartridge.read(addr),
            MEM_LCD_VRAM_START...MEM_LCD_VRAM_END => self.lcd.read(addr),
            MEM_CARTRIDGE_RAM_START...MEM_CARTRIDGE_RAM_END =>
                self.cartridge.read(addr),
            0xc000...0xdfff => self.wram[(addr - 0xc000) as usize],
            0xe000...0xfdff => self.wram[(addr - 0xe000) as usize],
            MEM_LCD_OAM_START...MEM_LCD_OAM_END => self.lcd.read(addr),
            0xff10...0xff27 => self.sound.read(addr),
            MEM_LCD_IO_START...MEM_LCD_IO_END => self.lcd.read(addr),
            0xff00...0xff09 | 0xff28...0xff7f | 0xffff => self.io.read(addr),
            0xff80...0xfffe => self.hram[(addr - 0xff80) as usize],
            _ => {
                trace!("Reading unmapped {:x}, just returning 0xff", addr);
                0xff
            }
        };
        value
    }
    pub fn read16(&self, addr: u16) -> u16 {
        self.read(addr) as u16 | (self.read(addr + 1) as u16) << 8
    }
    pub fn write16(&mut self, addr: u16, value: u16) {
        self.write(addr, value as u8);
        self.write(addr + 1, (value >> 8) as u8);
    }
    pub fn write(&mut self, addr: u16, value: u8) {
        trace!("Writing {:02x} to {:04x}", value, addr);
        match addr {
            MEM_LCD_IO_DMA => {
                // TODO Refactor stuff in IoPorts to Bus
                // TODO This should not be needed before sprites are implemented
                let src_addr = (value as u16) << 8;
                let dst_addr = 0xfe00;
                for i in 0x00..0x9f {
                    let d = self.read(src_addr + i);
                    self.write(dst_addr + i, d);
                }
            }
            MEM_CARTRIDGE_ROM_START...MEM_CARTRIDGE_ROM_END =>
                self.cartridge.write(addr, value),
            MEM_LCD_VRAM_START...MEM_LCD_VRAM_END => self.lcd.write(addr, value),
            MEM_CARTRIDGE_RAM_START...MEM_CARTRIDGE_RAM_END =>
                self.cartridge.write(addr, value),
            0xc000...0xdfff => self.wram[(addr - 0xc000) as usize] = value,
            0xe000...0xfdff => self.wram[(addr - 0xe000) as usize] = value,
            MEM_LCD_OAM_START...MEM_LCD_OAM_END => self.lcd.write(addr, value),
            0xff10...0xff27 => self.sound.write(addr, value),
            MEM_LCD_IO_START...MEM_LCD_IO_END => self.lcd.write(addr, value),
            0xff00...0xff09 | 0xff28...0xff7f | 0xffff => self.io.write(addr, value),
            0xff80...0xfffe => self.hram[(addr - 0xff80) as usize] = value,
            _ => trace!("NOT MAPPED FOR WRITE {:x} (value 0b{:05b})", addr, value),
        }
    }
}
