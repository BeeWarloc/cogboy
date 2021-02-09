use crate::romfile::RomFile;
use std;
use std::io;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Cartridge {
    pub rom: RomFile,
    ram: Vec<u8>,
    pub rom_bank: u8,
    ram_enabled: bool, // ram_bank: u8
}

struct RomHeader {
    ram_size: usize,
}

impl RomHeader {
    fn new(rom: &Vec<u8>) -> RomHeader {
        RomHeader {
            ram_size: match rom[0x0149] {
                0 => 0,
                1 => 2 * 1024,
                2 => 8 * 1024,
                3 => 32 * 1024,
                _ => 0,
            },
        }
    }
}

impl Cartridge {
    pub fn load(filename: &str) -> io::Result<Cartridge> {
        let rom = RomFile::load(filename).expect("Unable to load file");
        let header = RomHeader::new(&(rom.bytes));

        let cart = Cartridge {
            rom,
            rom_bank: 1,
            ram: vec![0; header.ram_size],
            ram_enabled: false,
        };

        Ok(cart)
    }

    pub fn clear_ram(&mut self) {
        for i in 0..self.ram.len() {
            self.ram[i] = 0
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        let value = match addr {
            0x0000..=0x3fff => self.rom[addr as usize],
            0x4000..=0x7fff => {
                // TODO bound checking for invalid ROM BANK select, or maybe mask?
                let offset = addr as usize + ((self.rom_bank as usize - 1) * 0x4000) as usize;
                let value = self.rom[offset];
                trace!("Reading value {:02x} from addr ROM{}:{:04x} at rom offset 0x{:04x}",
                       value,
                       self.rom_bank,
                       addr,
                       offset);
                value
            }
            0xa000..=0xbfff => {
                let idx = (addr - 0xa000) as usize;
                if idx < self.ram.len() {
                    self.ram[idx]
                } else {
                    0xff
                }
            }
            _ => 0xff,
        };
        value
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        trace!("Writing {:02x} to {:04x}", value, addr);
        match addr {
            0x0000..=0x1fff => {
                self.ram_enabled = self.ram.len() > 0 && (value & 0xf) == 0xa;
            }
            0x2000..=0x3fff => {
                self.rom_bank = std::cmp::max(value & 31, 1);
                trace!("Switching to bank {}", self.rom_bank);
            }
            0x4000..=0x5fff => {
                // TODO: RAM bank switch
                if value != 0 {
                    unimplemented!()
                }
            }
            0xa000..=0xbfff => {
                let idx = (addr - 0xa000) as usize;
                if idx < self.ram.len() {
                    self.ram[idx] = value;
                }
            }
            _ => trace!("NOT MAPPED FOR WRITE {:x} (value 0b{:05b})", addr, value),
        }
    }
}
