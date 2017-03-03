use std;
use std::io;
use std::sync::Arc;

use std::fs::File;
use std::io::Read;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Cartridge {
    rom: Arc<Vec<u8>>,
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
        let mut f = try!(File::open(&filename));
        let mut rom: Vec<u8> = Vec::new();
        try!(f.read_to_end(&mut rom));
        println!("Loaded ROM with MBC type {:02x} ROM SIZE (ID) {:02x} RAM SIZE (ID) {:02x}",
                 rom[0x147],
                 rom[0x148],
                 rom[0x149]);

        let header = RomHeader::new(&rom);

        let cart = Cartridge {
            rom: Arc::new(rom),
            rom_bank: 1,
            ram: vec![0; header.ram_size],
            ram_enabled: false,
        };

        Ok(cart)
    }

    pub fn read(&self, addr: u16) -> u8 {
        let value = match addr {
            0x0000...0x3fff => self.rom[addr as usize],
            0x4000...0x7fff => {
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
            0xa000...0xbfff => {
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
            0x0000...0x1fff => {
                self.ram_enabled = self.ram.len() > 0 && (value & 0xf) == 0xa;
            }
            0x2000...0x3fff => {
                self.rom_bank = std::cmp::max((value & 0x1f), 1);
                trace!("Switching to bank {}", self.rom_bank);
            }
            0x4000...0x5fff => {
                // TODO: RAM bank switch
                if value != 0 {
                    unimplemented!()
                }
            }
            0xa000...0xbfff => {
                let idx = (addr - 0xa000) as usize;
                if idx < self.ram.len() {
                    self.ram[idx] = value;
                }
            }
            _ => trace!("NOT MAPPED FOR WRITE {:x} (value 0b{:05b})", addr, value),
        }
    }
}
