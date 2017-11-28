
use std::mem;

use super::constants::*;

#[derive(Debug)]
enum LcdMode {
    Hblank,
    Vblank,
    SearchingOam,
    Transfer,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Lcd {
    counter: i32,
    pub vblank_sync: bool,
    pub buffer: Vec<u8>,
    last_frame: Vec<u8>,
    oam: Vec<u8>,
    vram_tile_data: Vec<u8>,
    vram_tile_table: Vec<u8>,
    lcdc: u8,
    stat: u8,
    scx: u8,
    scy: u8,
    ly: u8,
    lyc: u8,
    bgp: u8,
    obp0: u8,
    obp1: u8,
    wy: u8,
    wx: u8,

    pub show_bg: bool,
    pub show_sprites: bool,
    pub show_window: bool
}

#[derive(Debug)]
struct SpriteOamEntry {
    x: u8,
    y: u8,
    tile_id: u8,
    flags: u8,
}

impl SpriteOamEntry {
    fn is_behind_bg(&self) -> bool {
        self.flags & 0x80 != 0
    }
    fn is_vertically_flipped(&self) -> bool {
        self.flags & 0x40 != 0
    }
    fn is_horizontally_flipped(&self) -> bool {
        self.flags & 0x20 != 0
    }
    fn palette_number(&self) -> u8 {
        (self.flags >> 4) & 1
    }
    fn get_y(&self) -> i32 {
        self.y as i32 - 16
    }
    fn get_x(&self) -> i32 {
        self.x as i32 - 8
    }


    fn covers_line(&self, sprite_height: i32, line_y: i32) -> bool {
        let sprite_y = self.get_y();
        (line_y >= sprite_y) && (line_y < sprite_y + sprite_height)
    }
}

impl Lcd {
    pub fn new() -> Lcd {
        let mut lcd = Lcd {
            counter: 0,
            vblank_sync: false,
            buffer: vec![0u8; LCD_WIDTH * LCD_HEIGHT],
            last_frame: vec![0u8; LCD_WIDTH * LCD_HEIGHT],
            oam: vec![0u8; 0xa0],
            // De-interleaved and expanded pixels
            vram_tile_data: vec![0u8; 384 * 8 * 8],
            vram_tile_table: vec![0u8; 0x800],
            lcdc: 0x91,
            stat: 0,
            scx: 0,
            scy: 0,
            ly: 0,
            lyc: 0,
            bgp: 0xfc,
            obp0: 0xff,
            obp1: 0xff,
            wy: 0,
            wx: 0,
            show_bg: true,
            show_sprites: true,
            show_window: true
        };
        lcd.set_mode(LcdMode::Hblank);
        lcd
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            MEM_LCD_IO_LCDC => self.lcdc,
            MEM_LCD_IO_STAT => self.stat,
            MEM_LCD_IO_SCY => self.scy,
            MEM_LCD_IO_SCX => self.scx,
            MEM_LCD_IO_LY => self.ly,
            MEM_LCD_IO_LYC => self.lyc,
            MEM_LCD_IO_BGP => self.bgp,
            MEM_LCD_IO_OBP0 => self.obp0,
            MEM_LCD_IO_OBP1 => self.obp1,
            MEM_LCD_IO_WY => self.wy,
            MEM_LCD_IO_WX => self.wx,
            MEM_LCD_VRAM_TILE_DATA_START...MEM_LCD_VRAM_TILE_DATA_END => {
                self.read_vram_tile_data(addr)
            }
            MEM_LCD_VRAM_TILE_MAP_START...MEM_LCD_VRAM_TILE_MAP_END => {
                self.vram_tile_table[(addr - MEM_LCD_VRAM_TILE_MAP_START) as usize]
            }
            MEM_LCD_OAM_START...MEM_LCD_OAM_END => self.read_oam(addr),
            _ => 0xff,
        }
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        match addr {
            MEM_LCD_IO_LCDC => self.lcdc = value,
            MEM_LCD_IO_STAT => self.stat = (value & 0b1111000) | (self.stat & 0b111),
            MEM_LCD_IO_SCY => self.scy = value,
            MEM_LCD_IO_SCX => self.scx = value,
            MEM_LCD_IO_LY => self.ly = 0, // LY (writing will reset the counter)
            MEM_LCD_IO_LYC => self.lyc = value,
            MEM_LCD_IO_BGP => self.bgp = value,
            MEM_LCD_IO_OBP0 => self.obp0 = value,
            MEM_LCD_IO_OBP1 => self.obp1 = value,
            MEM_LCD_IO_WX => self.wx = value,
            MEM_LCD_IO_WY => self.wy = value,
            MEM_LCD_VRAM_TILE_DATA_START...MEM_LCD_VRAM_TILE_DATA_END => {
                self.write_vram_tile_data(addr, value)
            }
            MEM_LCD_VRAM_TILE_MAP_START...MEM_LCD_VRAM_TILE_MAP_END => {
                self.vram_tile_table[(addr - MEM_LCD_VRAM_TILE_MAP_START) as usize] = value
            }
            MEM_LCD_OAM_START...MEM_LCD_OAM_END => self.write_oam(addr, value),
            _ => (),
        }
    }

    pub fn try_get_buffer(&mut self) -> Option<Vec<u8>> {
        if self.vblank_sync {
            self.vblank_sync = false;
            Some(mem::replace(&mut self.last_frame, vec![0u8; LCD_WIDTH * LCD_HEIGHT]))
        } else {
            None
        }
    }

    fn read_oam(&self, addr: u16) -> u8 {
        self.oam[(addr - MEM_LCD_OAM_START) as usize]
    }

    fn write_oam(&mut self, addr: u16, value: u8) {
        self.oam[(addr - MEM_LCD_OAM_START) as usize] = value;
    }

    fn read_vram_tile_data(&self, addr: u16) -> u8 {
        let offset = ((addr & 0xfffe) - MEM_LCD_VRAM_TILE_DATA_START) as usize * 4;
        let mut value = 0;
        let set_mask = 1u8 << (addr & 1);

        for i in 0..8 {
            value <<= 1;
            if (self.vram_tile_data[offset + i] & set_mask) != 0 {
                value |= 0x1;
            }
        }
        value
        // self.vram[(addr - MEM_LCD_VRAM_START) as usize]
    }

    fn write_vram_tile_data(&mut self, addr: u16, value: u8) {
        let offset = ((addr & 0xfffe) - MEM_LCD_VRAM_TILE_DATA_START) as usize * 4;
        let mut v = value;
        let set_mask = 1u8 << (addr & 1);
        let clear_mask = !set_mask;

        for i in 0..8 {
            if v & 0x80 == 0 {
                self.vram_tile_data[offset + i] &= clear_mask;
            } else {
                self.vram_tile_data[offset + i] |= set_mask;
            }
            v <<= 1;
        }
        // assert_eq!(value, self.read_vram_tile_data(addr));
        // self.vram[(addr - MEM_LCD_VRAM_START) as usize] = value;
    }

    fn is_window_enabled(&self) -> bool {
        (self.lcdc & LCDC_WINDOW_DISPLAY_ENABLE) != 0
    }

    fn get_mode(&self) -> LcdMode {
        match self.stat & 3 {
            0b00 => LcdMode::Hblank,
            0b01 => LcdMode::Vblank,
            0b10 => LcdMode::SearchingOam,
            0b11 => LcdMode::Transfer,
            _ => unreachable!(),
        }
    }

    fn get_oam_entry(&self, index: usize) -> SpriteOamEntry {
        let offset = index * 4;
        SpriteOamEntry {
            y: self.oam[offset],
            x: self.oam[offset + 1],
            tile_id: self.oam[offset + 2],
            flags: self.oam[offset + 3],
        }
    }

    fn get_sprite_height(&self) -> i32 {
        if (self.lcdc & 0x04) == 0 { 8 } else { 16 }
    }

    fn get_oam_entries(&self, line_y: i32) -> Vec<SpriteOamEntry> {
        let sprite_height = self.get_sprite_height();

        let mut entries: Vec<SpriteOamEntry> = (0..40)
            .map(|idx| self.get_oam_entry(idx))
            .filter(|entry| entry.covers_line(sprite_height, line_y))
            .collect();


        entries.sort_by_key(|entry| entry.x);
        if entries.len() > 10 {
            entries.truncate(10);
        }
        entries
    }


    fn draw_sprites(&mut self, line_y: i32) {
        let sprite_height = self.get_sprite_height();
        let entries = self.get_oam_entries(line_y);
        let line_start = line_y * LCD_WIDTH as i32;

        for entry in entries {
            let palette = if entry.palette_number() == 0 {
                self.obp0
            } else {
                self.obp1
            };

            let tile_id = if sprite_height == 16 {
                entry.tile_id & 0xfe
            } else {
                entry.tile_id
            };
            let sprite_y = entry.get_y();

            let sprite_x = entry.get_x();
            let tile_y = line_y - sprite_y;
            let tile_y = if entry.is_vertically_flipped() {
                (sprite_height - 1) - tile_y
            } else {
                tile_y
            };
            let tile_offset = (tile_id as i32 * 8 * 8) + tile_y * 8;

            for tile_x in 0..8 {
                let screen_x = sprite_x + tile_x;
                let tile_x = if entry.is_horizontally_flipped() {
                    7 - tile_x
                } else {
                    tile_x
                };
                if screen_x >= 0 && screen_x < LCD_WIDTH as i32 {
                    let value = self.vram_tile_data[(tile_offset + tile_x) as usize];
                    let screen_offset = (line_start + screen_x) as usize;

                    if value != 0 && (!entry.is_behind_bg() || self.buffer[screen_offset] >= 0x80) {
                        let color = Lcd::palette_lookup(palette, value);
                        self.buffer[screen_offset] = color;
                    }
                }
            }
            // l0 = self.vram[]
        }
    }

    fn set_mode(&mut self, mode: LcdMode) {
        trace!("Entering lcd mode {:?}", mode);
        let stat = self.stat & !0x03;
        match mode {
            LcdMode::Hblank => {
                self.counter += 207;
                self.stat = stat | 0b00;
            }
            LcdMode::Vblank => {
                self.counter += 456;
                self.stat = stat | 0b01;
            }
            LcdMode::SearchingOam => {
                self.counter += 80;
                self.stat = stat | 0b10;
            }
            LcdMode::Transfer => {
                self.counter += 169;
                self.stat = stat | 0b11;
            }
        }
    }

    fn inc_ly(&mut self) -> u8 {
        self.ly = (self.ly + 1) % 154;
        trace!("Lcd ly incremented to {}", self.ly);
        if self.ly == self.lyc {
            self.stat |= 0x04;
            if self.stat & 0x40 != 0 {
                return INT_MASK_LCDSTAT;
            }
        } else {
            self.stat &= !0x04;
        }

        0 // No interrupt set
    }

    fn get_bg_tile_offset(&self, tile_id: u8) -> usize {
        if (self.lcdc & 0x10) == 0 {
            (0x1000 + ((tile_id as i8) as isize * 16)) as usize * 4
        } else {
            (0x0000 + (tile_id as usize * 16)) * 4
        }
    }

    fn get_window_at(&self, x: u8, y: u8) -> u8 {
        // Probably very unoptimized, shouldn't matter too much though..
        let tile_base = if (self.lcdc & LCDC_WINDOW_TILE_MAP_DISPLAY_SELECT) == 0 {
            0
        } else {
            0x400
        };

        let tile_id = self.vram_tile_table[tile_base + (((y & 0xf8) as usize) << 2) +
                                           ((x as usize) >> 3)];
        let palette_idx = self.vram_tile_data[self.get_bg_tile_offset(tile_id) +
                                              (((y & 0x7) << 3) | (x & 0x7)) as usize];
        // let line_offset = self.get_bg_tile_offset(tile_id) + ((y & 0x7) * 2) as usize;
        // let l0 = self.read_vram_tile_data(line_offset as u16 + MEM_LCD_VRAM_TILE_DATA_START);
        // let l1 = self.read_vram_tile_data(line_offset as u16 + 1 + MEM_LCD_VRAM_TILE_DATA_START);
        // let bit_offset = 7 - (x & 0x7);
        // let palette_idx = (((l0 >> bit_offset) & 1) | (((l1 >> bit_offset) & 1) << 1)) as usize;
        //
        Lcd::palette_lookup(self.bgp, palette_idx)
    }

    fn get_bg_at(&self, x: u8, y: u8) -> u8 {
        // Probably very unoptimized, shouldn't matter too much though..
        let tile_base = if (self.lcdc & LCDC_BG_TILE_MAP_DISPLAY_SELECT) == 0 {
            0
        } else {
            0x400
        };

        let tile_id = self.vram_tile_table[tile_base + (((y & 0xf8) as usize) << 2) +
                                           ((x as usize) >> 3)];
        let palette_idx = self.vram_tile_data[self.get_bg_tile_offset(tile_id) +
                                              (((y & 0x7) << 3) | (x & 0x7)) as usize];
        // let line_offset = self.get_bg_tile_offset(tile_id) + ((y & 0x7) * 2) as usize;
        // let l0 = self.read_vram_tile_data(line_offset as u16 + MEM_LCD_VRAM_TILE_DATA_START);
        // let l1 = self.read_vram_tile_data(line_offset as u16 + 1 + MEM_LCD_VRAM_TILE_DATA_START);
        // let bit_offset = 7 - (x & 0x7);
        // let palette_idx = (((l0 >> bit_offset) & 1) | (((l1 >> bit_offset) & 1) << 1)) as usize;
        //
        Lcd::palette_lookup(self.bgp, palette_idx) | if palette_idx == 0 { 0x80 } else { 0 }
    }

    fn palette_lookup(palette: u8, index: u8) -> u8 {
        (palette >> ((index & 0x03) * 2)) & 0x03
    }

    pub fn tick(&mut self, cycles: usize) -> u8 {
        if self.lcdc & 0x80 == 0 {
            return 0;
        }

        trace!("Lcd tick");

        let max_step: usize = 80;

        if cycles > max_step {
            return self.tick(max_step) | self.tick(cycles - max_step);
        }


        let mut ints = 0u8;
        self.counter -= cycles as i32;
        if self.counter <= 0 {
            let mode = self.get_mode();
            match mode {
                LcdMode::Hblank => {
                    // Transition from HBlank to either SearchingOam or Vblank
                    // I think LY increments at this point
                    ints |= self.inc_ly();
                    if self.ly >= 144 {
                        self.vblank_sync = true;
                        mem::swap(&mut self.buffer, &mut self.last_frame);
                        self.set_mode(LcdMode::Vblank);
                        ints |= INT_MASK_VBLANK;
                    } else {
                        self.set_mode(LcdMode::SearchingOam);
                    }
                }
                LcdMode::Vblank => {
                    ints |= self.inc_ly();
                    if self.ly == 0 {
                        self.set_mode(LcdMode::SearchingOam);
                    } else {
                        self.set_mode(LcdMode::Vblank);
                    }
                }
                LcdMode::SearchingOam => {
                    self.set_mode(LcdMode::Transfer);
                }
                LcdMode::Transfer => {
                    self.draw_line();
                    self.set_mode(LcdMode::Hblank);
                    if self.stat & 0x08 != 0 {
                        ints |= INT_MASK_LCDSTAT;
                    }
                }
            }
        }
        ints
    }

    fn draw_line(&mut self) {
        let y = self.ly;
        let offset = y as usize * LCD_WIDTH;
        if self.show_bg {
            for x in 0..LCD_WIDTH {
                self.buffer[offset + x] =
                    self.get_bg_at((x as u8).wrapping_add(self.scx), y.wrapping_add(self.scy));
            }
        }
        if self.show_sprites {
            self.draw_sprites(y as i32);
        }
        if self.show_window {
            if self.is_window_enabled() && y >= self.wy {
                let win_y = y - self.wy;
                for x in (self.wx.saturating_sub(7) as usize)..LCD_WIDTH {
                    self.buffer[offset + x] =
                        self.get_window_at((x as u8).wrapping_sub(self.wx.wrapping_sub(7)), win_y);
                }
            }
        }
    }
}

