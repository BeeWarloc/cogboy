use std::cmp;
use std::mem;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Sound {
    // nr52
    square1: SquareGenerator,
    square2: SquareGenerator,
    sound_output_select: u8,
    buffer: Vec<(i8, i8)>,
    seq_counter: u32,
}

impl Sound {
    pub fn new() -> Sound {
        Sound {
            square1: SquareGenerator::new(),
            square2: SquareGenerator::new(),
            sound_output_select: 0,
            buffer: Vec::new(),
            seq_counter: 0,
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            0xff10..=0xff14 => self.square1.read(addr.wrapping_sub(0xff10)),
            0xff16..=0xff19 => self.square2.read(addr.wrapping_sub(0xff15)),
            0xff25 => self.sound_output_select,
            _ => 0xff,
        }
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        match addr {
            0xff10..=0xff14 => self.square1.write(addr.wrapping_sub(0xff10), value),
            0xff16..=0xff19 => self.square2.write(addr.wrapping_sub(0xff15), value),
            0xff25 => self.sound_output_select = value,
            _ => (),
        }
    }

    pub fn tick(&mut self, cycles: usize) {
        if cycles > 4 {
            self.tick(4);
            self.tick(cycles - 4);
            return;
        }

        // Divide 4MHz (2^22) to 512Hz (2^9) counter by checking last 13 (22-9) bits
        if self.seq_counter & ((1 << 13) - 1) == 0 {
            let step_number = (self.seq_counter >> 13) & 0x7;
            self.square1.frame_sequencer_tick(step_number);
            self.square2.frame_sequencer_tick(step_number);
        }

        self.seq_counter += cycles as u32;

        let s1 = self.square1.next_sample();
        let s2 = self.square2.next_sample();

        let ss = self.sound_output_select;
        let c1 = if ss & 0x01 != 0 { s1 } else { 0 } + if ss & 0x02 != 0 { s2 } else { 0 };

        let c2 = if ss & 0x10 != 0 { s1 } else { 0 } + if ss & 0x20 != 0 { s2 } else { 0 };

        self.buffer.push((c1, c2));
    }

    pub fn dump_audio(&mut self) -> Vec<(i8, i8)> {
        mem::replace(&mut self.buffer, Vec::new())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct SquareGenerator {
    duty_number: i32,
    length_load: i32,
    length_enabled: bool,
    start_volume: i32,
    volume: i32,
    envelope_direction: i32,
    envelope_period: i32,
    envelope_counter: i32,
    frequency: i32,
    sweep_period: i32,
    shadow_frequency: i32,
    sweep_direction: i32,
    sweep_shift: i32,
    triggered: bool,
    duty: u8,
    period_counter: i32,
    length_counter: i32,
    sweep_counter: i32,
    internal_enabled: bool,
}

impl SquareGenerator {
    fn new() -> SquareGenerator {
        // TODO: Make sure we have correct initial values
        SquareGenerator {
            duty_number: 0,
            length_load: 0,
            length_enabled: false,
            start_volume: 0,
            volume: 0,
            envelope_direction: 1,
            envelope_period: 0,
            envelope_counter: 0,
            frequency: 0,
            sweep_period: 0,
            shadow_frequency: 0,
            sweep_direction: 1,
            sweep_shift: 0,
            sweep_counter: 8,
            triggered: false,
            duty: 0,
            period_counter: 0,
            length_counter: 0,
            internal_enabled: true,
        }
    }

    fn write(&mut self, addr: u16, value: u8) {
        let value = value as i32;
        match addr {
            0 => {
                self.sweep_period = (value >> 4) & 0x7;
                self.sweep_direction = if value & 0x08 == 0 { 1 } else { -1 };
                self.sweep_shift = value & 0x7;
            }
            1 => {
                // Write to NRx1
                self.length_counter = 64 - (value & 0x3f);
                self.duty_number = value >> 6;
            }
            2 => {
                self.start_volume = value >> 4;
                self.envelope_direction = if value & 0x08 != 0 { 1 } else { -1 };
                self.envelope_period = value & 0x7;
            }
            3 => {
                self.frequency = (self.frequency & !0xff) | value;
            }
            4 => {
                self.frequency = (self.frequency & 0xff) | ((value & 0x7) << 8);
                self.length_enabled = value & 0x40 != 0;
                self.triggered = value & 0x80 != 0;
            }
            _ => (),
        }
    }

    fn read(&self, addr: u16) -> u8 {
        (match addr {
            0 => {
                self.sweep_period << 4
                    | match self.sweep_direction {
                        -1 => 0x08,
                        _ => 0,
                    }
                    | self.sweep_shift
                    | 0x80
            }
            1 => (self.duty_number << 6) | 0x3f,
            2 => {
                (self.start_volume << 4)
                    | match self.envelope_direction {
                        -1 => 0,
                        _ => 0x08,
                    }
                    | (self.envelope_period & 0x07)
            }
            3 => 0xff, // Write only
            4 => 0xbf | if self.length_enabled { 0x40 } else { 0 },
            _ => 0xff,
        }) as u8
    }

    fn reset_period_counter(&mut self) {
        self.period_counter = 2048 - self.frequency;
    }

    fn calc_next_frequency(&self) -> Option<i32> {
        let next_frequency = self.shadow_frequency
            + (self.shadow_frequency >> self.sweep_shift) * self.sweep_direction;
        if next_frequency > 2047 || next_frequency < 0 {
            None
        } else {
            Some(next_frequency)
        }
    }

    fn frame_sequencer_tick(&mut self, step: u32) {
        if step & 1 == 0 && self.length_enabled && self.length_counter > 0 {
            // TODO: Length clock
            self.length_counter -= 1;
            if self.length_counter == 0 {
                self.internal_enabled = false;
            }
        }

        if step & 0b11 == 0b10 && self.sweep_shift != 0 {
            self.sweep_counter -= 1;
            if self.sweep_counter <= 0 {
                if let Some(next_freq) = self.calc_next_frequency() {
                    self.shadow_frequency = next_freq;
                    self.frequency = next_freq;

                    // then frequency calculation and overflow check are run AGAIN immediately
                    // using this new value, but this second new frequency is not written back.
                    if self.calc_next_frequency().is_none() {
                        self.internal_enabled = false;
                    }
                } else {
                    self.internal_enabled = false;
                }
            }
        }

        if step & 0b111 == 0b111 && self.envelope_period != 0 {
            self.envelope_counter -= 1;
            if self.envelope_counter <= 0 {
                self.volume = cmp::max(cmp::min(self.envelope_direction + self.volume, 15), 0);
                self.envelope_counter = self.envelope_period;
            }
        }
    }

    fn next_sample(&mut self) -> i8 {
        if self.triggered {
            self.duty = self.duty();
            self.volume = self.start_volume;
            self.envelope_counter = self.envelope_period;
            self.length_counter = 64;

            self.shadow_frequency = self.frequency;
            self.sweep_counter = self.sweep_period;
            // TODO: The internal enabled flag is set if either the sweep period or shift are non-zero, cleared otherwise.
            // This is confusing, does this mean you can't disable?
            self.internal_enabled = true;

            self.reset_period_counter();
            self.triggered = false;
        }
        if !self.internal_enabled {
            return 0;
        }

        let value = if self.duty & 0x80 == 0 {
            -self.volume as i8
        } else {
            self.volume as i8
        };
        self.period_counter -= 1;
        if self.period_counter == 0 {
            self.reset_period_counter();
            self.duty = self.duty.rotate_left(1);
        }

        value
    }

    fn duty(&self) -> u8 {
        match self.duty_number & 0x3 {
            0 => 0b00000001,
            1 => 0b10000001,
            2 => 0b10000111,
            3 => 0b01111110,
            _ => unreachable!(),
        }
    }
}
