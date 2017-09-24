use std::fmt;
use super::bus::Bus;
use super::cartridge::Cartridge;

#[derive(Clone,Serialize,Deserialize)]
pub struct Regs {
    pub a: u8, // accumulator
    pub f: u8, // flags

    pub b: u8,
    pub c: u8,

    pub d: u8,
    pub e: u8,

    pub h: u8,
    pub l: u8,

    pub sp: u16,
    pub pc: u16,
}

impl fmt::Debug for Regs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "Regs {{ AF {:02x}{:02x} BC {:02x}{:02x} DE {:02x}{:02x} HL {:02x}{:02x} SP {:04x} \
                PC {:04x} Z{}N{}H{}C{} }}",
               self.a,
               self.f,
               self.b,
               self.c,
               self.d,
               self.e,
               self.h,
               self.l,
               self.sp,
               self.pc,
               self.f >> 7,
               (self.f >> 6) & 1,
               (self.f >> 5) & 1,
               (self.f >> 4) & 1)
    }
}

impl Regs {
    fn new() -> Regs {
        Regs {
            a: 0x01,
            f: 0xb0,
            b: 0x00,
            c: 0x13,
            d: 0x00,
            e: 0xd8,
            h: 0x01,
            l: 0x4d,
            sp: 0xfffe,
            pc: 0x100,
        }
    }


    fn write_reg16(&mut self, reg: OpcodeRegister16, value: u16) {
        match reg {
            OpcodeRegister16::BC => {
                self.b = (value >> 8) as u8;
                self.c = value as u8;
            }
            OpcodeRegister16::DE => {
                self.d = (value >> 8) as u8;
                self.e = value as u8;
            }
            OpcodeRegister16::HL => {
                self.h = (value >> 8) as u8;
                self.l = value as u8;
            }
        }
    }

    fn read_reg16(&mut self, reg: OpcodeRegister16) -> u16 {
        match reg {
            OpcodeRegister16::BC => ((self.b as u16) << 8) | self.c as u16,
            OpcodeRegister16::DE => ((self.d as u16) << 8) | self.e as u16,
            OpcodeRegister16::HL => ((self.h as u16) << 8) | self.l as u16,
        }
    }
}

const CPU_FLAG_Z: u8 = 0x80;
const CPU_FLAG_N: u8 = 0x40;
const CPU_FLAG_H: u8 = 0x20;
const CPU_FLAG_C: u8 = 0x10;

#[derive(Debug, Clone, Copy)]
pub enum OpcodeOperand16 {
    BC,
    DE,
    HL,
    AF,
    SP,
    RefImmAddr(u16),
    Imm(u16),
    SPPlusImm(i8),
}

impl fmt::Display for OpcodeOperand16 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &OpcodeOperand16::BC => write!(f, "bc"),
            &OpcodeOperand16::DE => write!(f, "de"),
            &OpcodeOperand16::HL => write!(f, "hl"),
            &OpcodeOperand16::AF => write!(f, "af"),
            &OpcodeOperand16::SP => write!(f, "sp"),
            &OpcodeOperand16::RefImmAddr(addr) => write!(f, "(${:04x})", addr),
            &OpcodeOperand16::Imm(value) => write!(f, "${:04x}", value),
            &OpcodeOperand16::SPPlusImm(offset) => write!(f, "($ff00 + ${:02x})", offset),
        }
    }
}

impl OpcodeOperand16 {
    fn from_2bit_encoding(code: u8, offset: usize) -> OpcodeOperand16 {
        match (code >> offset) & 0x3 {
            0b00 => OpcodeOperand16::BC,
            0b01 => OpcodeOperand16::DE,
            0b10 => OpcodeOperand16::HL,
            0b11 => OpcodeOperand16::SP,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum OpcodeOperand8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    RefBC,
    RefDE,
    RefHL,
    RefHLInc,
    RefHLDec,
    RefImmAddr(u16),
    Imm(u8),
    RefUpperImmOffset(u8),
    RefUpperCOffset,
}

impl fmt::Display for OpcodeOperand8 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &OpcodeOperand8::A => write!(f, "a"),
            &OpcodeOperand8::B => write!(f, "b"),
            &OpcodeOperand8::C => write!(f, "c"),
            &OpcodeOperand8::D => write!(f, "d"),
            &OpcodeOperand8::E => write!(f, "e"),
            &OpcodeOperand8::H => write!(f, "h"),
            &OpcodeOperand8::L => write!(f, "l"),
            &OpcodeOperand8::RefBC=> write!(f, "(bc)"),
            &OpcodeOperand8::RefDE=> write!(f, "(de)"),
            &OpcodeOperand8::RefHL=> write!(f, "(hl)"),
            &OpcodeOperand8::RefHLInc=> write!(f, "(hl+)"),
            &OpcodeOperand8::RefHLDec=> write!(f, "(hl-)"),
            &OpcodeOperand8::RefImmAddr(addr) => write!(f, "(${:04x})", addr),
            &OpcodeOperand8::Imm(value) => write!(f, "${:02x}", value),
            &OpcodeOperand8::RefUpperImmOffset(offset) => write!(f, "($ff00 + ${:02x})", offset),
            &OpcodeOperand8::RefUpperCOffset => write!(f, "(0xff00 + c)")
        }
    }
}

impl OpcodeOperand8 {
    fn from_3bit_encoding(code: u8, offset: usize) -> OpcodeOperand8 {
        match (code >> offset) & 0x7 {
            0b000 => OpcodeOperand8::B,
            0b001 => OpcodeOperand8::C,
            0b010 => OpcodeOperand8::D,
            0b011 => OpcodeOperand8::E,
            0b100 => OpcodeOperand8::H,
            0b101 => OpcodeOperand8::L,
            0b110 => OpcodeOperand8::RefHL,
            0b111 => OpcodeOperand8::A,
            _ => unreachable!(),
        }
    }

    fn from_2bit_deref_encoding(code: u8, offset: usize) -> OpcodeOperand8 {
        match (code >> offset) & 0x3 {
            0b00 => OpcodeOperand8::RefBC,
            0b01 => OpcodeOperand8::RefDE,
            0b10 => OpcodeOperand8::RefHLInc,
            0b11 => OpcodeOperand8::RefHLDec,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum OpcodeRegister16 {
    BC,
    DE,
    HL,
}

#[derive(Debug, Clone, Copy)]
pub enum OpcodeCondition {
    None,
    Z,
    Nz,
    C,
    Nc,
}

impl fmt::Display for OpcodeCondition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            OpcodeCondition::Z => write!(f, "z"),
            OpcodeCondition::Nz => write!(f, "nz"),
            OpcodeCondition::C => write!(f, "c"),
            OpcodeCondition::Nc => write!(f, "nc"),
            OpcodeCondition::None => write!(f, "")
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    // 00
    Nop,
    Rra,
    Rla,
    Inc(OpcodeOperand8),
    Dec(OpcodeOperand8),
    Rlca,
    Rrca,
    Reti,
    Inc16(OpcodeOperand16),
    Dec16(OpcodeOperand16),
    Daa,
    Cpl,
    Halt,
    Push(OpcodeOperand16),
    Pop(OpcodeOperand16),
    Jr(OpcodeCondition, i8),
    Scf,
    Ccf,
    // 3E LD A,n
    Ld(u16, OpcodeOperand8, OpcodeOperand8),
    Ld16(u16, OpcodeOperand16, OpcodeOperand16),
    // A
    // C3 JP nn
    Jp(u16, OpcodeCondition, OpcodeOperand16),
    // F3 DI
    Di,
    Ei,
    Cp(u16, OpcodeOperand8),
    Adc(u16, OpcodeOperand8),
    Add(u16, OpcodeOperand8),
    Add16(OpcodeOperand16),
    AddSp(i8),
    Sub(u16, OpcodeOperand8),
    Sbc(u16, OpcodeOperand8),
    And(u16, OpcodeOperand8),
    Or(u16, OpcodeOperand8),
    Xor(u16, OpcodeOperand8),
    Call(OpcodeCondition, u16),
    Ret(OpcodeCondition),
    Rst(u16),

    // 0xCB prefix
    Rlc(OpcodeOperand8),
    Rl(OpcodeOperand8),
    Rrc(OpcodeOperand8),
    Rr(OpcodeOperand8),
    Sla(OpcodeOperand8),
    Sra(OpcodeOperand8),
    Swap(OpcodeOperand8),
    Srl(OpcodeOperand8),
    Bit(OpcodeOperand8, u8),
    Res(OpcodeOperand8, u8),
    Set(OpcodeOperand8, u8),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Instruction::Nop => write!(f, "nop"),
            Instruction::Dec(op) => write!(f, "dec {}", op),
            Instruction::Inc(op) => write!(f, "inc {}", op),
            Instruction::And(_, op) => write!(f, "and {}", op),
            Instruction::Xor(_, op) => write!(f, "xor {}", op),
            Instruction::Or(_, op) => write!(f, "or {}", op),
            Instruction::Cp(_, op) => write!(f, "cp {}", op),
            Instruction::Sub(_, op) => write!(f, "sub {}", op),
            Instruction::Add(_, op) => write!(f, "add a,{}", op),
            Instruction::Adc(_, op) => write!(f, "adc a,{}", op),
            Instruction::Sbc(_, op) => write!(f, "sbc a,{}", op),
            Instruction::Add16(op) => write!(f, "add hl,{}", op),
            Instruction::Push(op) => write!(f, "push {}", op),
            Instruction::Pop(op) => write!(f, "pop {}", op),
            Instruction::Ld(_, dst, src) => write!(f, "ld {}, {}", dst, src),
            Instruction::Ld16(_, dst, src) => write!(f, "ld {}, {}", dst, src),
            Instruction::Jr(cond, offset) => {
                match cond {
                    OpcodeCondition::None => write!(f, "jr {:+}", offset),
                    _ => write!(f, "jr {}, {:+}", cond, offset)
                }
            }
            Instruction::Ret(cond) => {
                match cond {
                    OpcodeCondition::None => write!(f, "ret"),
                    _ => write!(f, "ret {}", cond)
                }
            }
            Instruction::Call(cond, addr) => {
                match cond {
                    OpcodeCondition::None => write!(f, "call ${:04x}", addr),
                    _ => write!(f, "call {}, ${:04x}", cond, addr)
                }
            }
            Instruction::Jp(_, cond, op) => {
                match cond {
                    OpcodeCondition::None => write!(f, "jp {}", op),
                    _ => write!(f, "jp {}, {}", cond, op)
                }
            }
            Instruction::Inc16(op) => write!(f, "inc {}", op),
            Instruction::Dec16(op) => write!(f, "dec {}", op),
            Instruction::Daa => write!(f, "daa"),
            Instruction::AddSp(value) => write!(f, "add sp,${:02x}", value),
            Instruction::Cpl => write!(f, "cpl"),
            Instruction::Rst(addr) => write!(f, "rst ${:02x}", addr),
            Instruction::Di => write!(f, "di"),
            Instruction::Ei => write!(f, "ei"),
            Instruction::Scf => write!(f, "scf"),
            Instruction::Ccf => write!(f, "ccf"),
            Instruction::Halt => write!(f, "halt"),
            Instruction::Rra => write!(f, "rra"),
            Instruction::Rla => write!(f, "rla"),
            Instruction::Rlca => write!(f, "rlca"),
            Instruction::Rrca => write!(f, "rrca"),
            Instruction::Reti => write!(f, "reti"),
            Instruction::Rlc(op) => write!(f, "rlc {}", op),
            Instruction::Rrc(op) => write!(f, "rrc {}", op),
            Instruction::Rl(op) => write!(f, "rl {}", op),
            Instruction::Rr(op) => write!(f, "rr {}", op),
            Instruction::Sla(op) => write!(f, "sla {}", op),
            Instruction::Sra(op) => write!(f, "sra {}", op),
            Instruction::Srl(op) => write!(f, "srl {}", op),
            Instruction::Swap(op) => write!(f, "swap {}", op),
            Instruction::Bit(op, bit_offset) => write!(f, "bit {}, {}", bit_offset, op),
            Instruction::Set(op, bit_offset) => write!(f, "set {}, {}", bit_offset, op),
            Instruction::Res(op, bit_offset) => write!(f, "res {}, {}", bit_offset, op),

            // inst => write!(f, "{:?}", inst)
        }
    }
}

impl Instruction {
    pub fn length(&self) -> usize {
        match *self {
            Instruction::Nop => 1usize,
            Instruction::Rra => 1usize,
            Instruction::Rla => 1usize,
            Instruction::Rlca => 1usize,
            Instruction::Rrca => 1usize,
            Instruction::Reti => 1usize,
            Instruction::Halt => 1usize,
            Instruction::Inc(_) => 1usize,
            Instruction::Dec(_) => 1usize,
            Instruction::Inc16(_) => 1usize,
            Instruction::Dec16(_) => 1usize,
            Instruction::Daa => 1usize,
            Instruction::Cpl => 1usize,
            Instruction::Push(_) => 1usize,
            Instruction::Pop(_) => 1usize,
            Instruction::Jr(_, _) => 2usize,
            Instruction::Ld(size, _, _) => size as usize,
            Instruction::Ld16(size, _, _) => size as usize,
            Instruction::Jp(size, _, _) => size as usize,
            Instruction::Di => 1usize,
            Instruction::Ei => 1usize,
            Instruction::Adc(size, _) => size as usize,
            Instruction::Sbc(size, _) => size as usize,
            Instruction::Add(size, _) => size as usize,
            Instruction::Add16(_) => 1usize,
            Instruction::AddSp(_) => 2usize,
            Instruction::Xor(size, _) => size as usize,
            Instruction::And(size, _) => size as usize,
            Instruction::Or(size, _) => size as usize,
            Instruction::Sub(size, _) => size as usize,
            Instruction::Scf => 1usize,
            Instruction::Ccf => 1usize,
            Instruction::Call(_, _) => 3usize,
            Instruction::Ret(_) => 1usize,
            Instruction::Rst(_) => 1usize,
            Instruction::Cp(size, _) => size as usize,

            // 0xCB prefix (all has size 2)
            Instruction::Rlc(_) => 2usize,
            Instruction::Rrc(_) => 2usize,
            Instruction::Rr(_) => 2usize,
            Instruction::Sla(_) => 2usize,
            Instruction::Sra(_) => 2usize,
            Instruction::Rl(_) => 2usize,
            Instruction::Swap(_) => 2usize,
            Instruction::Srl(_) => 2usize,
            Instruction::Bit(_, _) => 2usize,
            Instruction::Res(_, _) => 2usize,
            Instruction::Set(_, _) => 2usize,
        }
    }

    pub fn decode(addr: u16, bus: &Bus) -> Result<(Instruction, u8), String> {
        let opcode = bus.read(addr);
        let timing = if opcode == 0xcb {
            let cb_code = bus.read(addr + 1);
            trace!("Decoding {:02X} {:02X} at {:04X}", opcode, cb_code, addr);
            CB_TIMINGS[cb_code as usize] * 4
        } else {
            trace!("Decoding {:02X} at {:04X}", opcode, addr);
            INST_TIMINGS[opcode as usize] * 4
        };
        match opcode {
                // LD r,r`
                // c if c & 0b1100_0000u8 == 0b0100_0000 {
                // let dst = OpcodeRegister::decode(c, 3);
                // let src =
                // }
                0x00 => Ok(Instruction::Nop),
                0x07 => Ok(Instruction::Rlca),
                0x08 => {
                    Ok(Instruction::Ld16(3,
                                         OpcodeOperand16::RefImmAddr(bus.read16(addr + 1)),
                                         OpcodeOperand16::SP))
                }
                0x0f => Ok(Instruction::Rrca),
                0x17 => Ok(Instruction::Rla),
                0x18 => Ok(Instruction::Jr(OpcodeCondition::None, bus.read(addr + 1) as i8)),
                0x1f => Ok(Instruction::Rra),
                0x20 => Ok(Instruction::Jr(OpcodeCondition::Nz, bus.read(addr + 1) as i8)),
                0x27 => Ok(Instruction::Daa),
                0x28 => Ok(Instruction::Jr(OpcodeCondition::Z, bus.read(addr + 1) as i8)),
                0x2f => Ok(Instruction::Cpl),
                0x30 => Ok(Instruction::Jr(OpcodeCondition::Nc, bus.read(addr + 1) as i8)),
                0x37 => Ok(Instruction::Scf),
                0x38 => Ok(Instruction::Jr(OpcodeCondition::C, bus.read(addr + 1) as i8)),
                0x3f => Ok(Instruction::Ccf),
                0x76 => Ok(Instruction::Halt),
                0xc0 => Ok(Instruction::Ret(OpcodeCondition::Nz)),
                0xc1 => Ok(Instruction::Pop(OpcodeOperand16::BC)),
                0xc2 => {
                    Ok(Instruction::Jp(3,
                                       OpcodeCondition::Nz,
                                       OpcodeOperand16::Imm(bus.read16(addr + 1))))
                }
                0xc3 => {
                    Ok(Instruction::Jp(3,
                                       OpcodeCondition::None,
                                       OpcodeOperand16::Imm(bus.read16(addr + 1))))
                }
                0xc5 => Ok(Instruction::Push(OpcodeOperand16::BC)),
                0xc4 => Ok(Instruction::Call(OpcodeCondition::Nz, bus.read16(addr + 1))),
                0xc6 => Ok(Instruction::Add(2, OpcodeOperand8::Imm(bus.read(addr + 1)))),
                0xc8 => Ok(Instruction::Ret(OpcodeCondition::Z)),
                0xc9 => Ok(Instruction::Ret(OpcodeCondition::None)),
                0xca => {
                    Ok(Instruction::Jp(3,
                                       OpcodeCondition::Z,
                                       OpcodeOperand16::Imm(bus.read16(addr + 1))))
                }
                0xcb => {
                    let cb_code = bus.read(addr + 1);
                    let operand = OpcodeOperand8::from_3bit_encoding(cb_code, 0);
                    if cb_code < 0x40 {
                        match cb_code & 0b11111_000 {
                            0x00 => Ok(Instruction::Rlc(operand)),
                            0x08 => Ok(Instruction::Rrc(operand)),
                            0x10 => Ok(Instruction::Rl(operand)),
                            0x18 => Ok(Instruction::Rr(operand)),
                            0x20 => Ok(Instruction::Sla(operand)),
                            0x28 => Ok(Instruction::Sra(operand)),
                            0x30 => Ok(Instruction::Swap(operand)),
                            0x38 => Ok(Instruction::Srl(operand)),
                            _ => panic!("Have not implemented CB {:02X}", cb_code),
                        }
                    } else {
                        let bit_offset = (cb_code >> 3) & 0b111;
                        match cb_code & 0b11_000_000 {
                            0b01_000_000 => Ok(Instruction::Bit(operand, bit_offset)),
                            0b10_000_000 => Ok(Instruction::Res(operand, bit_offset)),
                            0b11_000_000 => Ok(Instruction::Set(operand, bit_offset)),
                            _ => unreachable!(),
                        }
                    }
                }
                0xcc => Ok(Instruction::Call(OpcodeCondition::Z, bus.read16(addr + 1))),
                0xcd => Ok(Instruction::Call(OpcodeCondition::None, bus.read16(addr + 1))),
                0xce => Ok(Instruction::Adc(2, OpcodeOperand8::Imm(bus.read(addr + 1)))),
                0xd0 => Ok(Instruction::Ret(OpcodeCondition::Nc)),
                0xd1 => Ok(Instruction::Pop(OpcodeOperand16::DE)),
                0xd2 => {
                    Ok(Instruction::Jp(3,
                                       OpcodeCondition::Nc,
                                       OpcodeOperand16::Imm(bus.read16(addr + 1))))
                }
                0xd4 => Ok(Instruction::Call(OpcodeCondition::Nc, bus.read16(addr + 1))),
                0xd5 => Ok(Instruction::Push(OpcodeOperand16::DE)),
                0xd6 => Ok(Instruction::Sub(2, OpcodeOperand8::Imm(bus.read(addr + 1)))),
                0xd8 => Ok(Instruction::Ret(OpcodeCondition::C)),
                0xd9 => Ok(Instruction::Reti),
                0xda => {
                    Ok(Instruction::Jp(3,
                                       OpcodeCondition::C,
                                       OpcodeOperand16::Imm(bus.read16(addr + 1))))
                }
                0xdc => Ok(Instruction::Call(OpcodeCondition::C, bus.read16(addr + 1))),
                0xde => Ok(Instruction::Sbc(2, OpcodeOperand8::Imm(bus.read(addr + 1)))),
                0xe0 => {
                    Ok(Instruction::Ld(2,
                                       OpcodeOperand8::RefUpperImmOffset(bus.read(addr + 1)),
                                       OpcodeOperand8::A))
                }
                0xe1 => Ok(Instruction::Pop(OpcodeOperand16::HL)),
                0xe2 => Ok(Instruction::Ld(1, OpcodeOperand8::RefUpperCOffset, OpcodeOperand8::A)),
                0xe5 => Ok(Instruction::Push(OpcodeOperand16::HL)),
                0xe6 => Ok(Instruction::And(2, OpcodeOperand8::Imm(bus.read(addr + 1)))),
                0xe8 => Ok(Instruction::AddSp(bus.read(addr + 1) as i8)),
                0xe9 => Ok(Instruction::Jp(1, OpcodeCondition::None, OpcodeOperand16::HL)),
                0xea => {
                    Ok(Instruction::Ld(3,
                                       OpcodeOperand8::RefImmAddr(bus.read16(addr + 1)),
                                       OpcodeOperand8::A))
                }
                0xee => Ok(Instruction::Xor(2, OpcodeOperand8::Imm(bus.read(addr + 1)))),
                0xf0 => {
                    Ok(Instruction::Ld(2,
                                       OpcodeOperand8::A,
                                       OpcodeOperand8::RefUpperImmOffset(bus.read(addr + 1))))
                }
                0xf1 => Ok(Instruction::Pop(OpcodeOperand16::AF)),
                0xf2 => Ok(Instruction::Ld(1, OpcodeOperand8::A, OpcodeOperand8::RefUpperCOffset)),
                0xf3 => Ok(Instruction::Di),
                0xf5 => Ok(Instruction::Push(OpcodeOperand16::AF)),
                0xf6 => Ok(Instruction::Or(2, OpcodeOperand8::Imm(bus.read(addr + 1)))),
                0xf8 => {
                    Ok(Instruction::Ld16(2,
                                         OpcodeOperand16::HL,
                                         OpcodeOperand16::SPPlusImm(bus.read(addr + 1) as i8)))
                }
                0xf9 => Ok(Instruction::Ld16(1, OpcodeOperand16::SP, OpcodeOperand16::HL)),
                0xfa => {
                    Ok(Instruction::Ld(3,
                                       OpcodeOperand8::A,
                                       OpcodeOperand8::RefImmAddr(bus.read16(addr + 1))))
                }
                0xfb => Ok(Instruction::Ei),
                0xfe => Ok(Instruction::Cp(2, OpcodeOperand8::Imm(bus.read(addr + 1)))),
                c if c & 0b11_00_1111 == 0b00_00_1001 => {
                    // TODO: 1 opcode 8 cycles
                    let src = OpcodeOperand16::from_2bit_encoding(c, 4);
                    Ok(Instruction::Add16(src))
                }
                c if c & 0b11_000_111 == 0b11_000_111 => {
                    let addr = (c & 0b00_111_000) as u16;
                    Ok(Instruction::Rst(addr))
                }
                c if c & 0b11_000_000 == 0b10_000_000 => {
                    let operand = OpcodeOperand8::from_3bit_encoding(c, 0);
                    match c & 0b11_111_000 {
                        0x80 => Ok(Instruction::Add(1, operand)),
                        0x88 => Ok(Instruction::Adc(1, operand)),
                        0x90 => Ok(Instruction::Sub(1, operand)),
                        0x98 => Ok(Instruction::Sbc(1, operand)),
                        0xa0 => Ok(Instruction::And(1, operand)),
                        0xb0 => Ok(Instruction::Or(1, operand)),
                        0xb8 => Ok(Instruction::Cp(1, operand)),
                        0xa8 => Ok(Instruction::Xor(1, operand)),
                        _ => Err(format!("Unrecognized opcode 0x{:02X} {:?}", opcode, operand)),
                    }
                }
                c if c & 0b11_00_1111 == 0b00_00_0011 => {
                    let operand = OpcodeOperand16::from_2bit_encoding(c, 4);
                    Ok(Instruction::Inc16(operand))
                }
                c if c & 0b11_00_1111 == 0b00_00_1011 => {
                    let operand = OpcodeOperand16::from_2bit_encoding(c, 4);
                    Ok(Instruction::Dec16(operand))
                }
                c if c & 0b11_000_111 == 0b00_000_100 => {
                    let operand = OpcodeOperand8::from_3bit_encoding(c, 3);
                    Ok(Instruction::Inc(operand))
                }
                c if c & 0b11_000_111 == 0b00_000_101 => {
                    let operand = OpcodeOperand8::from_3bit_encoding(c, 3);
                    Ok(Instruction::Dec(operand))
                }
                c if c & 0b11_00_1111 == 0b00_00_1010 => {
                    let dst = OpcodeOperand8::A;
                    let src = OpcodeOperand8::from_2bit_deref_encoding(c, 4);
                    Ok(Instruction::Ld(1, dst, src))
                }
                c if c & 0b11_00_1111 == 0b00_00_0010 => {
                    let dst = OpcodeOperand8::from_2bit_deref_encoding(c, 4);
                    let src = OpcodeOperand8::A;
                    Ok(Instruction::Ld(1, dst, src))
                }
                c if c & 0b11_000_000 == 0b01_000_000 => {
                    // TODO: This should not match ld reg, (hl) and ld (hl), reg
                    let dst = OpcodeOperand8::from_3bit_encoding(c, 3);
                    let src = OpcodeOperand8::from_3bit_encoding(c, 0);
                    Ok(Instruction::Ld(1, dst, src))
                }
                c if c & 0b11_00_1111 == 0b00_00_0001 => {
                    let dst = OpcodeOperand16::from_2bit_encoding(c, 4);
                    let imm = bus.read16(addr + 1);
                    Ok(Instruction::Ld16(3, dst, OpcodeOperand16::Imm(imm)))
                }
                c if c & 0b11_000_111 == 0b00_000_110 => {
                    let dst = OpcodeOperand8::from_3bit_encoding(c, 3);
                    let imm = bus.read(addr + 1);
                    Ok(Instruction::Ld(2, dst, OpcodeOperand8::Imm(imm)))
                }
                _ => Err(format!("Unrecognized opcode 0x{:02X}", opcode)),
            }
            .map(|r| (r, timing))
    }
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct Cpu {
    // IME
    // TODO: Expose from function instead
    pub cycles: u64,
    pub instruction_counter: u64,
    interrupts_enabled: bool,
    halted: bool,
    pub regs: Regs,
    pub bus: Bus,
}

static INST_TIMINGS: [u8; 256] =
    [1, 3, 2, 2, 1, 1, 2, 1, 5, 2, 2, 2, 1, 1, 2, 1, 0, 3, 2, 2, 1, 1, 2, 1, 3, 2, 2, 2, 1, 1, 2,
     1, 2, 3, 2, 2, 1, 1, 2, 1, 2, 2, 2, 2, 1, 1, 2, 1, 2, 3, 2, 2, 3, 3, 3, 1, 2, 2, 2, 2, 1, 1,
     2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1,
     1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 0, 2, 1, 1, 1, 1,
     1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1,
     1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1,
     1, 1, 1, 1, 2, 1, 2, 3, 3, 4, 3, 4, 2, 4, 2, 4, 3, 0, 3, 6, 2, 4, 2, 3, 3, 0, 3, 4, 2, 4, 2,
     4, 3, 0, 3, 0, 2, 4, 3, 3, 2, 0, 0, 4, 2, 4, 4, 1, 4, 0, 0, 0, 2, 4, 3, 3, 2, 1, 0, 4, 2, 4,
     3, 2, 4, 1, 0, 0, 2, 4];

static CB_TIMINGS: [u8; 256] =
    [2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4,
     2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2,
     4, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2,
     2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2,
     2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2,
     2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2,
     2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2,
     2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2,
     2, 2, 2, 2, 2, 2, 4, 2];

impl Cpu {
    pub fn new(rom_file: &str) -> Cpu {
        Cpu {
            cycles: 0,
            instruction_counter: 0,
            bus: Bus::new(Cartridge::load(rom_file).unwrap()),
            regs: Regs::new(),
            interrupts_enabled: true,
            halted: false,
        }
    }

    pub fn reset(&mut self) {
        println!("Resetting..");
        self.cycles = 0;
        self.instruction_counter = 0;
        let cart = self.bus.cartridge.clone();
        self.bus = Bus::new(cart);
        self.regs = Regs::new();
        self.interrupts_enabled = false;
        self.halted = false;
        println!("Resetting done.");
    }

    fn pc_inc(&mut self, len: u16) {
        self.regs.pc = self.regs.pc.wrapping_add(len);
    }

    fn add_cycles(&mut self, count: u64) {
        self.cycles += count;
    }

    fn write_16bit_operand(&mut self, reg: &OpcodeOperand16, value: u16) {
        match *reg {
            OpcodeOperand16::AF => {
                self.regs.a = (value >> 8) as u8;
                self.regs.f = (value as u8) & 0xf0;
            }
            OpcodeOperand16::BC => {
                self.regs.b = (value >> 8) as u8;
                self.regs.c = value as u8;
            }
            OpcodeOperand16::DE => {
                self.regs.d = (value >> 8) as u8;
                self.regs.e = value as u8;
            }
            OpcodeOperand16::HL => {
                self.regs.h = (value >> 8) as u8;
                self.regs.l = value as u8;
            }
            OpcodeOperand16::SP => {
                self.regs.sp = value;
            }
            OpcodeOperand16::RefImmAddr(addr) => {
                self.bus.write16(addr, value);
            }
            OpcodeOperand16::Imm(_) => panic!("Doh, can't write to immediate"),
            OpcodeOperand16::SPPlusImm(_) => panic!("Doh, can't write to immediate"),
        }
    }

    fn read_16bit_operand(&mut self, reg: &OpcodeOperand16) -> u16 {
        match *reg {
            OpcodeOperand16::AF => ((self.regs.a as u16) << 8) | self.regs.f as u16,
            OpcodeOperand16::BC => ((self.regs.b as u16) << 8) | self.regs.c as u16,
            OpcodeOperand16::DE => ((self.regs.d as u16) << 8) | self.regs.e as u16,
            OpcodeOperand16::HL => ((self.regs.h as u16) << 8) | self.regs.l as u16,
            OpcodeOperand16::SP => self.regs.sp,
            OpcodeOperand16::RefImmAddr(addr) => self.bus.read16(addr),
            OpcodeOperand16::Imm(imm) => imm,
            OpcodeOperand16::SPPlusImm(imm) => {
                let sp = self.regs.sp as usize;
                let result = (sp as isize).wrapping_add(imm as isize) as u16;
                self.regs.f = if ((imm as usize) & 0xf) + (sp & 0xf) > 0xf {
                    CPU_FLAG_H
                } else {
                    0
                } |
                              if ((imm as usize) & 0xff) + (sp & 0xff) > 0xff {
                    CPU_FLAG_C
                } else {
                    0
                };
                result as u16
            }
        }
    }

    fn load(&mut self, addr: u16) -> u8 {
        let value = self.bus.read(addr);
        value
    }

    fn store(&mut self, addr: u16, value: u8) {
        let value = self.bus.write(addr, value);
        value
    }


    fn read_8bit_operand(&mut self, operand: &OpcodeOperand8) -> u8 {
        match *operand {
            OpcodeOperand8::A => self.regs.a,
            OpcodeOperand8::B => self.regs.b,
            OpcodeOperand8::C => self.regs.c,
            OpcodeOperand8::D => self.regs.d,
            OpcodeOperand8::E => self.regs.e,
            OpcodeOperand8::H => self.regs.h,
            OpcodeOperand8::L => self.regs.l,
            OpcodeOperand8::Imm(imm) => imm,
            OpcodeOperand8::RefBC => {
                let addr = self.regs.read_reg16(OpcodeRegister16::BC);
                self.load(addr)
            }
            OpcodeOperand8::RefDE => {
                let addr = self.regs.read_reg16(OpcodeRegister16::DE);
                self.load(addr)
            }
            OpcodeOperand8::RefHLInc => {
                let addr = self.regs.read_reg16(OpcodeRegister16::HL);
                let value = self.load(addr);
                self.regs.write_reg16(OpcodeRegister16::HL, addr.wrapping_add(1));
                value
            }
            OpcodeOperand8::RefHLDec => {
                let addr = self.regs.read_reg16(OpcodeRegister16::HL);
                let value = self.load(addr);
                self.regs.write_reg16(OpcodeRegister16::HL, addr.wrapping_sub(1));
                value
            }
            OpcodeOperand8::RefUpperImmOffset(offset) => self.load(0xff00 + (offset as u16)),
            OpcodeOperand8::RefUpperCOffset => {
                let addr = 0xff00 + (self.regs.c as u16);
                self.load(addr)
            }
            OpcodeOperand8::RefImmAddr(addr) => self.load(addr),
            OpcodeOperand8::RefHL => {
                let addr = self.regs.read_reg16(OpcodeRegister16::HL);
                self.load(addr)
            }
        }
    }
    fn write_8bit_operand(&mut self, operand: &OpcodeOperand8, value: u8) {
        match *operand {
            OpcodeOperand8::A => self.regs.a = value,
            OpcodeOperand8::B => self.regs.b = value,
            OpcodeOperand8::C => self.regs.c = value,
            OpcodeOperand8::D => self.regs.d = value,
            OpcodeOperand8::E => self.regs.e = value,
            OpcodeOperand8::H => self.regs.h = value,
            OpcodeOperand8::L => self.regs.l = value,
            OpcodeOperand8::Imm(_) => panic!("Doh, can't write to immediate"),
            OpcodeOperand8::RefBC => {
                let addr = self.regs.read_reg16(OpcodeRegister16::BC);
                self.store(addr, value)
            }
            OpcodeOperand8::RefDE => {
                let addr = self.regs.read_reg16(OpcodeRegister16::DE);
                self.store(addr, value)
            }
            OpcodeOperand8::RefHLInc => {
                let hl = self.regs.read_reg16(OpcodeRegister16::HL);
                self.store(hl, value);
                self.regs.write_reg16(OpcodeRegister16::HL, hl.wrapping_add(1));
            }
            OpcodeOperand8::RefHLDec => {
                let hl = self.regs.read_reg16(OpcodeRegister16::HL);
                self.store(hl, value);
                self.regs.write_reg16(OpcodeRegister16::HL, hl.wrapping_sub(1));
            }
            OpcodeOperand8::RefUpperImmOffset(offset) => {
                self.store(0xff00 + (offset as u16), value)
            }
            OpcodeOperand8::RefUpperCOffset => {
                let addr = 0xff00 + (self.regs.c as u16);
                self.store(addr, value)
            }
            OpcodeOperand8::RefImmAddr(addr) => self.store(addr, value),
            OpcodeOperand8::RefHL => {
                let addr = self.regs.read_reg16(OpcodeRegister16::HL);
                self.store(addr, value)
            }
        }
    }

    fn push(&mut self, value: u16) {
        let sp = self.regs.sp.wrapping_sub(2);
        self.bus.write16(sp, value);
        self.regs.sp = sp;
    }

    fn pop(&mut self) -> u16 {
        let sp = self.regs.sp;
        self.regs.sp = sp.wrapping_add(2);
        self.bus.read16(sp)
    }

    fn sub(lhs: u8, rhs: u8) -> (u8, u8) {
        let (res, overflow) = lhs.overflowing_sub(rhs);
        (res,
         (if overflow { CPU_FLAG_C } else { 0 }) | (if res == 0 { CPU_FLAG_Z } else { 0 }) |
         CPU_FLAG_N |
         (if (rhs & 0xf) > (lhs & 0xf) {
             CPU_FLAG_H
         } else {
             0
         }))
    }

    fn check_flag(&self, cond: OpcodeCondition) -> bool {
        match cond {
            OpcodeCondition::None => true,
            OpcodeCondition::C => self.regs.f & CPU_FLAG_C != 0,
            OpcodeCondition::Nc => self.regs.f & CPU_FLAG_C == 0,
            OpcodeCondition::Z => self.regs.f & CPU_FLAG_Z != 0,
            OpcodeCondition::Nz => self.regs.f & CPU_FLAG_Z == 0,
        }
    }

    fn execute_instruction(&mut self, instruction: Instruction) -> Result<(), &'static str> {
        match instruction {
            Instruction::Nop => Ok(()),
            Instruction::Rlca => {
                let a = self.regs.a;
                self.regs.f = if (a & 0x80) == 0x80 { CPU_FLAG_C } else { 0 };
                self.regs.a = a.rotate_left(1);
                Ok(())
            }
            Instruction::Rrca => {
                let a = self.regs.a;
                self.regs.f = if (a & 0x1) == 0x1 { CPU_FLAG_C } else { 0 };
                self.regs.a = a.rotate_right(1);
                Ok(())
            }
            Instruction::Call(cond, addr) => {
                if self.check_flag(cond) {
                    let saved_pc = self.regs.pc;
                    self.push(saved_pc);
                    self.regs.pc = addr;
                    match cond {
                        OpcodeCondition::None => {}
                        _ => {
                            self.add_cycles(12);
                        }
                    }
                }
                Ok(())
            }
            Instruction::Rst(addr) => {
                let saved_pc = self.regs.pc;
                self.push(saved_pc);
                self.regs.pc = addr;
                Ok(())
            }
            Instruction::Daa => {
                // bcd correction, whatever that entails
                let f = self.regs.f;
                let mut a = self.regs.a as u32;
                let mut carry_set = false;

                if (f & CPU_FLAG_N) == 0 {
                    if (a & 0xf) > 0x09 || (f & CPU_FLAG_H) != 0 {
                        a = a.wrapping_add(6);
                    }
                    if (a > 0x9f) || (f & CPU_FLAG_C) != 0 {
                        a = a.wrapping_add(0x60);
                        carry_set = true;
                    }
                } else {
                    if (f & CPU_FLAG_H) != 0 {
                        a = a.wrapping_sub(6);
                    }
                    if (f & CPU_FLAG_C) != 0 {
                        a = a.wrapping_sub(0x60);
                        carry_set = true;
                    }
                }

                self.regs.f = if a & 0xff == 0 { CPU_FLAG_Z } else { 0 } | (f & CPU_FLAG_N) |
                              if carry_set { CPU_FLAG_C } else { 0 } |
                              0; // ((self.regs.a ^ a) & CPU_FLAG_H);

                self.regs.a = a as u8;

                Ok(())
            }
            Instruction::Cpl => {
                self.regs.a = !self.regs.a;
                self.regs.f = (self.regs.f & (CPU_FLAG_Z | CPU_FLAG_C)) | CPU_FLAG_N | CPU_FLAG_H;
                Ok(())
            }
            Instruction::Halt => {
                self.halted = true;
                Ok(())
            }
            Instruction::Ret(cond) => {
                if self.check_flag(cond) {
                    self.regs.pc = self.pop();
                    match cond {
                        OpcodeCondition::None => {}
                        _ => {
                            self.add_cycles(12);
                        }
                    }
                }
                Ok(())
            }
            Instruction::Reti => {
                self.regs.pc = self.pop();
                self.interrupts_enabled = true;
                debug!("Exiting interrupt");
                Ok(())
            }
            Instruction::Scf => {
                self.regs.f = CPU_FLAG_C | (self.regs.f & CPU_FLAG_Z);
                Ok(())
            }
            Instruction::Ccf => {
                let f = self.regs.f;
                self.regs.f = (f & CPU_FLAG_Z) | if (f & CPU_FLAG_C) != 0 { 0 } else { CPU_FLAG_C };
                Ok(())
            }
            Instruction::Inc(operand) => {
                let value = self.read_8bit_operand(&operand).wrapping_add(1);
                self.write_8bit_operand(&operand, value);
                self.regs.f = (self.regs.f & CPU_FLAG_C) | if value == 0 { CPU_FLAG_Z } else { 0 } |
                              if (value & 0xf) == 0 { CPU_FLAG_H } else { 0 };
                Ok(())
            }
            Instruction::Dec(operand) => {
                let value = self.read_8bit_operand(&operand).wrapping_sub(1);
                self.write_8bit_operand(&operand, value);
                self.regs.f = (self.regs.f & CPU_FLAG_C) | if value == 0 { CPU_FLAG_Z } else { 0 } |
                              if (value & 0xf) == 0xf { CPU_FLAG_H } else { 0 } |
                              CPU_FLAG_N;
                Ok(())
            }
            Instruction::Inc16(operand) => {
                let value = self.read_16bit_operand(&operand).wrapping_add(1);
                self.write_16bit_operand(&operand, value);
                Ok(())
            }
            Instruction::Dec16(operand) => {
                let value = self.read_16bit_operand(&operand).wrapping_sub(1);
                self.write_16bit_operand(&operand, value);
                Ok(())
            }
            Instruction::Ld(_, dst, src) => {
                let value = self.read_8bit_operand(&src);
                self.write_8bit_operand(&dst, value);
                Ok(())
            }
            Instruction::Ld16(_, dst, src) => {
                let value = self.read_16bit_operand(&src);
                self.write_16bit_operand(&dst, value);
                Ok(())
            }
            Instruction::Jr(cond, imm) => {
                if self.check_flag(cond) {
                    self.regs.pc = self.regs.pc.wrapping_add((imm as i16) as u16);
                    match cond {
                        OpcodeCondition::None => {}
                        _ => {
                            self.add_cycles(4);
                        }
                    }
                }
                Ok(())
            }
            Instruction::Jp(_, cond, operand) => {
                if self.check_flag(cond) {
                    let addr = self.read_16bit_operand(&operand);
                    self.regs.pc = addr;
                    match cond {
                        OpcodeCondition::None => {}
                        _ => {
                            self.add_cycles(4);
                        }
                    }
                }
                Ok(())
            }
            Instruction::Di => {
                self.interrupts_enabled = false;
                Ok(())
            }
            Instruction::Ei => {
                self.interrupts_enabled = true;
                Ok(())
            }
            Instruction::Adc(_, r) => {
                let c = if (self.regs.f & CPU_FLAG_C) == CPU_FLAG_C {
                    1
                } else {
                    0
                };
                let rhs = self.read_8bit_operand(&r) as usize;
                let lhs = self.regs.a as usize;
                let result = lhs + rhs + c;

                self.regs.f = if result > 0xff { CPU_FLAG_C } else { 0 } |
                              if (result as u8) == 0 { CPU_FLAG_Z } else { 0 } |
                              if (rhs & 0xf) + (lhs & 0xf) + c > 0xf {
                    CPU_FLAG_H
                } else {
                    0
                };

                self.regs.a = result as u8;
                Ok(())
            }
            Instruction::Sbc(_, r) => {
                let c = if (self.regs.f & CPU_FLAG_C) == CPU_FLAG_C {
                    1
                } else {
                    0
                };
                let rhs = self.read_8bit_operand(&r) as isize;
                let lhs = self.regs.a as isize;
                let result = lhs - (rhs + c);

                self.regs.f = if result < 0 { CPU_FLAG_C } else { 0 } |
                              if (result as u8) == 0 { CPU_FLAG_Z } else { 0 } |
                              if (lhs & 0xf) - ((rhs & 0xf) + c) < 0 {
                    CPU_FLAG_H
                } else {
                    0
                } | CPU_FLAG_N;

                self.regs.a = result as u8;
                Ok(())
            }
            Instruction::Add(_, r) => {
                let lhs = self.regs.a;
                let rhs = self.read_8bit_operand(&r);
                let (result, carry) = lhs.overflowing_add(rhs);
                self.regs.a = result;
                self.regs.f = if result == 0 { CPU_FLAG_Z } else { 0 } |
                              if carry { CPU_FLAG_C } else { 0 } |
                              if ((lhs & 0xf) + (rhs & 0xf)) > 0xf {
                    CPU_FLAG_H
                } else {
                    0
                };
                Ok(())
            }
            Instruction::Add16(operand) => {
                let dst = OpcodeOperand16::HL;
                let lhs = self.read_16bit_operand(&dst);
                let rhs = self.read_16bit_operand(&operand);

                let (result, carry) = lhs.overflowing_add(rhs);
                self.write_16bit_operand(&dst, result);
                // TODO: Unsure if half carry should be set
                self.regs.f = (self.regs.f & CPU_FLAG_Z) | if carry { CPU_FLAG_C } else { 0 } |
                              if ((lhs & 0xfff) + (rhs & 0xfff)) > 0xfff {
                    CPU_FLAG_H
                } else {
                    0
                };
                // TODO Find out if this is correct behavior
                Ok(())
            }
            Instruction::AddSp(imm) => {
                // Bwaaahh this got dirty
                let operand = OpcodeOperand16::SPPlusImm(imm);
                let result = self.read_16bit_operand(&operand);
                self.regs.sp = result;
                Ok(())
            }
            Instruction::And(_, r) => {
                self.regs.a = self.regs.a & self.read_8bit_operand(&r);
                self.regs.f = if self.regs.a == 0 { CPU_FLAG_Z } else { 0 } | CPU_FLAG_H;
                Ok(())
            }
            Instruction::Or(_, r) => {
                self.regs.a = self.regs.a | self.read_8bit_operand(&r);
                self.regs.f = if self.regs.a == 0 { CPU_FLAG_Z } else { 0 };
                Ok(())
            }
            Instruction::Xor(_, r) => {
                self.regs.a = self.regs.a ^ self.read_8bit_operand(&r);
                self.regs.f = if self.regs.a == 0 { CPU_FLAG_Z } else { 0 };
                Ok(())
            }
            Instruction::Sub(_, r) => {
                let lhs = self.regs.a;
                let rhs = self.read_8bit_operand(&r);
                let (a, f) = Cpu::sub(lhs, rhs);
                self.regs.f = f;
                self.regs.a = a;
                Ok(())
            }
            Instruction::Cp(_, operand) => {
                let rhs = self.read_8bit_operand(&operand);
                let (_, f) = Cpu::sub(self.regs.a, rhs);
                self.regs.f = f;
                Ok(())
                // f self.regs.a == imm { self.regs.f  }
            }
            Instruction::Push(operand) => {
                let value = self.read_16bit_operand(&operand);
                self.push(value);
                Ok(())
            }
            Instruction::Pop(operand) => {
                let value = self.pop();
                self.write_16bit_operand(&operand, value);
                Ok(())
            }
            Instruction::Rra => {
                let value = self.regs.a;
                let result = value >> 1 |
                             if self.regs.f & CPU_FLAG_C != 0 {
                    0x80
                } else {
                    0
                };
                self.regs.a = result;
                // Almost like RR but according to opcode docs does not affect the Z flag
                self.regs.f = if (value & 1) == 1 { CPU_FLAG_C } else { 0 };
                Ok(())
            }
            Instruction::Rla => {
                let value = self.regs.a;
                let result = value << 1 | if self.regs.f & CPU_FLAG_C != 0 { 1 } else { 0 };
                self.regs.a = result;
                self.regs.f = if (value & 0x80) == 0x80 {
                    CPU_FLAG_C
                } else {
                    0
                };
                Ok(())
            }
            Instruction::Rlc(operand) => {
                let result = self.read_8bit_operand(&operand).rotate_left(1);
                self.write_8bit_operand(&operand, result);
                self.regs.f = if result == 0 { CPU_FLAG_Z } else { 0 } |
                              if (result & 1) == 1 { CPU_FLAG_C } else { 0 };
                Ok(())
            }
            Instruction::Rrc(operand) => {
                let result = self.read_8bit_operand(&operand).rotate_right(1);
                self.write_8bit_operand(&operand, result);
                self.regs.f = if result == 0 { CPU_FLAG_Z } else { 0 } |
                              if (result & 0x80) == 0x80 {
                    CPU_FLAG_C
                } else {
                    0
                };
                Ok(())
            }
            Instruction::Rr(operand) => {
                let value = self.read_8bit_operand(&operand);
                let result = value >> 1 |
                             if self.regs.f & CPU_FLAG_C != 0 {
                    0x80
                } else {
                    0
                };
                self.write_8bit_operand(&operand, result);
                self.regs.f = if result == 0 { CPU_FLAG_Z } else { 0 } |
                              if (value & 1) == 1 { CPU_FLAG_C } else { 0 };
                Ok(())
            }
            Instruction::Rl(operand) => {
                let value = self.read_8bit_operand(&operand);
                let result = value << 1 | if self.regs.f & CPU_FLAG_C != 0 { 1 } else { 0 };
                self.write_8bit_operand(&operand, result);
                self.regs.f = if result == 0 { CPU_FLAG_Z } else { 0 } |
                              if (value & 0x80) == 0x80 {
                    CPU_FLAG_C
                } else {
                    0
                };
                Ok(())
            }
            Instruction::Sla(operand) => {
                let value = self.read_8bit_operand(&operand);
                let result = value << 1;
                self.write_8bit_operand(&operand, result);
                self.regs.f = if result == 0 { CPU_FLAG_Z } else { 0 } |
                              if (value & 0x80) == 0x80 {
                    CPU_FLAG_C
                } else {
                    0
                };
                Ok(())
            }
            Instruction::Sra(operand) => {
                let value = self.read_8bit_operand(&operand);
                let result = value >> 1 | (value & 0x80);
                self.write_8bit_operand(&operand, result);
                self.regs.f = if result == 0 { CPU_FLAG_Z } else { 0 } |
                              if (value & 1) == 1 { CPU_FLAG_C } else { 0 };
                Ok(())
            }
            Instruction::Swap(operand) => {
                let value = self.read_8bit_operand(&operand);
                let result = (value >> 4) | (value << 4);
                self.regs.f = if result == 0 { CPU_FLAG_Z } else { 0 };
                self.write_8bit_operand(&operand, result);
                Ok(())
            }
            Instruction::Srl(operand) => {
                let value = self.read_8bit_operand(&operand);
                let result = value >> 1;
                self.write_8bit_operand(&operand, result);
                self.regs.f = if result == 0 { CPU_FLAG_Z } else { 0 } |
                              if (value & 1) == 1 { CPU_FLAG_C } else { 0 };
                Ok(())
            }
            Instruction::Bit(operand, offset) => {
                let value = self.read_8bit_operand(&operand);
                self.regs.f = if (value & (1u8 << offset)) == 0 {
                    CPU_FLAG_Z
                } else {
                    0
                } | CPU_FLAG_H | (self.regs.f & CPU_FLAG_C);
                Ok(())
            }
            Instruction::Res(operand, offset) => {
                let value = self.read_8bit_operand(&operand) & !(1 << offset);
                self.write_8bit_operand(&operand, value);
                Ok(())
            }
            Instruction::Set(operand, offset) => {
                let value = self.read_8bit_operand(&operand) | (1 << offset);
                self.write_8bit_operand(&operand, value);
                Ok(())
            }
        }
    }

    pub fn step(&mut self) -> Result<(), &'static str> {
        let pre_cycles = self.cycles;

        let mut interrupt_called = false;
        if self.interrupts_enabled {
            // Maybe refactor out some of this code to Interrupts
            let mut ints = self.bus.io.interrupt_flags & self.bus.io.interrupt_enable;
            if ints != 0 {
                let mut int_number = 0;
                while ints != 0 && ints & 1 == 0 {
                    int_number += 1;
                    ints = ints >> 1;
                }
                self.interrupts_enabled = false;
                self.bus.io.interrupt_flags &= !(1u8 << int_number);
                let saved_pc = self.regs.pc;
                self.push(saved_pc);
                self.add_cycles(20);
                self.regs.pc = 0x40u16 + int_number * 8;
                interrupt_called = true;
            }
        }

        if !interrupt_called {
            if !self.halted {
                let (instruction, timing) = Instruction::decode(self.regs.pc, &self.bus)
                    .unwrap_or_else(|err| {
                        self.bus.dump_wram().unwrap();
                        println!("{}", err);
                        panic!()
                    });
                trace!("inst: {:?}", instruction);
                let inst_len = instruction.length();
                self.pc_inc(inst_len as u16);
                try!(self.execute_instruction(instruction));
                self.add_cycles(timing as u64);
                self.instruction_counter += 1;
            } else {
                self.add_cycles(4);
            }
        }

        let passed_cycles = (self.cycles - pre_cycles) as usize;
        let ints = self.bus.io.timers.tick(passed_cycles) | self.bus.lcd.tick(passed_cycles);
        self.bus.sound.tick(passed_cycles);

        if ints != 0 {
            self.bus.io.interrupt_flags |= ints;
            self.halted = false;
        }

        Ok(())
    }
}
