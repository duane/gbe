use color_eyre::Result;
use std::{fmt::Display, str::FromStr};

use crate::{
    bus::Bus,
    instruction::{Cond, Instruction, R16Mem, R16Stack, R16, R8},
};

pub enum RegRef {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

impl FromStr for RegRef {
    type Err = &'static str;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        match s.to_uppercase().as_str() {
            "A" => Ok(RegRef::A),
            "B" => Ok(RegRef::B),
            "C" => Ok(RegRef::C),
            "D" => Ok(RegRef::D),
            "E" => Ok(RegRef::E),
            "H" => Ok(RegRef::H),
            "L" => Ok(RegRef::L),
            "AF" => Ok(RegRef::AF),
            "BC" => Ok(RegRef::BC),
            "DE" => Ok(RegRef::DE),
            "HL" => Ok(RegRef::HL),
            "SP" => Ok(RegRef::SP),
            "PC" => Ok(RegRef::PC),
            _ => Err("Invalid register"),
        }
    }
}

#[derive(Clone, Copy)]
pub struct AFRegisterPair {
    pub f: u8,
    pub a: u8,
}

#[derive(Clone, Copy)]
pub union AFRegister {
    pub single: AFRegisterPair,
    pub af: u16,
}

impl Default for AFRegister {
    fn default() -> Self {
        AFRegister { af: 0x0 }
    }
}

#[derive(Default, Clone, Copy)]
pub struct BCRegisterPair {
    pub c: u8,
    pub b: u8,
}

#[derive(Clone, Copy)]
pub union BCRegister {
    pub single: BCRegisterPair,
    pub bc: u16,
}

impl Default for BCRegister {
    fn default() -> Self {
        BCRegister { bc: 0x0 }
    }
}

#[derive(Clone, Copy)]
pub struct DERegisterPair {
    pub e: u8,
    pub d: u8,
}

#[derive(Clone, Copy)]
pub union DERegister {
    pub single: DERegisterPair,
    pub de: u16,
}

impl Default for DERegister {
    fn default() -> Self {
        DERegister { de: 0x0 }
    }
}

#[derive(Clone, Copy)]
pub struct HLRegisterPair {
    pub l: u8,
    pub h: u8,
}

#[derive(Clone, Copy)]
pub union HLRegister {
    pub single: HLRegisterPair,
    pub hl: u16,
}

impl Default for HLRegister {
    fn default() -> Self {
        HLRegister { hl: 0x0 }
    }
}

pub struct CPU {
    pub af: AFRegister,
    pub bc: BCRegister,
    pub de: DERegister,
    pub hl: HLRegister,
    pub i: u8,
    pub r: u8,
    pub pc: u16,
    pub sp: u16,

    pub m: u16,
    pub t: u16,

    pub halted: bool,
    pub locked: bool,
    pub ime: bool,

    pub bus: Bus,
}

impl CPU {
    pub fn new(bus: Bus) -> Self {
        Self {
            af: AFRegister::default(),
            bc: BCRegister::default(),
            de: DERegister::default(),
            hl: HLRegister::default(),
            i: 0,
            r: 0,
            pc: 0,
            sp: 0,
            m: 0,
            t: 0,
            halted: false,
            locked: false,
            ime: false,
            bus,
        }
    }

    pub fn render_reg_val(&self, reg_ref: RegRef) -> String {
        unsafe {
            let flags = format!(
                "Z{}N{}H{}C{}",
                self.af.single.f >> 7,
                (self.af.single.f >> 6) & 1,
                (self.af.single.f >> 5) & 1,
                (self.af.single.f >> 4) & 1
            );
            match reg_ref {
                RegRef::A => format!("{} ${1:02x} ({1:})", flags, self.af.single.a),
                RegRef::B => format!("{} ${1:02x} ({1:})", flags, self.bc.single.b),
                RegRef::C => format!("{} ${1:02x} ({1:})", flags, self.bc.single.c),
                RegRef::D => format!("{} ${1:02x} ({1:})", flags, self.de.single.d),
                RegRef::E => format!("{} ${1:02x} ({1:})", flags, self.de.single.e),
                RegRef::H => format!("{} ${1:02x} ({1:})", flags, self.hl.single.h),
                RegRef::L => format!("{} ${1:02x} ({1:})", flags, self.hl.single.l),
                RegRef::AF => format!("{} ${1:04x} ({1:})", flags, self.af.af),
                RegRef::BC => format!("{} ${1:04x} ({1:})", flags, self.bc.bc),
                RegRef::DE => format!("{} ${1:04x} ({1:})", flags, self.de.de),
                RegRef::HL => format!("{} ${1:04x} ({1:})", flags, self.hl.hl),
                RegRef::SP => format!("{} ${1:04x} ({1:})", flags, self.sp),
                RegRef::PC => format!("{} ${1:04x} ({1:})", flags, self.pc),
            }
        }
    }

    fn half_carry(a: u8, b: u8, c: bool) -> bool {
        ((a & 0xf) + (b & 0xf) + (c as u8)) & 0x10 == 0x10
    }

    fn carry(a: u8, b: u8, c: bool) -> bool {
        (a as u16) + (b as u16) + (c as u16) > 0xff
    }

    pub fn reset(&mut self) {
        self.af.af = 0;
        self.bc.bc = 0;
        self.de.de = 0;
        self.hl.hl = 0;
        self.sp = 0;
        self.pc = 0;
        self.m = 0;
        self.t = 0;
        self.halted = false;

        self.bus.reset();
    }

    fn execute_decoded_instruction(&mut self, decoded: Instruction) -> Result<()> {
        let mut action_taken = true;
        match decoded {
            Instruction::Nop => (),
            Instruction::StopN8(_) => {
                self.halted = true;
            }
            Instruction::Halt => {
                self.halted = true;
            }
            Instruction::Rst(tgt3) => {
                self.sp -= 2;
                self.bus.write_u16(self.sp, self.pc)?;
                self.pc = (tgt3.as_operand() as u16) << 3;
            }
            Instruction::CallA16(addr) => {
                self.sp -= 2;
                self.bus.write_u16(self.sp, self.pc)?;
                self.pc = addr;
            }
            Instruction::CallCondA16(cond, a16) => unsafe {
                action_taken = match cond {
                    Cond::CondZ => self.af.single.f & 0x80 == 0x80,
                    Cond::CondNZ => self.af.single.f & 0x80 != 0x80,
                    Cond::CondC => self.af.single.f & 0x10 == 0x10,
                    Cond::CondNC => self.af.single.f & 0x10 != 0x10,
                };
                if action_taken {
                    self.sp -= 2;
                    self.bus.write_u16(self.sp, self.pc)?;
                    self.pc = a16;
                }
            },
            Instruction::JumpNear(e8) => {
                self.pc = (self.pc as i32 + e8 as i32) as u16;
            }
            Instruction::JumpNearCond(cond, e8) => unsafe {
                action_taken = match cond {
                    Cond::CondZ => self.af.single.f & 0x80 == 0x80,
                    Cond::CondNZ => self.af.single.f & 0x80 != 0x80,
                    Cond::CondC => self.af.single.f & 0x10 == 0x10,
                    Cond::CondNC => self.af.single.f & 0x10 != 0x10,
                };
                if action_taken {
                    self.pc = (self.pc as i32 + e8 as i32) as u16;
                }
            },
            Instruction::JumpFar(a16) => self.pc = a16,
            Instruction::JumpFarCond(cond, a16) => unsafe {
                action_taken = match cond {
                    Cond::CondZ => self.af.single.f & 0x80 == 0x80,
                    Cond::CondNZ => self.af.single.f & 0x80 != 0x80,
                    Cond::CondC => self.af.single.f & 0x10 == 0x10,
                    Cond::CondNC => self.af.single.f & 0x10 != 0x10,
                };
                if action_taken {
                    self.pc = a16;
                }
            },
            Instruction::Reti => {
                self.pc = self.bus.read_u16(self.sp)?;
                self.sp += 2;
                self.ime = true;
            }
            Instruction::Ei => {
                self.ime = true;
            }
            Instruction::Di => {
                self.ime = false;
            }
            Instruction::Ret => {
                self.pc = self.bus.read_u16(self.sp)?;
                self.sp += 2;
            }
            Instruction::RetCond(cond) => unsafe {
                let action_taken = match cond {
                    Cond::CondZ => self.af.single.f & 0x80 == 0x80,
                    Cond::CondNZ => self.af.single.f & 0x80 != 0x80,
                    Cond::CondC => self.af.single.f & 0x10 == 0x10,
                    Cond::CondNC => self.af.single.f & 0x10 != 0x10,
                };
                if action_taken {
                    self.pc = self.bus.read_u16(self.sp)?;
                }
            },
            Instruction::LoadAR16Mem(r16mem) => unsafe {
                match r16mem {
                    R16Mem::R16MemBC => self.af.single.a = self.bus.read_u8(self.bc.bc)?,
                    R16Mem::R16MemDE => self.af.single.a = self.bus.read_u8(self.de.de)?,
                    R16Mem::R16MemHLInc => {
                        self.af.single.a = self.bus.read_u8(self.hl.hl)?;
                        self.hl.hl += 1;
                    }
                    R16Mem::R16MemHLDec => {
                        self.af.single.a = self.bus.read_u8(self.hl.hl)?;
                        self.hl.hl -= 1;
                    }
                }
            },
            Instruction::LoadR16N16(r16, val) => match r16 {
                R16::R16BC => self.bc.bc = val,
                R16::R16DE => self.de.de = val,
                R16::R16HL => self.hl.hl = val,
                R16::R16Sp => self.sp = val,
            },
            Instruction::LoadAA16(addr) => {
                self.af.single.a = self.bus.read_u8(addr)?;
            }
            Instruction::LoadR8N8(reg, val) => match reg {
                R8::R8A => self.af.single.a = val,
                R8::R8B => self.bc.single.b = val,
                R8::R8C => self.bc.single.c = val,
                R8::R8D => self.de.single.d = val,
                R8::R8E => self.de.single.e = val,
                R8::R8H => self.hl.single.h = val,
                R8::R8L => self.hl.single.l = val,
                R8::R8HLRef => unsafe {
                    self.bus.write_u8(self.hl.hl, val)?;
                },
            },
            Instruction::LoadR8R8(dst, src) => unsafe {
                let read_byte = match src {
                    R8::R8A => self.af.single.a,
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                };
                match dst {
                    R8::R8A => self.af.single.a = read_byte,
                    R8::R8B => self.bc.single.b = read_byte,
                    R8::R8C => self.bc.single.c = read_byte,
                    R8::R8D => self.de.single.d = read_byte,
                    R8::R8E => self.de.single.e = read_byte,
                    R8::R8H => self.hl.single.h = read_byte,
                    R8::R8L => self.hl.single.l = read_byte,
                    R8::R8HLRef => self.bus.write_u8(self.hl.hl, read_byte)?,
                };
            },
            Instruction::LoadACH => unsafe {
                self.af.single.a = self.bus.read_u8(0xff00 + self.bc.single.c as u16)?;
            },
            Instruction::LoadAA8H(addr) => {
                self.af.single.a = self.bus.read_u8(addr as u16 + 0xff00)?;
            }
            Instruction::StoreAA16(addr) => unsafe {
                self.bus.write_u8(addr, self.af.single.a)?;
            },
            Instruction::StoreACH => unsafe {
                self.bus
                    .write_u8(0xff00 + self.bc.single.c as u16, self.af.single.a)?;
            },
            Instruction::StoreAR16Mem(r16mem) => unsafe {
                match r16mem {
                    R16Mem::R16MemBC => self.bus.write_u8(self.bc.bc, self.af.single.a)?,
                    R16Mem::R16MemDE => self.bus.write_u8(self.de.de, self.af.single.a)?,
                    R16Mem::R16MemHLInc => {
                        self.bus.write_u8(self.hl.hl, self.af.single.a)?;
                        self.hl.hl += 1;
                    }
                    R16Mem::R16MemHLDec => {
                        self.bus.write_u8(self.hl.hl, self.af.single.a)?;
                        self.hl.hl -= 1;
                    }
                }
            },
            Instruction::StoreAA8H(addr) => unsafe {
                self.bus.write_u8(addr as u16 | 0xff00, self.af.single.a)?;
            },
            Instruction::StoreSPA16(imm16) => {
                self.bus.write_u16(imm16, self.sp)?;
            }
            Instruction::PushR16Stack(operand) => unsafe {
                self.sp -= 2;
                match operand {
                    R16Stack::R16StackAF => self.bus.write_u16(self.sp, self.af.af)?,
                    R16Stack::R16StackBC => self.bus.write_u16(self.sp, self.bc.bc)?,
                    R16Stack::R16StackDE => self.bus.write_u16(self.sp, self.de.de)?,
                    R16Stack::R16StackHL => self.bus.write_u16(self.sp, self.hl.hl)?,
                }
            },
            Instruction::PopR16Stack(operand) => {
                match operand {
                    R16Stack::R16StackAF => self.af.af = self.bus.read_u16(self.sp)?,
                    R16Stack::R16StackBC => self.bc.bc = self.bus.read_u16(self.sp)?,
                    R16Stack::R16StackDE => self.de.de = self.bus.read_u16(self.sp)?,
                    R16Stack::R16StackHL => self.hl.hl = self.bus.read_u16(self.sp)?,
                }
                self.sp += 2;
            }
            Instruction::IncR16(r16) => unsafe {
                match r16 {
                    R16::R16BC => self.bc.bc += 1,
                    R16::R16DE => self.de.de += 1,
                    R16::R16HL => self.hl.hl += 1,
                    R16::R16Sp => self.sp += 1,
                }
            },
            Instruction::IncR8(r8) => unsafe {
                let r8_val = match r8 {
                    R8::R8A => self.af.single.a,
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                };
                let result = r8_val.wrapping_add(1);
                self.af.single.f = ((result == 0) as u8) << 7
                    | (Self::half_carry(r8_val, 1, false) as u8) << 5
                    | self.af.single.f & 0x1f;
                match r8 {
                    R8::R8A => self.af.single.a = result,
                    R8::R8B => self.bc.single.b = result,
                    R8::R8C => self.bc.single.c = result,
                    R8::R8D => self.de.single.d = result,
                    R8::R8E => self.de.single.e = result,
                    R8::R8H => self.hl.single.h = result,
                    R8::R8L => self.hl.single.l = result,
                    R8::R8HLRef => self.bus.write_u8(self.hl.hl, result)?,
                };
            },
            Instruction::DecR16(r16) => unsafe {
                match r16 {
                    R16::R16BC => self.bc.bc -= 1,
                    R16::R16DE => self.de.de -= 1,
                    R16::R16HL => self.hl.hl -= 1,
                    R16::R16Sp => self.sp -= 1,
                }
            },
            Instruction::DecR8(r8) => unsafe {
                let r8_val = match r8 {
                    R8::R8A => self.af.single.a,
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                };
                let result = r8_val.wrapping_sub(1);
                self.af.single.f = ((result == 0) as u8) << 7
                    | 1 << 6
                    | (Self::half_carry(r8_val, u8::MAX, false) as u8) << 5
                    | self.af.single.f & 0x1f;
                match r8 {
                    R8::R8A => self.af.single.a = result,
                    R8::R8B => self.bc.single.b = result,
                    R8::R8C => self.bc.single.c = result,
                    R8::R8D => self.de.single.d = result,
                    R8::R8E => self.de.single.e = result,
                    R8::R8H => self.hl.single.h = result,
                    R8::R8L => self.hl.single.l = result,
                    R8::R8HLRef => self.bus.write_u8(self.hl.hl, result)?,
                };
            },

            Instruction::AddHLR16(r16) => unsafe {
                let hl = self.hl.hl;
                let r16_val = match r16 {
                    R16::R16BC => self.bc.bc,
                    R16::R16DE => self.de.de,
                    R16::R16HL => self.hl.hl,
                    R16::R16Sp => self.sp,
                };
                let result = hl.wrapping_add(r16_val);
                let h = (hl & 0xfff) + (r16_val & 0xfff) > 0xfff;
                let c = hl > u16::MAX - r16_val;
                self.af.single.f = (h as u8) << 5 | (c as u8) << 4 | self.af.single.f & 0x8f;
                self.hl.hl = result;
            },

            Instruction::AddR8(reg) => unsafe {
                let a = self.af.single.a;
                let b = match reg {
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => self.af.single.a,
                };
                let c = Self::carry(a, b, false);
                let h = Self::half_carry(a, b, false);
                let n = false;
                let result = a.wrapping_add(b);
                let z = result == 0;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                self.af.single.a = result;
            },
            Instruction::AdcR8(reg) => unsafe {
                let c = self.af.single.f & 0x10 == 0x10;
                let a = self.af.single.a;
                let b = match reg {
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => self.af.single.a,
                };
                let c3 = Self::carry(a, b, c);
                let h = Self::half_carry(a, b, c);
                let n = false;
                let result = a.wrapping_add(b).wrapping_add(c as u8);
                let z = result == 0;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c3 as u8) << 4);
                self.af.single.a = result;
            },
            Instruction::SubR8(reg) => unsafe {
                let a = self.af.single.a;
                let n = match reg {
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => self.af.single.a,
                };
                let result = a.wrapping_sub(n);
                let z = a == n;
                let c = a < n;
                let h = (a & 0xf) < (n & 0xf);
                let n = true;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                self.af.single.a = result;
            },
            Instruction::AddAN8(n8) => unsafe {
                let a = self.af.single.a;
                let n = n8;
                let c = Self::carry(a, n, false);
                let h = Self::half_carry(a, n, false);
                let result = a.wrapping_add(n);
                let z = result == 0;
                let n = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                self.af.single.a = result;
            },
            Instruction::AdcAN8(n8) => unsafe {
                let c = self.af.single.f & 0x10 == 0x10;
                let a = self.af.single.a;
                let n = n8;
                let c3 = Self::carry(a, n, c);
                let h = Self::half_carry(a, n, c);
                let result = a.wrapping_add(n).wrapping_add(c as u8);
                let z = result == 0;
                let n = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c3 as u8) << 4);
                self.af.single.a = result;
            },
            Instruction::SubAN8(n8) => unsafe {
                let a = self.af.single.a;
                let n = n8;
                let result = a.wrapping_sub(n);
                let z = result == 0;
                let c = a < n;
                let h = (a & 0xf) < (n & 0xf);
                let n = true;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                self.af.single.a = result;
            },
            Instruction::SbcAN8(n8) => unsafe {
                let c = self.af.single.f & 0x10 == 0x10;
                let a = self.af.single.a;
                let n = n8;
                let c3 = Self::carry(a, n, c);
                let h = Self::half_carry(a, n, c);
                let result = a.wrapping_sub(n).wrapping_sub(c as u8);
                let z = result == 0;
                let n = true;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c3 as u8) << 4);
                self.af.single.a = result;
            },
            Instruction::AndAN8(n8) => unsafe {
                let a = self.af.single.a;
                let n = n8;
                let result = a & n;
                let z = result == 0;
                let c = false;
                let n = false;
                let h = true;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                self.af.single.a = result;
            },
            Instruction::OrAN8(n8) => unsafe {
                let a = self.af.single.a;
                let n = n8;
                let result = a | n;
                let z = result == 0;
                let n = false;
                let h = false;
                let c = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                self.af.single.a = result;
            },
            Instruction::XorAN8(n8) => unsafe {
                let a = self.af.single.a;
                let n = n8;
                let result = a ^ n;
                let z = result == 0;
                let n = false;
                let h = false;
                let c = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                self.af.single.a = result;
            },
            Instruction::CpAN8(n8) => unsafe {
                let a = self.af.single.a;
                let n = n8;
                let z = a == n;
                let c = a < n;
                let h = (a & 0xf) < (n & 0xf);
                let n = true;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
            },

            Instruction::AndR8(reg) => unsafe {
                match reg as R8 {
                    R8::R8B => self.af.single.a &= self.bc.single.b,
                    R8::R8C => self.af.single.a &= self.bc.single.c,
                    R8::R8D => self.af.single.a &= self.de.single.d,
                    R8::R8E => self.af.single.a &= self.de.single.e,
                    R8::R8H => self.af.single.a &= self.hl.single.h,
                    R8::R8L => self.af.single.a &= self.hl.single.l,
                    R8::R8HLRef => self.af.single.a &= self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => {}
                }
                let z = self.af.single.a == 0;
                let c = false;
                let n = false;
                let h = true;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
            },
            Instruction::OrR8(reg) => unsafe {
                match reg as R8 {
                    R8::R8B => self.af.single.a |= self.bc.single.b,
                    R8::R8C => self.af.single.a |= self.bc.single.c,
                    R8::R8D => self.af.single.a |= self.de.single.d,
                    R8::R8E => self.af.single.a |= self.de.single.e,
                    R8::R8H => self.af.single.a |= self.hl.single.h,
                    R8::R8L => self.af.single.a |= self.hl.single.l,
                    R8::R8HLRef => self.af.single.a |= self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => {}
                }
                let z = self.af.single.a == 0;
                let n = false;
                let h = false;
                let c = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
            },
            Instruction::XorR8(reg) => unsafe {
                match reg as R8 {
                    R8::R8B => self.af.single.a ^= self.bc.single.b,
                    R8::R8C => self.af.single.a ^= self.bc.single.c,
                    R8::R8D => self.af.single.a ^= self.de.single.d,
                    R8::R8E => self.af.single.a ^= self.de.single.e,
                    R8::R8H => self.af.single.a ^= self.hl.single.h,
                    R8::R8L => self.af.single.a ^= self.hl.single.l,
                    R8::R8HLRef => self.af.single.a ^= self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => self.af.single.a ^= self.af.single.a,
                }
                let z = self.af.single.a == 0;
                let n = false;
                let h = false;
                let c = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
            },
            Instruction::CpR8(reg) => unsafe {
                let a = self.af.single.a;
                let n = match reg {
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => self.af.single.a,
                };
                let z = a == n;
                let c = a < n;
                let h = (a & 0xf) < (n & 0xf);
                let n = true;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
            },
            Instruction::SbcR8(reg) => unsafe {
                let a = self.af.single.a;
                let n = match reg {
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => self.af.single.a,
                };
                let result = a.wrapping_sub(n.wrapping_add(1));
                let z = a == n.wrapping_add(1);
                let c = a < n.wrapping_add(1);
                let h = (a & 0xf) < (n.wrapping_add(1) & 0xf);
                let n = true;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                self.af.single.a = result;
            },

            Instruction::RlcA => unsafe {
                let val = self.af.single.a;
                let c = val & 0x80 == 0x80;
                let result = (val << 1) | (c as u8);
                let z = result == 0;
                let n = false;
                let h = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                self.af.single.a = result;
            },
            Instruction::RrcA => unsafe {
                let val = self.af.single.a;
                let c = val & 0x1 == 0x1;
                let result = (val >> 1) | ((c as u8) << 7);
                let z = result == 0;
                let n = false;
                let h = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                self.af.single.a = result;
            },
            Instruction::RlA => unsafe {
                let val = self.af.single.a;
                let c = val & 0x80 == 0x80;
                let result = (val << 1) | ((self.af.single.f & 0x10) >> 4);
                let z = result == 0;
                let n = false;
                let h = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                self.af.single.a = result;
            },
            Instruction::RrA => unsafe {
                let val = self.af.single.a;
                let c = val & 0x1 == 0x1;
                let result = (val >> 1) | ((self.af.single.f & 0x10) << 3);
                let z = result == 0;
                let n = false;
                let h = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                self.af.single.a = result;
            },
            Instruction::Daa => unsafe {
                let mut a = self.af.single.a;
                let mut adjust = 0;
                if self.af.single.f & 0x20 == 0x20 {
                    adjust |= 0x60;
                }
                if self.af.single.f & 0x10 == 0x10 || (a & 0xf) > 9 {
                    adjust |= 0x6;
                }
                if self.af.single.f & 0x40 == 0x40 || a > 0x99 {
                    adjust |= 0x60;
                    self.af.single.f |= 0x10;
                }
                a = a.wrapping_add(adjust);
                self.af.single.f &= 0x40;
                if a == 0 {
                    self.af.single.f |= 0x80;
                }
                self.af.single.a = a;
            },
            Instruction::Cpl => unsafe {
                self.af.single.a = !self.af.single.a;
                self.af.single.f |= 0x60; // Set N and H only
            },
            Instruction::Scf => unsafe {
                self.af.single.f &= 0x80; // clear all flags but Z
                self.af.single.f |= 0x10; // set carry
            },
            Instruction::Ccf => unsafe {
                self.af.single.f ^= 0x60; // clear N and H
                self.af.single.f ^= 0x10; // flip carry
            },

            Instruction::Rlc(r8) => unsafe {
                let val = match r8 {
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => self.af.single.a,
                };
                let c = val & 0x80 == 0x80;
                let result = (val << 1) | (c as u8);
                let z = result == 0;
                let n = false;
                let h = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                match r8 {
                    R8::R8B => self.bc.single.b = result,
                    R8::R8C => self.bc.single.c = result,
                    R8::R8D => self.de.single.d = result,
                    R8::R8E => self.de.single.e = result,
                    R8::R8H => self.hl.single.h = result,
                    R8::R8L => self.hl.single.l = result,
                    R8::R8HLRef => self.bus.write_u8(self.hl.hl, result)?,
                    R8::R8A => self.af.single.a = result,
                };
            },
            Instruction::Rrc(r8) => unsafe {
                let val = match r8 {
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => self.af.single.a,
                };
                let c = val & 0x1 == 0x1;
                let result = (val >> 1) | ((c as u8) << 7);
                let z = result == 0;
                let n = false;
                let h = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                match r8 {
                    R8::R8B => self.bc.single.b = result,
                    R8::R8C => self.bc.single.c = result,
                    R8::R8D => self.de.single.d = result,
                    R8::R8E => self.de.single.e = result,
                    R8::R8H => self.hl.single.h = result,
                    R8::R8L => self.hl.single.l = result,
                    R8::R8HLRef => self.bus.write_u8(self.hl.hl, result)?,
                    R8::R8A => self.af.single.a = result,
                };
            },

            Instruction::Rl(r8) => unsafe {
                let val = match r8 {
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => self.af.single.a,
                };
                let c = val & 0x80 == 0x80;
                let result = (val << 1) | ((self.af.single.f & 0x10) >> 4);
                let z = result == 0;
                let n = false;
                let h = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                match r8 {
                    R8::R8B => self.bc.single.b = result,
                    R8::R8C => self.bc.single.c = result,
                    R8::R8D => self.de.single.d = result,
                    R8::R8E => self.de.single.e = result,
                    R8::R8H => self.hl.single.h = result,
                    R8::R8L => self.hl.single.l = result,
                    R8::R8HLRef => self.bus.write_u8(self.hl.hl, result)?,
                    R8::R8A => self.af.single.a = result,
                };
            },
            Instruction::Rr(r8) => unsafe {
                let val = match r8 {
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => self.af.single.a,
                };
                let c = val & 0x1 == 0x1;
                let result = (val >> 1) | ((self.af.single.f & 0x10) << 3);
                let z = result == 0;
                let n = false;
                let h = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                match r8 {
                    R8::R8B => self.bc.single.b = result,
                    R8::R8C => self.bc.single.c = result,
                    R8::R8D => self.de.single.d = result,
                    R8::R8E => self.de.single.e = result,
                    R8::R8H => self.hl.single.h = result,
                    R8::R8L => self.hl.single.l = result,
                    R8::R8HLRef => self.bus.write_u8(self.hl.hl, result)?,
                    R8::R8A => self.af.single.a = result,
                };
            },
            Instruction::Sla(r8) => unsafe {
                let val = match r8 {
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => self.af.single.a,
                };
                let c = val & 0x80 == 0x80;
                let result = val << 1;
                let z = result == 0;
                let n = false;
                let h = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                match r8 {
                    R8::R8B => self.bc.single.b = result,
                    R8::R8C => self.bc.single.c = result,
                    R8::R8D => self.de.single.d = result,
                    R8::R8E => self.de.single.e = result,
                    R8::R8H => self.hl.single.h = result,
                    R8::R8L => self.hl.single.l = result,
                    R8::R8HLRef => self.bus.write_u8(self.hl.hl, result)?,
                    R8::R8A => self.af.single.a = result,
                };
            },
            Instruction::Sra(r8) => unsafe {
                let val = match r8 {
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => self.af.single.a,
                };
                let c = val & 0x1 == 0x1;
                let result = (val >> 1) | (val & 0x80);
                let z = result == 0;
                let n = false;
                let h = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                match r8 {
                    R8::R8B => self.bc.single.b = result,
                    R8::R8C => self.bc.single.c = result,
                    R8::R8D => self.de.single.d = result,
                    R8::R8E => self.de.single.e = result,
                    R8::R8H => self.hl.single.h = result,
                    R8::R8L => self.hl.single.l = result,
                    R8::R8HLRef => self.bus.write_u8(self.hl.hl, result)?,
                    R8::R8A => self.af.single.a = result,
                };
            },
            Instruction::Swap(r8) => unsafe {
                let val = match r8 {
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => self.af.single.a,
                };
                let result = (val >> 4) | (val << 4);
                let z = result == 0;
                let n = false;
                let h = false;
                let c = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                match r8 {
                    R8::R8B => self.bc.single.b = result,
                    R8::R8C => self.bc.single.c = result,
                    R8::R8D => self.de.single.d = result,
                    R8::R8E => self.de.single.e = result,
                    R8::R8H => self.hl.single.h = result,
                    R8::R8L => self.hl.single.l = result,
                    R8::R8HLRef => self.bus.write_u8(self.hl.hl, result)?,
                    R8::R8A => self.af.single.a = result,
                };
            },
            Instruction::Srl(r8) => unsafe {
                let val = match r8 {
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => self.af.single.a,
                };
                let c = val & 0x1 == 0x1;
                let result = val >> 1;
                let z = result == 0;
                let n = false;
                let h = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
                match r8 {
                    R8::R8B => self.bc.single.b = result,
                    R8::R8C => self.bc.single.c = result,
                    R8::R8D => self.de.single.d = result,
                    R8::R8E => self.de.single.e = result,
                    R8::R8H => self.hl.single.h = result,
                    R8::R8L => self.hl.single.l = result,
                    R8::R8HLRef => self.bus.write_u8(self.hl.hl, result)?,
                    R8::R8A => self.af.single.a = result,
                };
            },

            Instruction::Bit(bit, r8) => unsafe {
                let mask: u8 = 1 << bit.encode();
                let val = match r8 {
                    R8::R8B => self.bc.single.b,
                    R8::R8C => self.bc.single.c,
                    R8::R8D => self.de.single.d,
                    R8::R8E => self.de.single.e,
                    R8::R8H => self.hl.single.h,
                    R8::R8L => self.hl.single.l,
                    R8::R8HLRef => self.bus.read_u8(self.hl.hl)?,
                    R8::R8A => self.af.single.a,
                };
                let result = val & mask;
                let z = result == 0;
                let n = false;
                let h = true;
                self.af.single.f = ((z as u8) << 7)
                    | ((n as u8) << 6)
                    | ((h as u8) << 5)
                    | (self.af.single.f & 0x10);
            },
            Instruction::Res(bit, r8) => unsafe {
                let mask: u8 = 0xff ^ (1 << bit.encode());
                match r8 {
                    R8::R8B => self.bc.single.b &= mask,
                    R8::R8C => self.bc.single.c &= mask,
                    R8::R8D => self.de.single.d &= mask,
                    R8::R8E => self.de.single.e &= mask,
                    R8::R8H => self.hl.single.h &= mask,
                    R8::R8L => self.hl.single.l &= mask,
                    R8::R8HLRef => {
                        let val = self.bus.read_u8(self.hl.hl)?;
                        self.bus.write_u8(self.hl.hl, val & mask)?;
                    }
                    R8::R8A => self.af.single.a &= mask,
                };
            },
            Instruction::Set(bit, r8) => unsafe {
                let mask: u8 = 1 << bit.encode();
                match r8 {
                    R8::R8B => self.bc.single.b |= mask,
                    R8::R8C => self.bc.single.c |= mask,
                    R8::R8D => self.de.single.d |= mask,
                    R8::R8E => self.de.single.e |= mask,
                    R8::R8H => self.hl.single.h |= mask,
                    R8::R8L => self.hl.single.l |= mask,
                    R8::R8HLRef => {
                        let val = self.bus.read_u8(self.hl.hl)?;
                        self.bus.write_u8(self.hl.hl, val | mask)?;
                    }
                    R8::R8A => self.af.single.a |= mask,
                };
            },

            Instruction::AddSPN8(n8) => unsafe {
                let sp = self.sp;
                let n = n8 as i8 as i16 as u16;
                let result = sp.wrapping_add(n);
                let c = (sp & 0xff) + (n & 0xff) > 0xff;
                let h = (sp & 0xf) + (n & 0xf) > 0xf;
                self.af.single.f = (self.af.single.f & 0x80) | ((c as u8) << 4) | ((h as u8) << 5);
                self.sp = result;
            },
            Instruction::LoadHLSPN8(n8) => unsafe {
                let sp = self.sp;
                let n = n8 as i8 as i16 as u16;
                let result = sp.wrapping_add(n);
                self.hl.hl = result;
                let c = (sp & 0xff) + (n & 0xff) > 0xff;
                let h = (sp & 0xf) + (n & 0xf) > 0xf;
                self.af.single.f = (self.af.single.f & 0x80) | ((c as u8) << 4) | ((h as u8) << 5);
            },
            Instruction::JumpFarHL => unsafe {
                self.pc = self.hl.hl;
            },
            Instruction::LoadHLSP => self.hl.hl = self.sp,
        }

        if action_taken {
            self.m = self.m.wrapping_add(decoded.m_cycles().0 as u16);
            self.t = self.t.wrapping_add(decoded.t_cycles().0 as u16);
        } else {
            self.m = self.m.wrapping_add(decoded.m_cycles().1 as u16);
            self.t = self.t.wrapping_add(decoded.t_cycles().1 as u16);
        }
        Ok(())
    }

    pub fn execute_single_instruction(&mut self) -> Result<()> {
        let insn = self.bus.read_u8(self.pc)?;
        self.pc += 1;
        let size = Instruction::size_header(insn)? as usize;
        let mut buf = vec![0u8; size];
        buf[0] = insn;
        for i in 0..(size - 1) {
            buf[i + 1] = self.bus.read_u8(self.pc)?;
            self.pc += 1;
        }
        let (decoded, bytes_consumed) = Instruction::from_u8_slice(buf.as_slice(), 0, size)?;
        assert!(
            bytes_consumed as usize == buf.len(),
            "differing expectations of instruction size: {} vs {}, bytes: {:02x?}",
            bytes_consumed,
            buf.len(),
            buf
        );
        let result = self.execute_decoded_instruction(decoded);
        if result.is_err() {
            println!("CPU state: {}", self);
        }
        result
    }
}

impl Display for CPU {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            write!(
                f,
                "CPU {{\n\ti: ${:02x},\tr: ${:02x},\n\tpc: ${:04x},\tsp: ${:04x},\n\ta: ${:02x},\tf: ${:02x},\tb: ${:02x},\tc: ${:02x},\td: ${:02x},\te: ${:02x},\th: ${:02x},\tl: ${:02x},\n\taf: ${:04x},\tbc: ${:04x},\tde: ${:04x},\thl: ${:04x},\n\tm: ${:02x},\tt: ${:02x}\n}}\n",
                self.i, self.r,
                self.pc, self.sp,
                self.af.single.a, self.af.single.f, self.bc.single.b, self.bc.single.c, self.de.single.d, self.de.single.e, self.hl.single.h, self.hl.single.l,
                self.af.af, self.bc.bc, self.de.de, self.hl.hl,
                self.m, self.t,
            )?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::machine::Machine;
    use crate::rom::ROM;

    #[test]
    fn boot() {
        // https://gbdev.io/pandocs/Power_Up_Sequence.html

        const VALID_BUT_EMPTY_ROM: &[u8; 0x8000] = concat_bytes!(
            [0; 0x104],
            // nintendo logo
            b"\xce\xedff\xcc\r\x00\x0b\x03s\x00\x83\x00\x0c\x00\r\x00\x08\x11\x1f\x88\x89\x00\x0e\xdc\xccn\xe6\xdd\xdd\xd9\x99\xbb\xbbgcn\x0e\xec\xcc\xdd\xdc\x99\x9f\xbb\xb93>",
            // checksum
            [0xe7],
            // remaining padding
            [0; 0x7ecb]
        );

        unsafe {
            let mut machine = Machine::new(ROM::from_buf(VALID_BUT_EMPTY_ROM.to_vec()));
            while machine.cpu.pc != 0x100 {
                machine.step().unwrap();
                // println!("pc: ${:02x}", machine.cpu.pc);
            }

            assert_eq!(machine.cpu.bc.bc, 0x0013);
            assert_eq!(machine.cpu.de.de, 0x00d8);
            assert_eq!(machine.cpu.hl.hl, 0x014d);
            assert_eq!(machine.cpu.sp, 0xfffe);
            assert_eq!(machine.cpu.af.af, 0x0180);
        }
    }
}
