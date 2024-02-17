use std::{fmt::Display, rc::Rc, sync::Mutex};

use crate::{
    bus::Bus,
    instruction::{Condition, Instruction, R16Mem, R16Stack, R16, R8},
};

#[derive(Clone, Copy)]
pub struct AFRegisterPair {
    pub a: u8,
    pub f: u8,
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
    pub b: u8,
    pub c: u8,
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
    pub d: u8,
    pub e: u8,
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
    pub h: u8,
    pub l: u8,
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
    pub t: u32,

    pub halted: bool,

    bus: Rc<Mutex<Bus>>,
}

impl CPU {
    pub fn new(bus: Rc<Mutex<Bus>>) -> Self {
        Self {
            bus: bus,
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

        self.bus.lock().unwrap().reset();
    }

    pub fn execute_single_instruction(&mut self) {
        let mut bus = self.bus.lock().unwrap();
        let insn = bus.read_u8(self.pc);
        self.pc += 1;
        let size = Instruction::size_header(insn);
        let mut buf = vec![0u8; (size + 1).into()];
        buf[0] = insn;
        for i in 0..(size as usize - 1) {
            buf[i + 1] = bus.read_u8(self.pc);
            self.pc += 1;
        }
        let decoded = Instruction::from_u8_slice(buf.as_slice(), 0);
        match decoded {
            Instruction::Nop => {
                self.m += 1;
                self.t += 4;
            }
            Instruction::Stop(_) => {
                self.halted = true;
            }
            Instruction::Halt => {
                self.halted = true;
            }
            Instruction::Call(addr) => {
                self.sp -= 2;
                bus.write_u16(self.sp, self.pc);
                self.pc = addr;
                self.m += 6;
                self.t += 24;
            }
            Instruction::JR(offset, None) => {
                self.pc = (self.pc as i32 + offset as i32) as u16;
                self.m += 3;
                self.t += 12;
            }
            Instruction::JR(offset, Some(cond)) => unsafe {
                let should_jump = match cond {
                    Condition::Z => self.af.single.f & 0x80 == 0x80,
                    Condition::NZ => self.af.single.f & 0x80 != 0x80,
                    Condition::C => self.af.single.f & 0x10 == 0x10,
                    Condition::NC => self.af.single.f & 0x10 != 0x10,
                };
                if should_jump {
                    self.pc = (self.pc as i32 + offset as i32) as u16;
                    self.m += 3;
                    self.t += 12;
                } else {
                    self.m += 2;
                    self.t += 8;
                }
            },
            Instruction::Ret(None) => {
                self.pc = bus.read_u16(self.sp);
                self.sp += 2;
                self.m += 3;
                self.t += 16;
            }
            Instruction::Ret(Some(cond)) => unsafe {
                let should_ret = match cond {
                    Condition::Z => self.af.single.f & 0x80 == 0x80,
                    Condition::NZ => self.af.single.f & 0x80 != 0x80,
                    Condition::C => self.af.single.f & 0x10 == 0x10,
                    Condition::NC => self.af.single.f & 0x10 != 0x10,
                };
                if should_ret {
                    self.pc = bus.read_u16(self.sp);
                }
            },
            Instruction::Load16Mem(r16mem) => unsafe {
                match r16mem {
                    R16Mem::BC => self.af.single.a = bus.read_u8(self.bc.bc),
                    R16Mem::DE => self.af.single.a = bus.read_u8(self.de.de),
                    R16Mem::HLInc => {
                        self.af.single.a = bus.read_u8(self.hl.hl);
                        self.hl.hl += 1;
                    }
                    R16Mem::HLDec => {
                        self.af.single.a = bus.read_u8(self.hl.hl);
                        self.hl.hl -= 1;
                    }
                }
                self.m += 2;
                self.t += 8;
            },
            Instruction::Load16Imm(r16, val) => {
                match r16 {
                    R16::BC => self.bc.bc = val,
                    R16::DE => self.de.de = val,
                    R16::HL => self.hl.hl = val,
                    R16::SP => self.sp = val,
                }
                self.m += 3;
                self.t += 12;
            }
            Instruction::Load8Imm(reg, val) => {
                match reg {
                    R8::A => self.af.single.a = val,
                    R8::B => self.bc.single.b = val,
                    R8::C => self.bc.single.c = val,
                    R8::D => self.de.single.d = val,
                    R8::E => self.de.single.e = val,
                    R8::H => self.hl.single.h = val,
                    R8::L => self.hl.single.l = val,
                    R8::HLRef => unsafe {
                        bus.write_u8(self.hl.hl, val);
                    },
                }
                self.m += 4;
                self.t += 8;
            }
            Instruction::Load8(dst, src) => unsafe {
                let read_byte = match src {
                    R8::A => self.af.single.a,
                    R8::B => self.bc.single.b,
                    R8::C => self.bc.single.c,
                    R8::D => self.de.single.d,
                    R8::E => self.de.single.e,
                    R8::H => self.hl.single.h,
                    R8::L => self.hl.single.l,
                    R8::HLRef => bus.read_u8(self.hl.hl),
                };
                match dst {
                    R8::A => self.af.single.a = read_byte,
                    R8::B => self.bc.single.b = read_byte,
                    R8::C => self.bc.single.c = read_byte,
                    R8::D => self.de.single.d = read_byte,
                    R8::E => self.de.single.e = read_byte,
                    R8::H => self.hl.single.h = read_byte,
                    R8::L => self.hl.single.l = read_byte,
                    R8::HLRef => bus.write_u8(self.hl.hl, read_byte),
                };
            },
            Instruction::Store8(r16mem) => unsafe {
                match r16mem {
                    R16Mem::BC => bus.write_u8(self.bc.bc, self.af.single.a),
                    R16Mem::DE => bus.write_u8(self.de.de, self.af.single.a),
                    R16Mem::HLInc => {
                        bus.write_u8(self.hl.hl, self.af.single.a);
                        self.hl.hl += 1;
                    }
                    R16Mem::HLDec => {
                        bus.write_u8(self.hl.hl, self.af.single.a);
                        self.hl.hl -= 1;
                    }
                };
                self.m += 4;
                self.t += 8;
            },
            Instruction::Store8H(addr) => unsafe {
                bus.write_u8(addr, self.af.single.a);
                self.m += 4;
                self.t += 8;
            },
            Instruction::StoreSP(imm16) => {
                bus.write_u16(imm16, self.sp);
                self.m += 5;
                self.t += 20;
            }
            Instruction::Push(operand) => unsafe {
                self.sp -= 2;
                match operand {
                    R16Stack::AF => bus.write_u16(self.sp, self.af.af),
                    R16Stack::BC => bus.write_u16(self.sp, self.bc.bc),
                    R16Stack::DE => bus.write_u16(self.sp, self.de.de),
                    R16Stack::HL => bus.write_u16(self.sp, self.hl.hl),
                }
                self.m += 4;
                self.t += 16;
            },
            Instruction::Pop(operand) => {
                match operand {
                    R16Stack::AF => self.af.af = bus.read_u16(self.sp),
                    R16Stack::BC => self.bc.bc = bus.read_u16(self.sp),
                    R16Stack::DE => self.de.de = bus.read_u16(self.sp),
                    R16Stack::HL => self.hl.hl = bus.read_u16(self.sp),
                }
                self.sp += 2;
                self.m += 3;
                self.t += 12;
            }
            Instruction::INC16(r16) => unsafe {
                match r16 {
                    R16::BC => self.bc.bc += 1,
                    R16::DE => self.de.de += 1,
                    R16::HL => self.hl.hl += 1,
                    R16::SP => self.sp += 1,
                }
            },
            Instruction::INC8(r8) => unsafe {
                let r8_val = match r8 {
                    R8::A => self.af.single.a,
                    R8::B => self.bc.single.b,
                    R8::C => self.bc.single.c,
                    R8::D => self.de.single.d,
                    R8::E => self.de.single.e,
                    R8::H => self.hl.single.h,
                    R8::L => self.hl.single.l,
                    R8::HLRef => bus.read_u8(self.hl.hl),
                };
                let result = r8_val.wrapping_add(1);
                self.af.single.f = ((result == 0) as u8) << 7
                    | (Self::half_carry(r8_val, 1, false) as u8) << 5
                    | self.af.single.f & 0x1f;
                match r8 {
                    R8::A => self.af.single.a = result,
                    R8::B => self.bc.single.b = result,
                    R8::C => self.bc.single.c = result,
                    R8::D => self.de.single.d = result,
                    R8::E => self.de.single.e = result,
                    R8::H => self.hl.single.h = result,
                    R8::L => self.hl.single.l = result,
                    R8::HLRef => bus.write_u8(self.hl.hl, result),
                };
            },
            Instruction::DEC16(r16) => unsafe {
                match r16 {
                    R16::BC => self.bc.bc -= 1,
                    R16::DE => self.de.de -= 1,
                    R16::HL => self.hl.hl -= 1,
                    R16::SP => self.sp -= 1,
                }
            },
            Instruction::DEC8(r8) => unsafe {
                let r8_val = match r8 {
                    R8::A => self.af.single.a,
                    R8::B => self.bc.single.b,
                    R8::C => self.bc.single.c,
                    R8::D => self.de.single.d,
                    R8::E => self.de.single.e,
                    R8::H => self.hl.single.h,
                    R8::L => self.hl.single.l,
                    R8::HLRef => bus.read_u8(self.hl.hl),
                };
                let result = r8_val.wrapping_sub(1);
                self.af.single.f = ((result == 0) as u8) << 7
                    | 1 << 6
                    | (Self::half_carry(r8_val, u8::MAX, false) as u8) << 5
                    | self.af.single.f & 0x1f;
                match r8 {
                    R8::A => self.af.single.a = result,
                    R8::B => self.bc.single.b = result,
                    R8::C => self.bc.single.c = result,
                    R8::D => self.de.single.d = result,
                    R8::E => self.de.single.e = result,
                    R8::H => self.hl.single.h = result,
                    R8::L => self.hl.single.l = result,
                    R8::HLRef => bus.write_u8(self.hl.hl, result),
                };
            },
            Instruction::ADD(reg) => unsafe {
                let a = self.af.single.a;
                let b = match reg {
                    R8::B => self.bc.single.b,
                    R8::C => self.bc.single.c,
                    R8::D => self.de.single.d,
                    R8::E => self.de.single.e,
                    R8::H => self.hl.single.h,
                    R8::L => self.hl.single.l,
                    R8::HLRef => bus.read_u8(self.hl.hl),
                    R8::A => self.af.single.a,
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
            Instruction::ADC(reg) => unsafe {
                let c = self.af.single.f & 0x10 == 0x10;
                let a = self.af.single.a;
                let b = match reg {
                    R8::B => self.bc.single.b,
                    R8::C => self.bc.single.c,
                    R8::D => self.de.single.d,
                    R8::E => self.de.single.e,
                    R8::H => self.hl.single.h,
                    R8::L => self.hl.single.l,
                    R8::HLRef => bus.read_u8(self.hl.hl),
                    R8::A => self.af.single.a,
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
            Instruction::SUB(reg) => unsafe {
                let a = self.af.single.a;
                let n = match reg {
                    R8::B => self.bc.single.b,
                    R8::C => self.bc.single.c,
                    R8::D => self.de.single.d,
                    R8::E => self.de.single.e,
                    R8::H => self.hl.single.h,
                    R8::L => self.hl.single.l,
                    R8::HLRef => bus.read_u8(self.hl.hl),
                    R8::A => self.af.single.a,
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
            Instruction::AND(reg) => unsafe {
                match reg as R8 {
                    R8::B => self.af.single.a &= self.bc.single.b,
                    R8::C => self.af.single.a &= self.bc.single.c,
                    R8::D => self.af.single.a &= self.de.single.d,
                    R8::E => self.af.single.a &= self.de.single.e,
                    R8::H => self.af.single.a &= self.hl.single.h,
                    R8::L => self.af.single.a &= self.hl.single.l,
                    R8::HLRef => self.af.single.a &= bus.read_u8(self.hl.hl),
                    R8::A => {}
                }
                let z = self.af.single.a == 0;
                let c = false;
                let n = false;
                let h = true;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
            },
            Instruction::OR(reg) => unsafe {
                match reg as R8 {
                    R8::B => self.af.single.a |= self.bc.single.b,
                    R8::C => self.af.single.a |= self.bc.single.c,
                    R8::D => self.af.single.a |= self.de.single.d,
                    R8::E => self.af.single.a |= self.de.single.e,
                    R8::H => self.af.single.a |= self.hl.single.h,
                    R8::L => self.af.single.a |= self.hl.single.l,
                    R8::HLRef => self.af.single.a |= bus.read_u8(self.hl.hl),
                    R8::A => {}
                }
                let z = self.af.single.a == 0;
                let n = false;
                let h = false;
                let c = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
            },
            Instruction::XOR(reg) => unsafe {
                match reg as R8 {
                    R8::B => self.af.single.a ^= self.bc.single.b,
                    R8::C => self.af.single.a ^= self.bc.single.c,
                    R8::D => self.af.single.a ^= self.de.single.d,
                    R8::E => self.af.single.a ^= self.de.single.e,
                    R8::H => self.af.single.a ^= self.hl.single.h,
                    R8::L => self.af.single.a ^= self.hl.single.l,
                    R8::HLRef => self.af.single.a ^= bus.read_u8(self.hl.hl),
                    R8::A => self.af.single.a ^= self.af.single.a,
                }
                let z = self.af.single.a == 0;
                let n = false;
                let h = false;
                let c = false;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
            },
            Instruction::CP(reg) => unsafe {
                let a = self.af.single.a;
                let n = match reg {
                    R8::B => self.bc.single.b,
                    R8::C => self.bc.single.c,
                    R8::D => self.de.single.d,
                    R8::E => self.de.single.e,
                    R8::H => self.hl.single.h,
                    R8::L => self.hl.single.l,
                    R8::HLRef => bus.read_u8(self.hl.hl),
                    R8::A => self.af.single.a,
                };
                let z = a == n;
                let c = a < n;
                let h = (a & 0xf) < (n & 0xf);
                let n = true;
                self.af.single.f =
                    ((z as u8) << 7) | ((n as u8) << 6) | ((h as u8) << 5) | ((c as u8) << 4);
            },
            Instruction::SBC(reg) => unsafe {
                let a = self.af.single.a;
                let n = match reg {
                    R8::B => self.bc.single.b,
                    R8::C => self.bc.single.c,
                    R8::D => self.de.single.d,
                    R8::E => self.de.single.e,
                    R8::H => self.hl.single.h,
                    R8::L => self.hl.single.l,
                    R8::HLRef => bus.read_u8(self.hl.hl),
                    R8::A => self.af.single.a,
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
        }
    }
}

impl Display for CPU {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            write!(
                f,
                "CPU {{\n\ti: {:2X},\tr: {:2X},\n\tpc: {:4X},\tsp: {:4X},\n\ta: {:2X},\tb: {:2X},\tc: {:2X},\td: {:2X},\te: {:2X},\tf: {:2X},\th: {:2X},\tl: {:2X},\n\taf: {:4X},\tbc: {:4X},\tde: {:4X},\thl: {:4X},\n\tm: {:2X},\tt: {:2X}\n}}\n",
                self.i, self.r,
                self.pc, self.sp,
                self.af.single.a, self.bc.single.b, self.bc.single.c, self.bc.single.c, self.de.single.d, self.de.single.e, self.hl.single.h, self.hl.single.l,
                self.af.af, self.bc.bc, self.de.de, self.hl.hl,
                self.m, self.t,
            )?;
        }
        Ok(())
    }
}
