use std::{fmt::Display, rc::Rc, sync::Mutex};

use crate::{
    bus::Bus,
    instruction::{
        ALUOperand, Conditions, Instruction, LoadOperand, LoadTarget16, LoadTarget8, R16Mem,
        StackOperand, StoreSource,
    },
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
    pub z: bool,
    pub c: bool,

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
            c: false,
            z: true,
            halted: false,
        }
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
        self.c = false;
        self.z = true;
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
            Instruction::JR(offset, Some(cond)) => {
                let should_jump = match cond {
                    Conditions::Z => self.z,
                    Conditions::NZ => !self.z,
                    Conditions::C => self.c,
                    Conditions::NC => !self.c,
                };
                if should_jump {
                    self.pc = (self.pc as i32 + offset as i32) as u16;
                    self.m += 3;
                    self.t += 12;
                } else {
                    self.m += 2;
                    self.t += 8;
                }
            }
            Instruction::Ret(None) => {
                self.pc = bus.read_u16(self.sp);
                self.sp += 2;
                self.m += 3;
                self.t += 16;
            }
            Instruction::Ret(Some(cond)) => {
                let should_ret = match cond {
                    Conditions::Z => self.z,
                    Conditions::NZ => !self.z,
                    Conditions::C => self.c,
                    Conditions::NC => !self.c,
                };
                if should_ret {
                    self.pc = bus.read_u16(self.sp);
                }
                self.sp += 2;
                self.m += 3;
                self.t += 16;
            }
            Instruction::Load16Mem(R16Mem::BC) => unsafe {
                self.af.single.a = bus.read_u8(self.bc.bc);
                self.m += 2;
                self.t += 8;
            },
            Instruction::Load16Mem(R16Mem::DE) => unsafe {
                self.af.single.a = bus.read_u8(self.de.de);
                self.m += 2;
                self.t += 8;
            },
            Instruction::Load16Mem(R16Mem::HLInc) => unsafe {
                self.af.single.a = bus.read_u8(self.hl.hl);
                self.hl.hl += 1;
                self.m += 2;
                self.t += 8;
            },
            Instruction::Load16Mem(R16Mem::HLDec) => unsafe {
                self.af.single.a = bus.read_u8(self.hl.hl);
                self.hl.hl -= 1;
                self.m += 2;
                self.t += 8;
            },
            Instruction::Load16Imm(LoadTarget16::SP, val) => {
                self.sp = val;
                self.m += 3;
                self.t += 12;
            }
            Instruction::Load16Imm(LoadTarget16::HL, val) => {
                self.hl.hl = val;
                self.m += 3;
                self.t += 12;
            }
            Instruction::Load8Imm(LoadTarget8::A, val) => {
                self.af.single.a = val;
                self.m += 4;
                self.t += 8;
            }
            Instruction::Load8(LoadOperand::A, LoadOperand::HLRef) => unsafe {
                self.af.single.a = bus.read_u8(self.hl.hl);
                self.m += 3;
                self.t += 12;
            },
            Instruction::Store8H(addr, StoreSource::A) => unsafe {
                bus.write_u8(addr, self.af.single.a);
                self.m += 4;
                self.t += 8;
            },
            Instruction::Push(operand) => unsafe {
                self.sp -= 2;
                match operand {
                    StackOperand::AF => bus.write_u16(self.sp, self.af.af),
                    StackOperand::BC => bus.write_u16(self.sp, self.bc.bc),
                    StackOperand::DE => bus.write_u16(self.sp, self.de.de),
                    StackOperand::HL => bus.write_u16(self.sp, self.hl.hl),
                }
                self.m += 4;
                self.t += 16;
            },
            Instruction::Pop(operand) => {
                match operand {
                    StackOperand::AF => self.af.af = bus.read_u16(self.sp),
                    StackOperand::BC => self.bc.bc = bus.read_u16(self.sp),
                    StackOperand::DE => self.de.de = bus.read_u16(self.sp),
                    StackOperand::HL => self.hl.hl = bus.read_u16(self.sp),
                }
                self.sp += 2;
                self.m += 3;
                self.t += 12;
            }

            Instruction::OR(_, reg) => unsafe {
                match reg as ALUOperand {
                    ALUOperand::B => self.af.single.a |= self.bc.single.b,
                    ALUOperand::C => self.af.single.a |= self.bc.single.c,
                    ALUOperand::D => self.af.single.a |= self.de.single.d,
                    ALUOperand::E => self.af.single.a |= self.de.single.e,
                    ALUOperand::H => self.af.single.a |= self.hl.single.h,
                    ALUOperand::L => self.af.single.a |= self.hl.single.l,
                    ALUOperand::HLRef => self.af.single.a |= bus.read_u8(self.hl.hl),
                    ALUOperand::A => {}
                }
                self.z = self.af.single.a == 0;
                self.c = false;

                self.m += 1;
                self.t += 4;
            },
            _ => unimplemented!("instruction: {}", decoded),
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
