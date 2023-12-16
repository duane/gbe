use std::{fmt::Display, rc::Rc, sync::Mutex};

use crate::{bus::Bus, instruction::Instruction};

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
    pub t: u16,

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

        self.bus.lock().unwrap().reset();
    }

    pub fn execute_single_instruction(&mut self) {
        let bus = self.bus.lock().unwrap();
        let insn = bus.read_u8(self.pc);
        self.pc += 1;
        let size = Instruction::size_header(insn);
        let mut buf = vec![0u8; (size + 1).into()];
        buf[0] = insn;
        for i in 0..(size as usize) {
            buf[i + 1] = bus.read_u8(self.pc);
        }
        let decoded = Instruction::from_u8_slice(buf.as_slice(), 0);
        match decoded {
            Instruction::Nop => {
                self.m += 1;
                self.t += 4;
            }
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
