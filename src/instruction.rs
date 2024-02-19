// https://gbdev.io/gb-opcodes/optables/
// https://gbdev.io/pandocs/CPU_Instruction_Set.html

use std::fmt::Display;
use thiserror::Error;

type A16 = u16;
type A8 = u8;
type N8 = u8;
type N16 = u16;
type E8 = i8;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Tgt3 {
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
}

impl Display for Tgt3 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tgt3::T0 => write!(f, "$00"),
            Tgt3::T1 => write!(f, "$08"),
            Tgt3::T2 => write!(f, "$10"),
            Tgt3::T3 => write!(f, "$18"),
            Tgt3::T4 => write!(f, "$20"),
            Tgt3::T5 => write!(f, "$28"),
            Tgt3::T6 => write!(f, "$30"),
            Tgt3::T7 => write!(f, "$38"),
        }
    }
}

impl Tgt3 {
    pub fn as_operand(&self) -> u8 {
        match self {
            Tgt3::T0 => 0b000,
            Tgt3::T1 => 0b001,
            Tgt3::T2 => 0b010,
            Tgt3::T3 => 0b011,
            Tgt3::T4 => 0b100,
            Tgt3::T5 => 0b101,
            Tgt3::T6 => 0b110,
            Tgt3::T7 => 0b111,
        }
    }

    pub fn from_operand(operand: u8) -> Self {
        match operand {
            0b000 => Tgt3::T0,
            0b001 => Tgt3::T1,
            0b010 => Tgt3::T2,
            0b011 => Tgt3::T3,
            0b100 => Tgt3::T4,
            0b101 => Tgt3::T5,
            0b110 => Tgt3::T6,
            0b111 => Tgt3::T7,
            _ => unimplemented!("Tgt3::from_operand({:#04x})", operand),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BitRef {
    B0,
    B1,
    B2,
    B3,
    B4,
    B5,
    B6,
    B7,
}

impl Display for BitRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BitRef::B0 => write!(f, "0"),
            BitRef::B1 => write!(f, "1"),
            BitRef::B2 => write!(f, "2"),
            BitRef::B3 => write!(f, "3"),
            BitRef::B4 => write!(f, "4"),
            BitRef::B5 => write!(f, "5"),
            BitRef::B6 => write!(f, "6"),
            BitRef::B7 => write!(f, "7"),
        }
    }
}

impl BitRef {
    pub fn encode(&self) -> u8 {
        match self {
            BitRef::B0 => 0b000,
            BitRef::B1 => 0b001,
            BitRef::B2 => 0b010,
            BitRef::B3 => 0b011,
            BitRef::B4 => 0b100,
            BitRef::B5 => 0b101,
            BitRef::B6 => 0b110,
            BitRef::B7 => 0b111,
        }
    }

    pub fn decode(encoded: u8) -> Self {
        assert!(encoded < 8, "BitRef::decode({:#04x})", encoded);
        match encoded {
            0b000 => BitRef::B0,
            0b001 => BitRef::B1,
            0b010 => BitRef::B2,
            0b011 => BitRef::B3,
            0b100 => BitRef::B4,
            0b101 => BitRef::B5,
            0b110 => BitRef::B6,
            0b111 => BitRef::B7,
            _ => unimplemented!("BitRef::decode({:#04x})", encoded),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum R16 {
    R16BC,
    R16DE,
    R16HL,
    R16Sp,
}

impl R16 {
    pub fn as_operand(&self) -> u8 {
        match self {
            R16::R16BC => 0b00,
            R16::R16DE => 0b01,
            R16::R16HL => 0b10,
            R16::R16Sp => 0b11,
        }
    }
    pub fn from_operand(operand: u8) -> Self {
        match operand {
            0b00 => R16::R16BC,
            0b01 => R16::R16DE,
            0b10 => R16::R16HL,
            0b11 => R16::R16Sp,
            _ => unimplemented!("R16::from_operand({:#04x})", operand),
        }
    }
}

impl Display for R16 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            R16::R16BC => write!(f, "bc"),
            R16::R16DE => write!(f, "de"),
            R16::R16HL => write!(f, "hl"),
            R16::R16Sp => write!(f, "sp"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum R16Mem {
    R16MemBC,
    R16MemDE,
    R16MemHLInc,
    R16MemHLDec,
}

impl R16Mem {
    pub fn as_operand(&self) -> u8 {
        match self {
            R16Mem::R16MemBC => 0b00,
            R16Mem::R16MemDE => 0b01,
            R16Mem::R16MemHLInc => 0b10,
            R16Mem::R16MemHLDec => 0b11,
        }
    }

    pub fn from_operand(operand: u8) -> Self {
        match operand {
            0b00 => R16Mem::R16MemBC,
            0b01 => R16Mem::R16MemDE,
            0b10 => R16Mem::R16MemHLInc,
            0b11 => R16Mem::R16MemHLDec,
            _ => unimplemented!("R16Mem::from_operand({:#04x})", operand),
        }
    }
}

impl Display for R16Mem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            R16Mem::R16MemBC => write!(f, "[be]"),
            R16Mem::R16MemDE => write!(f, "[de]"),
            R16Mem::R16MemHLInc => write!(f, "[hl+]"),
            R16Mem::R16MemHLDec => write!(f, "[hl-]"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum R8 {
    R8B,
    R8C,
    R8D,
    R8E,
    R8H,
    R8L,
    R8HLRef,
    R8A,
}

impl Display for R8 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            R8::R8B => write!(f, "b"),
            R8::R8C => write!(f, "c"),
            R8::R8D => write!(f, "d"),
            R8::R8E => write!(f, "e"),
            R8::R8H => write!(f, "h"),
            R8::R8L => write!(f, "l"),
            R8::R8HLRef => write!(f, "[hl]"),
            R8::R8A => write!(f, "a"),
        }
    }
}

impl R8 {
    pub fn as_operand(&self) -> u8 {
        match self {
            R8::R8B => 0b000,
            R8::R8C => 0b001,
            R8::R8D => 0b010,
            R8::R8E => 0b011,
            R8::R8H => 0b100,
            R8::R8L => 0b101,
            R8::R8HLRef => 0b110,
            R8::R8A => 0b111,
        }
    }

    pub fn from_operand(operand: u8) -> Self {
        match operand {
            0b000 => R8::R8B,
            0b001 => R8::R8C,
            0b010 => R8::R8D,
            0b011 => R8::R8E,
            0b100 => R8::R8H,
            0b101 => R8::R8L,
            0b110 => R8::R8HLRef,
            0b111 => R8::R8A,
            _ => unimplemented!("LoadOperand::from_operand({:#04x})", operand),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum R16Stack {
    R16StackBC,
    R16StackDE,
    R16StackHL,
    R16StackAF,
}

impl R16Stack {
    pub fn as_operand(&self) -> u8 {
        match self {
            R16Stack::R16StackBC => 0b00,
            R16Stack::R16StackDE => 0b01,
            R16Stack::R16StackHL => 0b10,
            R16Stack::R16StackAF => 0b11,
        }
    }

    pub fn from_operand(operand: u8) -> Self {
        match operand {
            0b00 => R16Stack::R16StackBC,
            0b01 => R16Stack::R16StackDE,
            0b10 => R16Stack::R16StackHL,
            0b11 => R16Stack::R16StackAF,
            _ => unimplemented!("R16Stack::from_operand({:#04x})", operand),
        }
    }
}

impl Display for R16Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            R16Stack::R16StackBC => write!(f, "bc"),
            R16Stack::R16StackDE => write!(f, "de"),
            R16Stack::R16StackHL => write!(f, "hl"),
            R16Stack::R16StackAF => write!(f, "af"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Cond {
    CondNZ,
    CondZ,
    CondNC,
    CondC,
}

impl Cond {
    pub fn from_operand(operand: u8) -> Self {
        match operand {
            0b00 => Cond::CondNZ,
            0b01 => Cond::CondZ,
            0b10 => Cond::CondNC,
            0b11 => Cond::CondC,
            _ => unimplemented!("Conditions::from_operand({:#04x})", operand),
        }
    }

    pub fn as_operand(&self) -> u8 {
        match self {
            Cond::CondNZ => 0b00,
            Cond::CondZ => 0b01,
            Cond::CondNC => 0b10,
            Cond::CondC => 0b11,
        }
    }
}

impl Display for Cond {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Cond::CondNZ => write!(f, "nz"),
            Cond::CondZ => write!(f, "z"),
            Cond::CondNC => write!(f, "nc"),
            Cond::CondC => write!(f, "c"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Instruction {
    Nop,                    // 0x0
    LoadR16N16(R16, N16),   // 0x1
    StoreAR16Mem(R16Mem),   // 0x2
    IncR16(R16),            // 0x3
    IncR8(R8),              // 0x4
    DecR8(R8),              // 0x5
    LoadR8N8(R8, N8),       // 0x6
    RlcA,                   // 0x7
    StoreSPA16(A16),        // 0x8
    AddHLR16(R16),          // 0x9
    LoadAR16Mem(R16Mem),    // 0xA
    DecR16(R16),            // 0xB
    RrcA,                   // 0xF
    StopN8(N8),             // 0x10
    RlA,                    // 0x17
    JumpNear(E8),           // 0x18
    RrA,                    // 0x1F
    JumpNearCond(Cond, E8), // 0x20
    Daa,                    // 0x27
    Cpl,                    // 0x2f
    Scf,                    // 0x37
    Ccf,                    // 0x3f
    LoadR8R8(R8, R8),       // 0x40
    Halt,                   // 0x76
    AddR8(R8),              // 0x80
    AdcR8(R8),              // 0x88
    SubR8(R8),              // 0x90
    SbcR8(R8),              // 0x98
    AndR8(R8),              // 0xA0
    XorR8(R8),              // 0xA8
    OrR8(R8),               // 0xB0
    CpR8(R8),               // 0xB8
    RetCond(Cond),          // 0xc0
    PopR16Stack(R16Stack),  // 0xc1
    JumpFarCond(Cond, A16), // 0xc2
    JumpFar(A16),           // 0xc3
    CallCondA16(Cond, A16), // 0xc4
    PushR16Stack(R16Stack), // 0xC5
    AddAN8(N8),             // 0xC6
    Rst(Tgt3),              // 0xc6
    Ret,                    // 0xc0
    // prefix // 0xcb
    CallA16(A16),   // 0xcd
    AdcAN8(N8),     // 0xce
    SubAN8(N8),     // 0xD6
    Reti,           // 0xD9
    SbcAN8(N8),     // 0xDE
    StoreAA8H(A8),  // 0xE0
    StoreACH,       // 0xE2
    AndAN8(N8),     // 0xE6
    AddSPN8(N8),    // 0xE8
    JumpFarHL,      // 0xE9
    StoreAA16(A16), // 0xEA
    XorAN8(N8),     // 0xEE
    LoadAA8H(N8),   // 0xF0
    LoadACH,        // 0xF2
    Di,             // 0xF3
    OrAN8(N8),      // 0xF6
    LoadHLSPN8(N8), // 0xF8
    LoadHLSP,       // 0xF9
    LoadAA16(A16),  // 0xFA
    Ei,             // 0xfb
    // ei // 0xFB
    CpAN8(N8), // 0xFE

    // Prefixed instructions
    Rlc(R8),         // 0x00
    Rrc(R8),         // 0x08
    Rl(R8),          // 0x10
    Rr(R8),          // 0x18
    Sla(R8),         // 0x20
    Sra(R8),         // 0x28
    Swap(R8),        // 0x30
    Srl(R8),         // 0x38
    Bit(BitRef, R8), // 0x40
    Res(BitRef, R8), // 0x80
    Set(BitRef, R8), // 0xC0
}

pub const ILLEGAL_INSTRUCTIONS: [u8; 11] = [
    0xd3, 0xdb, 0xdd, 0xe3, 0xe4, 0xeb, 0xec, 0xed, 0xf4, 0xfc, 0xfd,
];

#[derive(Error, Debug)]
pub enum InstructionError {
    #[error("Unknown Instruction - {0:#02x}")]
    Illegal(u8),
    #[error("Unknown instruction - {0:#02x}")]
    Unknown(u8),
    #[error("Instruction is incomplete - {0:02x?}")]
    Incomplete(Vec<u8>),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Nop => write!(f, "nop"),
            Instruction::Halt => write!(f, "halt"),
            Instruction::StopN8(val) => write!(f, "stop {:#02x}", val),

            Instruction::LoadR16N16(r16, imm16) => write!(f, "ld {}, {}", r16, imm16),
            Instruction::LoadAR16Mem(r16mem) => write!(f, "ld a, [{}]", r16mem),
            Instruction::LoadAA16(imm16) => write!(f, "ld a, [{}]", imm16),
            Instruction::LoadR8N8(r8, imm8) => write!(f, "ld {}, {}", r8, imm8),
            Instruction::LoadR8R8(r8_dst, r8_src) => write!(f, "ld {}, {}", r8_dst, r8_src),
            Instruction::LoadACH => write!(f, "ldh a, [c]"),
            Instruction::LoadAA8H(imm8) => write!(f, "ldh a, [${:02x}]", imm8),

            Instruction::StoreAA16(imm16) => write!(f, "ld [{}], A", imm16),
            Instruction::StoreACH => write!(f, "ldh [c], a"),
            Instruction::StoreAR16Mem(dest) => write!(f, "ld [{}], a", dest),
            Instruction::StoreAA8H(imm8) => write!(f, "ldh [${:02x}], a", imm8),
            Instruction::StoreSPA16(imm16) => write!(f, "ld [{}], sp", imm16),

            Instruction::PushR16Stack(operand) => write!(f, "push {}", operand),
            Instruction::PopR16Stack(operand) => write!(f, "pop {}", operand),
            Instruction::CallA16(a16) => write!(f, "call #{:04x}", a16),
            Instruction::CallCondA16(cond, a16) => write!(f, "call {}, ${:04x}", cond, a16),
            Instruction::JumpNear(e8) => write!(f, "jr {:+}", e8),
            Instruction::JumpNearCond(e8, cond) => write!(f, "jr {}, {:+}", cond, e8),
            Instruction::JumpFar(a16) => write!(f, "jp ${:04x}", a16),
            Instruction::JumpFarCond(cond, a16) => write!(f, "jp {}, ${:04x}", cond, a16),
            Instruction::Ret => write!(f, "ret"),
            Instruction::RetCond(cond) => write!(f, "ret {}", cond),
            Instruction::IncR16(r16) => write!(f, "inc {}", r16),
            Instruction::IncR8(r8) => write!(f, "inc {}", r8),
            Instruction::DecR16(r16) => write!(f, "dec {}", r16),
            Instruction::DecR8(r8) => write!(f, "dec {}", r8),
            Instruction::AddHLR16(r16) => write!(f, "add hl, {}", r16),

            Instruction::AddAN8(n8) => write!(f, "add a, {}", n8),
            Instruction::AdcAN8(n8) => write!(f, "adc a, {}", n8),
            Instruction::SubAN8(n8) => write!(f, "sub a, {}", n8),
            Instruction::SbcAN8(n8) => write!(f, "sbc a, {}", n8),
            Instruction::AndAN8(n8) => write!(f, "and A, {}", n8),
            Instruction::XorAN8(n8) => write!(f, "xor A, {}", n8),
            Instruction::OrAN8(n8) => write!(f, "or A, {}", n8),
            Instruction::CpAN8(n8) => write!(f, "cp A, {}", n8),

            Instruction::AddR8(source) => write!(f, "add a, {}", source),
            Instruction::AdcR8(source) => write!(f, "adc a, {}", source),
            Instruction::SubR8(source) => write!(f, "sub a, {}", source),
            Instruction::SbcR8(source) => write!(f, "sbc a, {}", source),
            Instruction::AndR8(source) => write!(f, "and A, {}", source),
            Instruction::XorR8(source) => write!(f, "xor A, {}", source),
            Instruction::OrR8(source) => write!(f, "or A, {}", source),
            Instruction::CpR8(source) => write!(f, "cp A, {}", source),

            Instruction::RlcA => write!(f, "rlc a"),
            Instruction::RrcA => write!(f, "rrc a"),
            Instruction::RlA => write!(f, "rl a"),
            Instruction::RrA => write!(f, "rr a"),
            Instruction::Daa => write!(f, "daa"),
            Instruction::Cpl => write!(f, "cpl"),
            Instruction::Scf => write!(f, "scf"),
            Instruction::Ccf => write!(f, "ccf"),

            Instruction::Rlc(r8) => write!(f, "rlc {}", r8),
            Instruction::Rrc(r8) => write!(f, "rrc {}", r8),
            Instruction::Rl(r8) => write!(f, "rl {}", r8),
            Instruction::Rr(r8) => write!(f, "rr {}", r8),
            Instruction::Sla(r8) => write!(f, "sla {}", r8),
            Instruction::Sra(r8) => write!(f, "sra {}", r8),
            Instruction::Swap(r8) => write!(f, "swap {}", r8),
            Instruction::Srl(r8) => write!(f, "srl {}", r8),

            Instruction::Rst(tgt3) => write!(f, "rst {}", tgt3),

            Instruction::Bit(b3, r8) => write!(f, "bit {}, {}", b3, r8),
            Instruction::Res(b3, r8) => write!(f, "res {}, {}", b3, r8),
            Instruction::Set(b3, r8) => write!(f, "set {}, {}", b3, r8),

            Instruction::Reti => write!(f, "reti"),
            Instruction::Ei => write!(f, "ei"),

            Instruction::Di => write!(f, "di"),

            Instruction::AddSPN8(n8) => write!(f, "add sp, {}", n8),
            Instruction::JumpFarHL => write!(f, "jp [hl]"),
            Instruction::LoadHLSPN8(n8) => write!(f, "ld hl, sp+{}", n8),
            Instruction::LoadHLSP => write!(f, "ld sp, hl"),
        }
    }
}

const OPCODE_SIZE_LOOKUP: [u8; 256] = [
    1, 3, 1, 1, 1, 1, 2, 1, 3, 1, 1, 1, 1, 1, 2, 1, 2, 3, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1,
    2, 3, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1, 2, 3, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 3, 3, 3, 1, 2, 1, 1, 1, 3, 2, 3, 3, 2, 1, 1, 1, 3, 0, 3, 1, 2, 1, 1, 1, 3, 0, 3, 0, 2, 1,
    2, 1, 1, 0, 0, 1, 2, 1, 2, 1, 3, 0, 0, 0, 2, 1, 2, 1, 1, 1, 0, 1, 2, 1, 2, 1, 3, 1, 0, 0, 2, 1,
];

impl Instruction {
    pub fn size_header(opcode: u8) -> Result<u8, InstructionError> {
        let size = OPCODE_SIZE_LOOKUP[opcode as usize];
        if size == 0 {
            return Err(InstructionError::Illegal(opcode).into());
        }
        Ok(size)
    }

    pub fn encode(&self) -> Vec<u8> {
        match self {
            Instruction::Nop => vec![self.opcode()],
            Instruction::Halt => vec![self.opcode()],
            Instruction::StopN8(n8) => vec![self.opcode(), *n8],
            Instruction::Reti => vec![self.opcode()],
            Instruction::Di => vec![self.opcode()],
            Instruction::Ei => vec![self.opcode()],
            Instruction::CallA16(a16) => vec![self.opcode(), *a16 as u8, (*a16 >> 8) as u8],
            Instruction::CallCondA16(_, a16) => vec![self.opcode(), *a16 as u8, (*a16 >> 8) as u8],
            Instruction::JumpNear(e8) => vec![self.opcode(), *e8 as u8],
            Instruction::JumpNearCond(_, e8) => vec![self.opcode(), *e8 as u8],
            Instruction::JumpFar(a16) => vec![self.opcode(), *a16 as u8, (*a16 >> 8) as u8],
            Instruction::JumpFarCond(_, a16) => vec![self.opcode(), *a16 as u8, (*a16 >> 8) as u8],
            Instruction::Ret => vec![self.opcode()],
            Instruction::RetCond(_) => vec![self.opcode()],
            Instruction::LoadR16N16(_, val) => vec![self.opcode(), *val as u8, (*val >> 8) as u8],
            Instruction::LoadAR16Mem(_) => vec![self.opcode()],
            Instruction::LoadAA16(addr) => vec![self.opcode(), *addr as u8, (*addr >> 8) as u8],
            Instruction::LoadR8N8(_, val) => vec![self.opcode(), *val],
            Instruction::LoadR8R8(_, _) => vec![self.opcode()],
            Instruction::LoadACH => vec![self.opcode()],
            Instruction::LoadAA8H(addr) => vec![self.opcode(), *addr],
            Instruction::StoreACH => vec![self.opcode()],
            Instruction::StoreAA16(addr) => vec![self.opcode(), *addr as u8, (*addr >> 8) as u8],
            Instruction::StoreAR16Mem(_) => vec![self.opcode()],
            Instruction::StoreAA8H(addr) => vec![self.opcode(), (*addr & 0xff) as u8],
            Instruction::StoreSPA16(imm16) => {
                vec![self.opcode(), *imm16 as u8, (*imm16 >> 8) as u8]
            }
            Instruction::PushR16Stack(_) => vec![self.opcode()],
            Instruction::PopR16Stack(_) => vec![self.opcode()],
            Instruction::IncR16(_) => vec![self.opcode()],
            Instruction::IncR8(_) => vec![self.opcode()],
            Instruction::DecR16(_) => vec![self.opcode()],
            Instruction::DecR8(_) => vec![self.opcode()],
            Instruction::AddHLR16(_) => vec![self.opcode()],
            Instruction::AddAN8(n8) => vec![self.opcode(), *n8],
            Instruction::AdcAN8(n8) => vec![self.opcode(), *n8],
            Instruction::SubAN8(n8) => vec![self.opcode(), *n8],
            Instruction::SbcAN8(n8) => vec![self.opcode(), *n8],
            Instruction::AndAN8(n8) => vec![self.opcode(), *n8],
            Instruction::XorAN8(n8) => vec![self.opcode(), *n8],
            Instruction::OrAN8(n8) => vec![self.opcode(), *n8],
            Instruction::CpAN8(n8) => vec![self.opcode(), *n8],
            Instruction::AddR8(_) => vec![self.opcode()],
            Instruction::AdcR8(_) => vec![self.opcode()],
            Instruction::SubR8(_) => vec![self.opcode()],
            Instruction::SbcR8(_) => vec![self.opcode()],
            Instruction::AndR8(_) => vec![self.opcode()],
            Instruction::XorR8(_) => vec![self.opcode()],
            Instruction::OrR8(_) => vec![self.opcode()],
            Instruction::CpR8(_) => vec![self.opcode()],
            Instruction::RlcA => vec![self.opcode()],
            Instruction::RrcA => vec![self.opcode()],
            Instruction::RlA => vec![self.opcode()],
            Instruction::RrA => vec![self.opcode()],
            Instruction::Daa => vec![self.opcode()],
            Instruction::Cpl => vec![self.opcode()],
            Instruction::Scf => vec![self.opcode()],
            Instruction::Ccf => vec![self.opcode()],
            Instruction::Rlc(_) => vec![0xCB, self.opcode()],
            Instruction::Rrc(_) => vec![0xCB, self.opcode()],
            Instruction::Rl(_) => vec![0xCB, self.opcode()],
            Instruction::Rr(_) => vec![0xCB, self.opcode()],
            Instruction::Sla(_) => vec![0xCB, self.opcode()],
            Instruction::Sra(_) => vec![0xCB, self.opcode()],
            Instruction::Swap(_) => vec![0xCB, self.opcode()],
            Instruction::Srl(_) => vec![0xCB, self.opcode()],
            Instruction::Bit(_, _) => vec![0xCB, self.opcode()],
            Instruction::Res(_, _) => vec![0xCB, self.opcode()],
            Instruction::Set(_, _) => vec![0xCB, self.opcode()],
            Instruction::Rst(_) => vec![self.opcode()],
            Instruction::AddSPN8(n8) => vec![self.opcode(), *n8],
            Instruction::JumpFarHL => vec![self.opcode()],
            Instruction::LoadHLSPN8(n8) => vec![self.opcode(), *n8],
            Instruction::LoadHLSP => vec![self.opcode()],
        }
    }

    pub fn size(&self) -> u8 {
        match self {
            Instruction::Nop => 1,
            Instruction::Halt => 1,
            Instruction::StopN8(_) => 2,
            Instruction::CallA16(_) => 3,
            Instruction::CallCondA16(_, _) => 3,
            Instruction::JumpNear(_) | Instruction::JumpNearCond(_, _) => 2,
            Instruction::JumpFar(_) | Instruction::JumpFarCond(_, _) => 3,
            Instruction::Reti => 1,
            Instruction::Ei => 1,
            Instruction::Di => 1,
            Instruction::Ret => 1,
            Instruction::RetCond(_) => 1,

            Instruction::LoadAR16Mem(_) => 1,
            Instruction::LoadR16N16(_, _) => 3,
            Instruction::LoadAA16(_) => 3,
            Instruction::LoadR8N8(_, _) => 2,
            Instruction::LoadR8R8(_, _) => 1,
            Instruction::LoadACH => 1,
            Instruction::LoadAA8H(_) => 2,
            Instruction::StoreAA16(_) => 3,
            Instruction::StoreACH => 1,
            Instruction::StoreAR16Mem(_) => 1,
            Instruction::StoreAA8H(_) => 2,
            Instruction::StoreSPA16(_) => 3,
            Instruction::PushR16Stack(_) => 1,
            Instruction::PopR16Stack(_) => 1,

            Instruction::IncR16(_) => 1,
            Instruction::IncR8(_) => 1,
            Instruction::DecR16(_) => 1,
            Instruction::DecR8(_) => 1,
            Instruction::AddHLR16(_) => 1,

            Instruction::AddAN8(_) => 2,
            Instruction::AdcAN8(_) => 2,
            Instruction::SubAN8(_) => 2,
            Instruction::SbcAN8(_) => 2,
            Instruction::AndAN8(_) => 2,
            Instruction::XorAN8(_) => 2,
            Instruction::OrAN8(_) => 2,
            Instruction::CpAN8(_) => 2,

            Instruction::AddR8(_) => 1,
            Instruction::AdcR8(_) => 1,
            Instruction::SubR8(_) => 1,
            Instruction::SbcR8(_) => 1,
            Instruction::AndR8(_) => 1,
            Instruction::XorR8(_) => 1,
            Instruction::OrR8(_) => 1,
            Instruction::CpR8(_) => 1,

            Instruction::RlcA => 1,
            Instruction::RrcA => 1,
            Instruction::RlA => 1,
            Instruction::RrA => 1,
            Instruction::Daa => 1,
            Instruction::Cpl => 1,
            Instruction::Scf => 1,
            Instruction::Ccf => 1,

            Instruction::Rlc(_) => 2,
            Instruction::Rrc(_) => 2,
            Instruction::Rl(_) => 2,
            Instruction::Rr(_) => 2,
            Instruction::Sla(_) => 2,
            Instruction::Sra(_) => 2,
            Instruction::Swap(_) => 2,
            Instruction::Srl(_) => 2,

            Instruction::Bit(_, _) => 2,
            Instruction::Res(_, _) => 2,
            Instruction::Set(_, _) => 2,

            Instruction::Rst(_) => 1,

            Instruction::AddSPN8(_) => 2,
            Instruction::JumpFarHL => 1,
            Instruction::LoadHLSPN8(_) => 2,
            Instruction::LoadHLSP => 1,
        }
    }

    pub fn m_cycles(&self) -> (u8, u8) {
        let (taken, not_taken) = self.t_cycles();
        (taken / 4, not_taken / 4)
    }

    pub fn t_cycles(&self) -> (u8, u8) {
        match self {
            Instruction::Nop => (4, 4),
            Instruction::Halt => (4, 4),
            Instruction::StopN8(_) => (4, 4),
            Instruction::Reti => (16, 16),
            Instruction::Ei => (4, 4),
            Instruction::Di => (4, 4),
            Instruction::CallA16(_) => (24, 24),
            Instruction::CallCondA16(_, _) => (24, 12),
            Instruction::JumpNear(_) => (12, 8),
            Instruction::JumpNearCond(_, _) => (12, 8),
            Instruction::JumpFar(_) => (16, 12),
            Instruction::JumpFarCond(_, _) => (16, 12),
            Instruction::Ret => (16, 16),
            Instruction::RetCond(_) => (20, 8),

            Instruction::LoadAR16Mem(_) => (8, 8),
            Instruction::LoadR16N16(_, _) => (12, 12),
            Instruction::LoadAA16(_) => (16, 16),
            Instruction::LoadR8N8(_, _) => (8, 8),
            Instruction::LoadR8R8(R8::R8HLRef, _) => (8, 8),
            Instruction::LoadR8R8(_, R8::R8HLRef) => (8, 8),
            Instruction::LoadR8R8(_, _) => (4, 4),
            Instruction::LoadACH => (8, 8),
            Instruction::LoadAA8H(_) => (12, 12),
            Instruction::StoreAA16(_) => (16, 16),
            Instruction::StoreACH => (8, 8),
            Instruction::StoreAR16Mem(_) => (8, 8),
            Instruction::StoreAA8H(_) => (12, 12),
            Instruction::StoreSPA16(_) => (20, 20),
            Instruction::PushR16Stack(_) => (16, 16),
            Instruction::PopR16Stack(_) => (16, 16),

            Instruction::IncR16(_) => (8, 8),
            Instruction::IncR8(_) => (4, 4),
            Instruction::DecR16(_) => (8, 8),
            Instruction::DecR8(_) => (4, 4),
            Instruction::AddHLR16(_) => (12, 12),

            Instruction::AddAN8(_) => (8, 8),
            Instruction::AdcAN8(_) => (8, 8),
            Instruction::SubAN8(_) => (8, 8),
            Instruction::SbcAN8(_) => (8, 8),
            Instruction::AndAN8(_) => (8, 8),
            Instruction::XorAN8(_) => (8, 8),
            Instruction::OrAN8(_) => (8, 8),
            Instruction::CpAN8(_) => (8, 8),

            Instruction::AddR8(_) => (4, 4),
            Instruction::AdcR8(_) => (4, 4),
            Instruction::SubR8(_) => (4, 4),
            Instruction::SbcR8(_) => (4, 4),
            Instruction::AndR8(_) => (4, 4),
            Instruction::XorR8(_) => (4, 4),
            Instruction::OrR8(_) => (4, 4),
            Instruction::CpR8(_) => (4, 4),

            Instruction::RlcA => (4, 4),
            Instruction::RrcA => (4, 4),
            Instruction::RlA => (4, 4),
            Instruction::RrA => (4, 4),
            Instruction::Daa => (4, 4),
            Instruction::Cpl => (4, 4),
            Instruction::Scf => (4, 4),
            Instruction::Ccf => (4, 4),

            Instruction::Rlc(_) => (8, 8),
            Instruction::Rrc(_) => (8, 8),
            Instruction::Rl(_) => (8, 8),
            Instruction::Rr(_) => (8, 8),
            Instruction::Sla(_) => (8, 8),
            Instruction::Sra(_) => (8, 8),
            Instruction::Swap(_) => (8, 8),
            Instruction::Srl(_) => (8, 8),

            Instruction::Bit(_, _) => (8, 8),
            Instruction::Res(_, _) => (8, 8),
            Instruction::Set(_, _) => (8, 8),

            Instruction::Rst(_) => (16, 16),

            Instruction::AddSPN8(_) => (16, 16),
            Instruction::JumpFarHL => (4, 4),
            Instruction::LoadHLSPN8(_) => (12, 12),
            Instruction::LoadHLSP => (8, 8),
        }
    }

    fn read_u8_helper(buf: &[u8], addr: u16) -> u8 {
        buf[addr as usize]
    }

    fn read_u16_helper(buf: &[u8], addr: u16) -> u16 {
        let lo = buf[addr as usize];
        let hi = buf[(addr + 1) as usize];
        ((hi as u16) << 8) | (lo as u16)
    }

    fn opcode(&self) -> u8 {
        match self {
            Instruction::Nop => 0x0,
            Instruction::LoadR16N16(R16::R16BC, _) => 0x1,
            Instruction::StoreAR16Mem(R16Mem::R16MemBC) => 0x2,
            Instruction::IncR16(R16::R16BC) => 0x3,
            Instruction::IncR8(R8::R8B) => 0x4,
            Instruction::DecR8(R8::R8B) => 0x5,
            Instruction::LoadR8N8(R8::R8B, _) => 0x6,
            Instruction::RlcA => 0x7,
            Instruction::StoreSPA16(_) => 0x8,
            Instruction::AddHLR16(R16::R16BC) => 0x9,
            Instruction::LoadAR16Mem(R16Mem::R16MemBC) => 0xa,
            Instruction::DecR16(R16::R16BC) => 0xb,
            Instruction::IncR8(R8::R8C) => 0xc,
            Instruction::DecR8(R8::R8C) => 0xd,
            Instruction::LoadR8N8(R8::R8C, _) => 0xe,
            Instruction::RrcA => 0xf,
            Instruction::StopN8(_) => 0x10,
            Instruction::LoadR16N16(R16::R16DE, _) => 0x11,
            Instruction::StoreAR16Mem(R16Mem::R16MemDE) => 0x12,
            Instruction::IncR16(R16::R16DE) => 0x13,
            Instruction::IncR8(R8::R8D) => 0x14,
            Instruction::DecR8(R8::R8D) => 0x15,
            Instruction::LoadR8N8(R8::R8D, _) => 0x16,
            Instruction::RlA => 0x17,
            Instruction::JumpNear(_) => 0x18,
            Instruction::AddHLR16(R16::R16DE) => 0x19,
            Instruction::LoadAR16Mem(R16Mem::R16MemDE) => 0x1a,
            Instruction::DecR16(R16::R16DE) => 0x1b,
            Instruction::IncR8(R8::R8E) => 0x1c,
            Instruction::DecR8(R8::R8E) => 0x1d,
            Instruction::LoadR8N8(R8::R8E, _) => 0x1e,
            Instruction::RrA => 0x1f,
            Instruction::JumpNearCond(Cond::CondNZ, _) => 0x20,
            Instruction::LoadR16N16(R16::R16HL, _) => 0x21,
            Instruction::StoreAR16Mem(R16Mem::R16MemHLInc) => 0x22,
            Instruction::IncR16(R16::R16HL) => 0x23,
            Instruction::IncR8(R8::R8H) => 0x24,
            Instruction::DecR8(R8::R8H) => 0x25,
            Instruction::LoadR8N8(R8::R8H, _) => 0x26,
            Instruction::Daa => 0x27,
            Instruction::JumpNearCond(Cond::CondZ, _) => 0x28,
            Instruction::AddHLR16(R16::R16HL) => 0x29,
            Instruction::LoadAR16Mem(R16Mem::R16MemHLInc) => 0x2a,
            Instruction::DecR16(R16::R16HL) => 0x2b,
            Instruction::IncR8(R8::R8L) => 0x2c,
            Instruction::DecR8(R8::R8L) => 0x2d,
            Instruction::LoadR8N8(R8::R8L, _) => 0x2e,
            Instruction::Cpl => 0x2f,
            Instruction::JumpNearCond(Cond::CondNC, _) => 0x30,
            Instruction::LoadR16N16(R16::R16Sp, _) => 0x31,
            Instruction::StoreAR16Mem(R16Mem::R16MemHLDec) => 0x32,
            Instruction::IncR16(R16::R16Sp) => 0x33,
            Instruction::IncR8(R8::R8HLRef) => 0x34,
            Instruction::DecR8(R8::R8HLRef) => 0x35,
            Instruction::LoadR8N8(R8::R8HLRef, _) => 0x36,
            Instruction::Scf => 0x37,
            Instruction::JumpNearCond(Cond::CondC, _) => 0x38,
            Instruction::AddHLR16(R16::R16Sp) => 0x39,
            Instruction::LoadAR16Mem(R16Mem::R16MemHLDec) => 0x3a,
            Instruction::DecR16(R16::R16Sp) => 0x3b,
            Instruction::IncR8(R8::R8A) => 0x3c,
            Instruction::DecR8(R8::R8A) => 0x3d,
            Instruction::LoadR8N8(R8::R8A, _) => 0x3e,
            Instruction::Ccf => 0x3f,
            Instruction::LoadR8R8(R8::R8B, R8::R8B) => 0x40,
            Instruction::LoadR8R8(R8::R8B, R8::R8C) => 0x41,
            Instruction::LoadR8R8(R8::R8B, R8::R8D) => 0x42,
            Instruction::LoadR8R8(R8::R8B, R8::R8E) => 0x43,
            Instruction::LoadR8R8(R8::R8B, R8::R8H) => 0x44,
            Instruction::LoadR8R8(R8::R8B, R8::R8L) => 0x45,
            Instruction::LoadR8R8(R8::R8B, R8::R8HLRef) => 0x46,
            Instruction::LoadR8R8(R8::R8B, R8::R8A) => 0x47,
            Instruction::LoadR8R8(R8::R8C, R8::R8B) => 0x48,
            Instruction::LoadR8R8(R8::R8C, R8::R8C) => 0x49,
            Instruction::LoadR8R8(R8::R8C, R8::R8D) => 0x4a,
            Instruction::LoadR8R8(R8::R8C, R8::R8E) => 0x4b,
            Instruction::LoadR8R8(R8::R8C, R8::R8H) => 0x4c,
            Instruction::LoadR8R8(R8::R8C, R8::R8L) => 0x4d,
            Instruction::LoadR8R8(R8::R8C, R8::R8HLRef) => 0x4e,
            Instruction::LoadR8R8(R8::R8C, R8::R8A) => 0x4f,
            Instruction::LoadR8R8(R8::R8D, R8::R8B) => 0x50,
            Instruction::LoadR8R8(R8::R8D, R8::R8C) => 0x51,
            Instruction::LoadR8R8(R8::R8D, R8::R8D) => 0x52,
            Instruction::LoadR8R8(R8::R8D, R8::R8E) => 0x53,
            Instruction::LoadR8R8(R8::R8D, R8::R8H) => 0x54,
            Instruction::LoadR8R8(R8::R8D, R8::R8L) => 0x55,
            Instruction::LoadR8R8(R8::R8D, R8::R8HLRef) => 0x56,
            Instruction::LoadR8R8(R8::R8D, R8::R8A) => 0x57,
            Instruction::LoadR8R8(R8::R8E, R8::R8B) => 0x58,
            Instruction::LoadR8R8(R8::R8E, R8::R8C) => 0x59,
            Instruction::LoadR8R8(R8::R8E, R8::R8D) => 0x5a,
            Instruction::LoadR8R8(R8::R8E, R8::R8E) => 0x5b,
            Instruction::LoadR8R8(R8::R8E, R8::R8H) => 0x5c,
            Instruction::LoadR8R8(R8::R8E, R8::R8L) => 0x5d,
            Instruction::LoadR8R8(R8::R8E, R8::R8HLRef) => 0x5e,
            Instruction::LoadR8R8(R8::R8E, R8::R8A) => 0x5f,
            Instruction::LoadR8R8(R8::R8H, R8::R8B) => 0x60,
            Instruction::LoadR8R8(R8::R8H, R8::R8C) => 0x61,
            Instruction::LoadR8R8(R8::R8H, R8::R8D) => 0x62,
            Instruction::LoadR8R8(R8::R8H, R8::R8E) => 0x63,
            Instruction::LoadR8R8(R8::R8H, R8::R8H) => 0x64,
            Instruction::LoadR8R8(R8::R8H, R8::R8L) => 0x65,
            Instruction::LoadR8R8(R8::R8H, R8::R8HLRef) => 0x66,
            Instruction::LoadR8R8(R8::R8H, R8::R8A) => 0x67,
            Instruction::LoadR8R8(R8::R8L, R8::R8B) => 0x68,
            Instruction::LoadR8R8(R8::R8L, R8::R8C) => 0x69,
            Instruction::LoadR8R8(R8::R8L, R8::R8D) => 0x6a,
            Instruction::LoadR8R8(R8::R8L, R8::R8E) => 0x6b,
            Instruction::LoadR8R8(R8::R8L, R8::R8H) => 0x6c,
            Instruction::LoadR8R8(R8::R8L, R8::R8L) => 0x6d,
            Instruction::LoadR8R8(R8::R8L, R8::R8HLRef) => 0x6e,
            Instruction::LoadR8R8(R8::R8L, R8::R8A) => 0x6f,
            Instruction::LoadR8R8(R8::R8HLRef, R8::R8B) => 0x70,
            Instruction::LoadR8R8(R8::R8HLRef, R8::R8C) => 0x71,
            Instruction::LoadR8R8(R8::R8HLRef, R8::R8D) => 0x72,
            Instruction::LoadR8R8(R8::R8HLRef, R8::R8E) => 0x73,
            Instruction::LoadR8R8(R8::R8HLRef, R8::R8H) => 0x74,
            Instruction::LoadR8R8(R8::R8HLRef, R8::R8L) => 0x75,
            Instruction::Halt => 0x76,
            Instruction::LoadR8R8(R8::R8HLRef, R8::R8A) => 0x77,
            Instruction::LoadR8R8(R8::R8A, R8::R8B) => 0x78,
            Instruction::LoadR8R8(R8::R8A, R8::R8C) => 0x79,
            Instruction::LoadR8R8(R8::R8A, R8::R8D) => 0x7a,
            Instruction::LoadR8R8(R8::R8A, R8::R8E) => 0x7b,
            Instruction::LoadR8R8(R8::R8A, R8::R8H) => 0x7c,
            Instruction::LoadR8R8(R8::R8A, R8::R8L) => 0x7d,
            Instruction::LoadR8R8(R8::R8A, R8::R8HLRef) => 0x7e,
            Instruction::LoadR8R8(R8::R8A, R8::R8A) => 0x7f,
            Instruction::AddR8(R8::R8B) => 0x80,
            Instruction::AddR8(R8::R8C) => 0x81,
            Instruction::AddR8(R8::R8D) => 0x82,
            Instruction::AddR8(R8::R8E) => 0x83,
            Instruction::AddR8(R8::R8H) => 0x84,
            Instruction::AddR8(R8::R8L) => 0x85,
            Instruction::AddR8(R8::R8HLRef) => 0x86,
            Instruction::AddR8(R8::R8A) => 0x87,
            Instruction::AdcR8(R8::R8B) => 0x88,
            Instruction::AdcR8(R8::R8C) => 0x89,
            Instruction::AdcR8(R8::R8D) => 0x8a,
            Instruction::AdcR8(R8::R8E) => 0x8b,
            Instruction::AdcR8(R8::R8H) => 0x8c,
            Instruction::AdcR8(R8::R8L) => 0x8d,
            Instruction::AdcR8(R8::R8HLRef) => 0x8e,
            Instruction::AdcR8(R8::R8A) => 0x8f,
            Instruction::SubR8(R8::R8B) => 0x90,
            Instruction::SubR8(R8::R8C) => 0x91,
            Instruction::SubR8(R8::R8D) => 0x92,
            Instruction::SubR8(R8::R8E) => 0x93,
            Instruction::SubR8(R8::R8H) => 0x94,
            Instruction::SubR8(R8::R8L) => 0x95,
            Instruction::SubR8(R8::R8HLRef) => 0x96,
            Instruction::SubR8(R8::R8A) => 0x97,
            Instruction::SbcR8(R8::R8B) => 0x98,
            Instruction::SbcR8(R8::R8C) => 0x99,
            Instruction::SbcR8(R8::R8D) => 0x9a,
            Instruction::SbcR8(R8::R8E) => 0x9b,
            Instruction::SbcR8(R8::R8H) => 0x9c,
            Instruction::SbcR8(R8::R8L) => 0x9d,
            Instruction::SbcR8(R8::R8HLRef) => 0x9e,
            Instruction::SbcR8(R8::R8A) => 0x9f,
            Instruction::AndR8(R8::R8B) => 0xa0,
            Instruction::AndR8(R8::R8C) => 0xa1,
            Instruction::AndR8(R8::R8D) => 0xa2,
            Instruction::AndR8(R8::R8E) => 0xa3,
            Instruction::AndR8(R8::R8H) => 0xa4,
            Instruction::AndR8(R8::R8L) => 0xa5,
            Instruction::AndR8(R8::R8HLRef) => 0xa6,
            Instruction::AndR8(R8::R8A) => 0xa7,
            Instruction::XorR8(R8::R8B) => 0xa8,
            Instruction::XorR8(R8::R8C) => 0xa9,
            Instruction::XorR8(R8::R8D) => 0xaa,
            Instruction::XorR8(R8::R8E) => 0xab,
            Instruction::XorR8(R8::R8H) => 0xac,
            Instruction::XorR8(R8::R8L) => 0xad,
            Instruction::XorR8(R8::R8HLRef) => 0xae,
            Instruction::XorR8(R8::R8A) => 0xaf,
            Instruction::OrR8(R8::R8B) => 0xb0,
            Instruction::OrR8(R8::R8C) => 0xb1,
            Instruction::OrR8(R8::R8D) => 0xb2,
            Instruction::OrR8(R8::R8E) => 0xb3,
            Instruction::OrR8(R8::R8H) => 0xb4,
            Instruction::OrR8(R8::R8L) => 0xb5,
            Instruction::OrR8(R8::R8HLRef) => 0xb6,
            Instruction::OrR8(R8::R8A) => 0xb7,
            Instruction::CpR8(R8::R8B) => 0xb8,
            Instruction::CpR8(R8::R8C) => 0xb9,
            Instruction::CpR8(R8::R8D) => 0xba,
            Instruction::CpR8(R8::R8E) => 0xbb,
            Instruction::CpR8(R8::R8H) => 0xbc,
            Instruction::CpR8(R8::R8L) => 0xbd,
            Instruction::CpR8(R8::R8HLRef) => 0xbe,
            Instruction::CpR8(R8::R8A) => 0xbf,
            Instruction::RetCond(Cond::CondNZ) => 0xc0,
            Instruction::PopR16Stack(R16Stack::R16StackBC) => 0xc1,
            Instruction::JumpFarCond(Cond::CondNZ, _) => 0xc2,
            Instruction::JumpFar(_) => 0xc3,
            Instruction::CallCondA16(Cond::CondNZ, _) => 0xc4,
            Instruction::PushR16Stack(R16Stack::R16StackBC) => 0xc5,
            Instruction::AddAN8(_) => 0xc6,
            Instruction::Rst(Tgt3::T0) => 0xc7,
            Instruction::RetCond(Cond::CondZ) => 0xc8,
            Instruction::Ret => 0xc9,
            Instruction::JumpFarCond(Cond::CondZ, _) => 0xca,
            Instruction::CallCondA16(Cond::CondZ, _) => 0xcc,
            Instruction::CallA16(_) => 0xcd,
            Instruction::AdcAN8(_) => 0xce,
            Instruction::Rst(Tgt3::T1) => 0xcf,
            Instruction::RetCond(Cond::CondNC) => 0xd0,
            Instruction::PopR16Stack(R16Stack::R16StackDE) => 0xd1,
            Instruction::JumpFarCond(Cond::CondNC, _) => 0xd2,
            Instruction::CallCondA16(Cond::CondNC, _) => 0xd4,
            Instruction::PushR16Stack(R16Stack::R16StackDE) => 0xd5,
            Instruction::SubAN8(_) => 0xd6,
            Instruction::Rst(Tgt3::T2) => 0xd7,
            Instruction::RetCond(Cond::CondC) => 0xd8,
            Instruction::Reti => 0xd9,
            Instruction::JumpFarCond(Cond::CondC, _) => 0xda,
            Instruction::CallCondA16(Cond::CondC, _) => 0xdc,
            Instruction::SbcAN8(_) => 0xde,
            Instruction::Rst(Tgt3::T3) => 0xdf,
            Instruction::StoreAA8H(_) => 0xe0,
            Instruction::PopR16Stack(R16Stack::R16StackHL) => 0xe1,
            Instruction::StoreACH => 0xe2,
            Instruction::PushR16Stack(R16Stack::R16StackHL) => 0xe5,
            Instruction::AndAN8(_) => 0xe6,
            Instruction::Rst(Tgt3::T4) => 0xe7,
            Instruction::AddSPN8(_) => 0xe8,
            Instruction::JumpFarHL => 0xe9,
            Instruction::StoreAA16(_) => 0xea,
            Instruction::XorAN8(_) => 0xee,
            Instruction::Rst(Tgt3::T5) => 0xef,
            Instruction::LoadAA8H(_) => 0xf0,
            Instruction::PopR16Stack(R16Stack::R16StackAF) => 0xf1,
            Instruction::LoadACH => 0xf2,
            Instruction::Di => 0xf3,
            Instruction::PushR16Stack(R16Stack::R16StackAF) => 0xf5,
            Instruction::OrAN8(_) => 0xf6,
            Instruction::Rst(Tgt3::T6) => 0xf7,
            Instruction::LoadHLSPN8(_) => 0xf8,
            Instruction::LoadHLSP => 0xf9,
            Instruction::LoadAA16(_) => 0xfa,
            Instruction::Ei => 0xfb,
            Instruction::CpAN8(_) => 0xfe,
            Instruction::Rst(Tgt3::T7) => 0xff,

            Instruction::Rlc(R8::R8B) => 0x0,
            Instruction::Rlc(R8::R8C) => 0x1,
            Instruction::Rlc(R8::R8D) => 0x2,
            Instruction::Rlc(R8::R8E) => 0x3,
            Instruction::Rlc(R8::R8H) => 0x4,
            Instruction::Rlc(R8::R8L) => 0x5,
            Instruction::Rlc(R8::R8HLRef) => 0x6,
            Instruction::Rlc(R8::R8A) => 0x7,
            Instruction::Rrc(R8::R8B) => 0x8,
            Instruction::Rrc(R8::R8C) => 0x9,
            Instruction::Rrc(R8::R8D) => 0xa,
            Instruction::Rrc(R8::R8E) => 0xb,
            Instruction::Rrc(R8::R8H) => 0xc,
            Instruction::Rrc(R8::R8L) => 0xd,
            Instruction::Rrc(R8::R8HLRef) => 0xe,
            Instruction::Rrc(R8::R8A) => 0xf,
            Instruction::Rl(R8::R8B) => 0x10,
            Instruction::Rl(R8::R8C) => 0x11,
            Instruction::Rl(R8::R8D) => 0x12,
            Instruction::Rl(R8::R8E) => 0x13,
            Instruction::Rl(R8::R8H) => 0x14,
            Instruction::Rl(R8::R8L) => 0x15,
            Instruction::Rl(R8::R8HLRef) => 0x16,
            Instruction::Rl(R8::R8A) => 0x17,
            Instruction::Rr(R8::R8B) => 0x18,
            Instruction::Rr(R8::R8C) => 0x19,
            Instruction::Rr(R8::R8D) => 0x1a,
            Instruction::Rr(R8::R8E) => 0x1b,
            Instruction::Rr(R8::R8H) => 0x1c,
            Instruction::Rr(R8::R8L) => 0x1d,
            Instruction::Rr(R8::R8HLRef) => 0x1e,
            Instruction::Rr(R8::R8A) => 0x1f,
            Instruction::Sla(R8::R8B) => 0x20,
            Instruction::Sla(R8::R8C) => 0x21,
            Instruction::Sla(R8::R8D) => 0x22,
            Instruction::Sla(R8::R8E) => 0x23,
            Instruction::Sla(R8::R8H) => 0x24,
            Instruction::Sla(R8::R8L) => 0x25,
            Instruction::Sla(R8::R8HLRef) => 0x26,
            Instruction::Sla(R8::R8A) => 0x27,
            Instruction::Sra(R8::R8B) => 0x28,
            Instruction::Sra(R8::R8C) => 0x29,
            Instruction::Sra(R8::R8D) => 0x2a,
            Instruction::Sra(R8::R8E) => 0x2b,
            Instruction::Sra(R8::R8H) => 0x2c,
            Instruction::Sra(R8::R8L) => 0x2d,
            Instruction::Sra(R8::R8HLRef) => 0x2e,
            Instruction::Sra(R8::R8A) => 0x2f,
            Instruction::Swap(R8::R8B) => 0x30,
            Instruction::Swap(R8::R8C) => 0x31,
            Instruction::Swap(R8::R8D) => 0x32,
            Instruction::Swap(R8::R8E) => 0x33,
            Instruction::Swap(R8::R8H) => 0x34,
            Instruction::Swap(R8::R8L) => 0x35,
            Instruction::Swap(R8::R8HLRef) => 0x36,
            Instruction::Swap(R8::R8A) => 0x37,
            Instruction::Srl(R8::R8B) => 0x38,
            Instruction::Srl(R8::R8C) => 0x39,
            Instruction::Srl(R8::R8D) => 0x3a,
            Instruction::Srl(R8::R8E) => 0x3b,
            Instruction::Srl(R8::R8H) => 0x3c,
            Instruction::Srl(R8::R8L) => 0x3d,
            Instruction::Srl(R8::R8HLRef) => 0x3e,
            Instruction::Srl(R8::R8A) => 0x3f,
            Instruction::Bit(BitRef::B0, R8::R8B) => 0x40,
            Instruction::Bit(BitRef::B0, R8::R8C) => 0x41,
            Instruction::Bit(BitRef::B0, R8::R8D) => 0x42,
            Instruction::Bit(BitRef::B0, R8::R8E) => 0x43,
            Instruction::Bit(BitRef::B0, R8::R8H) => 0x44,
            Instruction::Bit(BitRef::B0, R8::R8L) => 0x45,
            Instruction::Bit(BitRef::B0, R8::R8HLRef) => 0x46,
            Instruction::Bit(BitRef::B0, R8::R8A) => 0x47,
            Instruction::Bit(BitRef::B1, R8::R8B) => 0x48,
            Instruction::Bit(BitRef::B1, R8::R8C) => 0x49,
            Instruction::Bit(BitRef::B1, R8::R8D) => 0x4a,
            Instruction::Bit(BitRef::B1, R8::R8E) => 0x4b,
            Instruction::Bit(BitRef::B1, R8::R8H) => 0x4c,
            Instruction::Bit(BitRef::B1, R8::R8L) => 0x4d,
            Instruction::Bit(BitRef::B1, R8::R8HLRef) => 0x4e,
            Instruction::Bit(BitRef::B1, R8::R8A) => 0x4f,
            Instruction::Bit(BitRef::B2, R8::R8B) => 0x50,
            Instruction::Bit(BitRef::B2, R8::R8C) => 0x51,
            Instruction::Bit(BitRef::B2, R8::R8D) => 0x52,
            Instruction::Bit(BitRef::B2, R8::R8E) => 0x53,
            Instruction::Bit(BitRef::B2, R8::R8H) => 0x54,
            Instruction::Bit(BitRef::B2, R8::R8L) => 0x55,
            Instruction::Bit(BitRef::B2, R8::R8HLRef) => 0x56,
            Instruction::Bit(BitRef::B2, R8::R8A) => 0x57,
            Instruction::Bit(BitRef::B3, R8::R8B) => 0x58,
            Instruction::Bit(BitRef::B3, R8::R8C) => 0x59,
            Instruction::Bit(BitRef::B3, R8::R8D) => 0x5a,
            Instruction::Bit(BitRef::B3, R8::R8E) => 0x5b,
            Instruction::Bit(BitRef::B3, R8::R8H) => 0x5c,
            Instruction::Bit(BitRef::B3, R8::R8L) => 0x5d,
            Instruction::Bit(BitRef::B3, R8::R8HLRef) => 0x5e,
            Instruction::Bit(BitRef::B3, R8::R8A) => 0x5f,
            Instruction::Bit(BitRef::B4, R8::R8B) => 0x60,
            Instruction::Bit(BitRef::B4, R8::R8C) => 0x61,
            Instruction::Bit(BitRef::B4, R8::R8D) => 0x62,
            Instruction::Bit(BitRef::B4, R8::R8E) => 0x63,
            Instruction::Bit(BitRef::B4, R8::R8H) => 0x64,
            Instruction::Bit(BitRef::B4, R8::R8L) => 0x65,
            Instruction::Bit(BitRef::B4, R8::R8HLRef) => 0x66,
            Instruction::Bit(BitRef::B4, R8::R8A) => 0x67,
            Instruction::Bit(BitRef::B5, R8::R8B) => 0x68,
            Instruction::Bit(BitRef::B5, R8::R8C) => 0x69,
            Instruction::Bit(BitRef::B5, R8::R8D) => 0x6a,
            Instruction::Bit(BitRef::B5, R8::R8E) => 0x6b,
            Instruction::Bit(BitRef::B5, R8::R8H) => 0x6c,
            Instruction::Bit(BitRef::B5, R8::R8L) => 0x6d,
            Instruction::Bit(BitRef::B5, R8::R8HLRef) => 0x6e,
            Instruction::Bit(BitRef::B5, R8::R8A) => 0x6f,
            Instruction::Bit(BitRef::B6, R8::R8B) => 0x70,
            Instruction::Bit(BitRef::B6, R8::R8C) => 0x71,
            Instruction::Bit(BitRef::B6, R8::R8D) => 0x72,
            Instruction::Bit(BitRef::B6, R8::R8E) => 0x73,
            Instruction::Bit(BitRef::B6, R8::R8H) => 0x74,
            Instruction::Bit(BitRef::B6, R8::R8L) => 0x75,
            Instruction::Bit(BitRef::B6, R8::R8HLRef) => 0x76,
            Instruction::Bit(BitRef::B6, R8::R8A) => 0x77,
            Instruction::Bit(BitRef::B7, R8::R8B) => 0x78,
            Instruction::Bit(BitRef::B7, R8::R8C) => 0x79,
            Instruction::Bit(BitRef::B7, R8::R8D) => 0x7a,
            Instruction::Bit(BitRef::B7, R8::R8E) => 0x7b,
            Instruction::Bit(BitRef::B7, R8::R8H) => 0x7c,
            Instruction::Bit(BitRef::B7, R8::R8L) => 0x7d,
            Instruction::Bit(BitRef::B7, R8::R8HLRef) => 0x7e,
            Instruction::Bit(BitRef::B7, R8::R8A) => 0x7f,
            Instruction::Res(BitRef::B0, R8::R8B) => 0x80,
            Instruction::Res(BitRef::B0, R8::R8C) => 0x81,
            Instruction::Res(BitRef::B0, R8::R8D) => 0x82,
            Instruction::Res(BitRef::B0, R8::R8E) => 0x83,
            Instruction::Res(BitRef::B0, R8::R8H) => 0x84,
            Instruction::Res(BitRef::B0, R8::R8L) => 0x85,
            Instruction::Res(BitRef::B0, R8::R8HLRef) => 0x86,
            Instruction::Res(BitRef::B0, R8::R8A) => 0x87,
            Instruction::Res(BitRef::B1, R8::R8B) => 0x88,
            Instruction::Res(BitRef::B1, R8::R8C) => 0x89,
            Instruction::Res(BitRef::B1, R8::R8D) => 0x8a,
            Instruction::Res(BitRef::B1, R8::R8E) => 0x8b,
            Instruction::Res(BitRef::B1, R8::R8H) => 0x8c,
            Instruction::Res(BitRef::B1, R8::R8L) => 0x8d,
            Instruction::Res(BitRef::B1, R8::R8HLRef) => 0x8e,
            Instruction::Res(BitRef::B1, R8::R8A) => 0x8f,
            Instruction::Res(BitRef::B2, R8::R8B) => 0x90,
            Instruction::Res(BitRef::B2, R8::R8C) => 0x91,
            Instruction::Res(BitRef::B2, R8::R8D) => 0x92,
            Instruction::Res(BitRef::B2, R8::R8E) => 0x93,
            Instruction::Res(BitRef::B2, R8::R8H) => 0x94,
            Instruction::Res(BitRef::B2, R8::R8L) => 0x95,
            Instruction::Res(BitRef::B2, R8::R8HLRef) => 0x96,
            Instruction::Res(BitRef::B2, R8::R8A) => 0x97,
            Instruction::Res(BitRef::B3, R8::R8B) => 0x98,
            Instruction::Res(BitRef::B3, R8::R8C) => 0x99,
            Instruction::Res(BitRef::B3, R8::R8D) => 0x9a,
            Instruction::Res(BitRef::B3, R8::R8E) => 0x9b,
            Instruction::Res(BitRef::B3, R8::R8H) => 0x9c,
            Instruction::Res(BitRef::B3, R8::R8L) => 0x9d,
            Instruction::Res(BitRef::B3, R8::R8HLRef) => 0x9e,
            Instruction::Res(BitRef::B3, R8::R8A) => 0x9f,
            Instruction::Res(BitRef::B4, R8::R8B) => 0xa0,
            Instruction::Res(BitRef::B4, R8::R8C) => 0xa1,
            Instruction::Res(BitRef::B4, R8::R8D) => 0xa2,
            Instruction::Res(BitRef::B4, R8::R8E) => 0xa3,
            Instruction::Res(BitRef::B4, R8::R8H) => 0xa4,
            Instruction::Res(BitRef::B4, R8::R8L) => 0xa5,
            Instruction::Res(BitRef::B4, R8::R8HLRef) => 0xa6,
            Instruction::Res(BitRef::B4, R8::R8A) => 0xa7,
            Instruction::Res(BitRef::B5, R8::R8B) => 0xa8,
            Instruction::Res(BitRef::B5, R8::R8C) => 0xa9,
            Instruction::Res(BitRef::B5, R8::R8D) => 0xaa,
            Instruction::Res(BitRef::B5, R8::R8E) => 0xab,
            Instruction::Res(BitRef::B5, R8::R8H) => 0xac,
            Instruction::Res(BitRef::B5, R8::R8L) => 0xad,
            Instruction::Res(BitRef::B5, R8::R8HLRef) => 0xae,
            Instruction::Res(BitRef::B5, R8::R8A) => 0xaf,
            Instruction::Res(BitRef::B6, R8::R8B) => 0xb0,
            Instruction::Res(BitRef::B6, R8::R8C) => 0xb1,
            Instruction::Res(BitRef::B6, R8::R8D) => 0xb2,
            Instruction::Res(BitRef::B6, R8::R8E) => 0xb3,
            Instruction::Res(BitRef::B6, R8::R8H) => 0xb4,
            Instruction::Res(BitRef::B6, R8::R8L) => 0xb5,
            Instruction::Res(BitRef::B6, R8::R8HLRef) => 0xb6,
            Instruction::Res(BitRef::B6, R8::R8A) => 0xb7,
            Instruction::Res(BitRef::B7, R8::R8B) => 0xb8,
            Instruction::Res(BitRef::B7, R8::R8C) => 0xb9,
            Instruction::Res(BitRef::B7, R8::R8D) => 0xba,
            Instruction::Res(BitRef::B7, R8::R8E) => 0xbb,
            Instruction::Res(BitRef::B7, R8::R8H) => 0xbc,
            Instruction::Res(BitRef::B7, R8::R8L) => 0xbd,
            Instruction::Res(BitRef::B7, R8::R8HLRef) => 0xbe,
            Instruction::Res(BitRef::B7, R8::R8A) => 0xbf,
            Instruction::Set(BitRef::B0, R8::R8B) => 0xc0,
            Instruction::Set(BitRef::B0, R8::R8C) => 0xc1,
            Instruction::Set(BitRef::B0, R8::R8D) => 0xc2,
            Instruction::Set(BitRef::B0, R8::R8E) => 0xc3,
            Instruction::Set(BitRef::B0, R8::R8H) => 0xc4,
            Instruction::Set(BitRef::B0, R8::R8L) => 0xc5,
            Instruction::Set(BitRef::B0, R8::R8HLRef) => 0xc6,
            Instruction::Set(BitRef::B0, R8::R8A) => 0xc7,
            Instruction::Set(BitRef::B1, R8::R8B) => 0xc8,
            Instruction::Set(BitRef::B1, R8::R8C) => 0xc9,
            Instruction::Set(BitRef::B1, R8::R8D) => 0xca,
            Instruction::Set(BitRef::B1, R8::R8E) => 0xcb,
            Instruction::Set(BitRef::B1, R8::R8H) => 0xcc,
            Instruction::Set(BitRef::B1, R8::R8L) => 0xcd,
            Instruction::Set(BitRef::B1, R8::R8HLRef) => 0xce,
            Instruction::Set(BitRef::B1, R8::R8A) => 0xcf,
            Instruction::Set(BitRef::B2, R8::R8B) => 0xd0,
            Instruction::Set(BitRef::B2, R8::R8C) => 0xd1,
            Instruction::Set(BitRef::B2, R8::R8D) => 0xd2,
            Instruction::Set(BitRef::B2, R8::R8E) => 0xd3,
            Instruction::Set(BitRef::B2, R8::R8H) => 0xd4,
            Instruction::Set(BitRef::B2, R8::R8L) => 0xd5,
            Instruction::Set(BitRef::B2, R8::R8HLRef) => 0xd6,
            Instruction::Set(BitRef::B2, R8::R8A) => 0xd7,
            Instruction::Set(BitRef::B3, R8::R8B) => 0xd8,
            Instruction::Set(BitRef::B3, R8::R8C) => 0xd9,
            Instruction::Set(BitRef::B3, R8::R8D) => 0xda,
            Instruction::Set(BitRef::B3, R8::R8E) => 0xdb,
            Instruction::Set(BitRef::B3, R8::R8H) => 0xdc,
            Instruction::Set(BitRef::B3, R8::R8L) => 0xdd,
            Instruction::Set(BitRef::B3, R8::R8HLRef) => 0xde,
            Instruction::Set(BitRef::B3, R8::R8A) => 0xdf,
            Instruction::Set(BitRef::B4, R8::R8B) => 0xe0,
            Instruction::Set(BitRef::B4, R8::R8C) => 0xe1,
            Instruction::Set(BitRef::B4, R8::R8D) => 0xe2,
            Instruction::Set(BitRef::B4, R8::R8E) => 0xe3,
            Instruction::Set(BitRef::B4, R8::R8H) => 0xe4,
            Instruction::Set(BitRef::B4, R8::R8L) => 0xe5,
            Instruction::Set(BitRef::B4, R8::R8HLRef) => 0xe6,
            Instruction::Set(BitRef::B4, R8::R8A) => 0xe7,
            Instruction::Set(BitRef::B5, R8::R8B) => 0xe8,
            Instruction::Set(BitRef::B5, R8::R8C) => 0xe9,
            Instruction::Set(BitRef::B5, R8::R8D) => 0xea,
            Instruction::Set(BitRef::B5, R8::R8E) => 0xeb,
            Instruction::Set(BitRef::B5, R8::R8H) => 0xec,
            Instruction::Set(BitRef::B5, R8::R8L) => 0xed,
            Instruction::Set(BitRef::B5, R8::R8HLRef) => 0xee,
            Instruction::Set(BitRef::B5, R8::R8A) => 0xef,
            Instruction::Set(BitRef::B6, R8::R8B) => 0xf0,
            Instruction::Set(BitRef::B6, R8::R8C) => 0xf1,
            Instruction::Set(BitRef::B6, R8::R8D) => 0xf2,
            Instruction::Set(BitRef::B6, R8::R8E) => 0xf3,
            Instruction::Set(BitRef::B6, R8::R8H) => 0xf4,
            Instruction::Set(BitRef::B6, R8::R8L) => 0xf5,
            Instruction::Set(BitRef::B6, R8::R8HLRef) => 0xf6,
            Instruction::Set(BitRef::B6, R8::R8A) => 0xf7,
            Instruction::Set(BitRef::B7, R8::R8B) => 0xf8,
            Instruction::Set(BitRef::B7, R8::R8C) => 0xf9,
            Instruction::Set(BitRef::B7, R8::R8D) => 0xfa,
            Instruction::Set(BitRef::B7, R8::R8E) => 0xfb,
            Instruction::Set(BitRef::B7, R8::R8H) => 0xfc,
            Instruction::Set(BitRef::B7, R8::R8L) => 0xfd,
            Instruction::Set(BitRef::B7, R8::R8HLRef) => 0xfe,
            Instruction::Set(BitRef::B7, R8::R8A) => 0xff,
            Instruction::LoadR8R8(R8::R8HLRef, R8::R8HLRef) => unreachable!(),
        }
    }

    pub fn from_u8_slice(
        buf: &[u8],
        addr: u16,
        _unused: usize,
    ) -> Result<(Instruction, u8), InstructionError> {
        let size = OPCODE_SIZE_LOOKUP[buf[0] as usize];
        let insn = buf[0];
        if size == 0 {
            return Err(InstructionError::Illegal(buf[0]).into());
        }
        let insn = match insn {
            0x0 => Instruction::Nop,
            0x1 => Instruction::LoadR16N16(R16::R16BC, Self::read_u16_helper(buf, addr + 1)),
            0x2 => Instruction::StoreAR16Mem(R16Mem::R16MemBC),
            0x3 => Instruction::IncR16(R16::R16BC),
            0x4 => Instruction::IncR8(R8::R8B),
            0x5 => Instruction::DecR8(R8::R8B),
            0x6 => Instruction::LoadR8N8(R8::R8B, Self::read_u8_helper(buf, addr + 1)),
            0x7 => Instruction::RlcA,
            0x8 => Instruction::StoreSPA16(Self::read_u16_helper(buf, addr + 1)),
            0x9 => Instruction::AddHLR16(R16::R16BC),
            0xa => Instruction::LoadAR16Mem(R16Mem::R16MemBC),
            0xb => Instruction::DecR16(R16::R16BC),
            0xc => Instruction::IncR8(R8::R8C),
            0xd => Instruction::DecR8(R8::R8C),
            0xe => Instruction::LoadR8N8(R8::R8C, Self::read_u8_helper(buf, addr + 1)),
            0xf => Instruction::RrcA,
            0x10 => Instruction::StopN8(Self::read_u8_helper(buf, addr + 1)),
            0x11 => Instruction::LoadR16N16(R16::R16DE, Self::read_u16_helper(buf, addr + 1)),
            0x12 => Instruction::StoreAR16Mem(R16Mem::R16MemDE),
            0x13 => Instruction::IncR16(R16::R16DE),
            0x14 => Instruction::IncR8(R8::R8D),
            0x15 => Instruction::DecR8(R8::R8D),
            0x16 => Instruction::LoadR8N8(R8::R8D, Self::read_u8_helper(buf, addr + 1)),
            0x17 => Instruction::RlA,
            0x18 => Instruction::JumpNear(Self::read_u8_helper(buf, addr + 1) as i8),
            0x19 => Instruction::AddHLR16(R16::R16DE),
            0x1a => Instruction::LoadAR16Mem(R16Mem::R16MemDE),
            0x1b => Instruction::DecR16(R16::R16DE),
            0x1c => Instruction::IncR8(R8::R8E),
            0x1d => Instruction::DecR8(R8::R8E),
            0x1e => Instruction::LoadR8N8(R8::R8E, Self::read_u8_helper(buf, addr + 1)),
            0x1f => Instruction::RrA,
            0x20 => {
                Instruction::JumpNearCond(Cond::CondNZ, Self::read_u8_helper(buf, addr + 1) as i8)
            }
            0x21 => Instruction::LoadR16N16(R16::R16HL, Self::read_u16_helper(buf, addr + 1)),
            0x22 => Instruction::StoreAR16Mem(R16Mem::R16MemHLInc),
            0x23 => Instruction::IncR16(R16::R16HL),
            0x24 => Instruction::IncR8(R8::R8H),
            0x25 => Instruction::DecR8(R8::R8H),
            0x26 => Instruction::LoadR8N8(R8::R8H, Self::read_u8_helper(buf, addr + 1)),
            0x27 => Instruction::Daa,
            0x28 => {
                Instruction::JumpNearCond(Cond::CondZ, Self::read_u8_helper(buf, addr + 1) as i8)
            }
            0x29 => Instruction::AddHLR16(R16::R16HL),
            0x2a => Instruction::LoadAR16Mem(R16Mem::R16MemHLInc),
            0x2b => Instruction::DecR16(R16::R16HL),
            0x2c => Instruction::IncR8(R8::R8L),
            0x2d => Instruction::DecR8(R8::R8L),
            0x2e => Instruction::LoadR8N8(R8::R8L, Self::read_u8_helper(buf, addr + 1)),
            0x2f => Instruction::Cpl,
            0x30 => {
                Instruction::JumpNearCond(Cond::CondNC, Self::read_u8_helper(buf, addr + 1) as i8)
            }
            0x31 => Instruction::LoadR16N16(R16::R16Sp, Self::read_u16_helper(buf, addr + 1)),
            0x32 => Instruction::StoreAR16Mem(R16Mem::R16MemHLDec),
            0x33 => Instruction::IncR16(R16::R16Sp),
            0x34 => Instruction::IncR8(R8::R8HLRef),
            0x35 => Instruction::DecR8(R8::R8HLRef),
            0x36 => Instruction::LoadR8N8(R8::R8HLRef, Self::read_u8_helper(buf, addr + 1)),
            0x37 => Instruction::Scf,
            0x38 => {
                Instruction::JumpNearCond(Cond::CondC, Self::read_u8_helper(buf, addr + 1) as i8)
            }
            0x39 => Instruction::AddHLR16(R16::R16Sp),
            0x3a => Instruction::LoadAR16Mem(R16Mem::R16MemHLDec),
            0x3b => Instruction::DecR16(R16::R16Sp),
            0x3c => Instruction::IncR8(R8::R8A),
            0x3d => Instruction::DecR8(R8::R8A),
            0x3e => Instruction::LoadR8N8(R8::R8A, Self::read_u8_helper(buf, addr + 1)),
            0x3f => Instruction::Ccf,
            0x40 => Instruction::LoadR8R8(R8::R8B, R8::R8B),
            0x41 => Instruction::LoadR8R8(R8::R8B, R8::R8C),
            0x42 => Instruction::LoadR8R8(R8::R8B, R8::R8D),
            0x43 => Instruction::LoadR8R8(R8::R8B, R8::R8E),
            0x44 => Instruction::LoadR8R8(R8::R8B, R8::R8H),
            0x45 => Instruction::LoadR8R8(R8::R8B, R8::R8L),
            0x46 => Instruction::LoadR8R8(R8::R8B, R8::R8HLRef),
            0x47 => Instruction::LoadR8R8(R8::R8B, R8::R8A),
            0x48 => Instruction::LoadR8R8(R8::R8C, R8::R8B),
            0x49 => Instruction::LoadR8R8(R8::R8C, R8::R8C),
            0x4a => Instruction::LoadR8R8(R8::R8C, R8::R8D),
            0x4b => Instruction::LoadR8R8(R8::R8C, R8::R8E),
            0x4c => Instruction::LoadR8R8(R8::R8C, R8::R8H),
            0x4d => Instruction::LoadR8R8(R8::R8C, R8::R8L),
            0x4e => Instruction::LoadR8R8(R8::R8C, R8::R8HLRef),
            0x4f => Instruction::LoadR8R8(R8::R8C, R8::R8A),
            0x50 => Instruction::LoadR8R8(R8::R8D, R8::R8B),
            0x51 => Instruction::LoadR8R8(R8::R8D, R8::R8C),
            0x52 => Instruction::LoadR8R8(R8::R8D, R8::R8D),
            0x53 => Instruction::LoadR8R8(R8::R8D, R8::R8E),
            0x54 => Instruction::LoadR8R8(R8::R8D, R8::R8H),
            0x55 => Instruction::LoadR8R8(R8::R8D, R8::R8L),
            0x56 => Instruction::LoadR8R8(R8::R8D, R8::R8HLRef),
            0x57 => Instruction::LoadR8R8(R8::R8D, R8::R8A),
            0x58 => Instruction::LoadR8R8(R8::R8E, R8::R8B),
            0x59 => Instruction::LoadR8R8(R8::R8E, R8::R8C),
            0x5a => Instruction::LoadR8R8(R8::R8E, R8::R8D),
            0x5b => Instruction::LoadR8R8(R8::R8E, R8::R8E),
            0x5c => Instruction::LoadR8R8(R8::R8E, R8::R8H),
            0x5d => Instruction::LoadR8R8(R8::R8E, R8::R8L),
            0x5e => Instruction::LoadR8R8(R8::R8E, R8::R8HLRef),
            0x5f => Instruction::LoadR8R8(R8::R8E, R8::R8A),
            0x60 => Instruction::LoadR8R8(R8::R8H, R8::R8B),
            0x61 => Instruction::LoadR8R8(R8::R8H, R8::R8C),
            0x62 => Instruction::LoadR8R8(R8::R8H, R8::R8D),
            0x63 => Instruction::LoadR8R8(R8::R8H, R8::R8E),
            0x64 => Instruction::LoadR8R8(R8::R8H, R8::R8H),
            0x65 => Instruction::LoadR8R8(R8::R8H, R8::R8L),
            0x66 => Instruction::LoadR8R8(R8::R8H, R8::R8HLRef),
            0x67 => Instruction::LoadR8R8(R8::R8H, R8::R8A),
            0x68 => Instruction::LoadR8R8(R8::R8L, R8::R8B),
            0x69 => Instruction::LoadR8R8(R8::R8L, R8::R8C),
            0x6a => Instruction::LoadR8R8(R8::R8L, R8::R8D),
            0x6b => Instruction::LoadR8R8(R8::R8L, R8::R8E),
            0x6c => Instruction::LoadR8R8(R8::R8L, R8::R8H),
            0x6d => Instruction::LoadR8R8(R8::R8L, R8::R8L),
            0x6e => Instruction::LoadR8R8(R8::R8L, R8::R8HLRef),
            0x6f => Instruction::LoadR8R8(R8::R8L, R8::R8A),
            0x70 => Instruction::LoadR8R8(R8::R8HLRef, R8::R8B),
            0x71 => Instruction::LoadR8R8(R8::R8HLRef, R8::R8C),
            0x72 => Instruction::LoadR8R8(R8::R8HLRef, R8::R8D),
            0x73 => Instruction::LoadR8R8(R8::R8HLRef, R8::R8E),
            0x74 => Instruction::LoadR8R8(R8::R8HLRef, R8::R8H),
            0x75 => Instruction::LoadR8R8(R8::R8HLRef, R8::R8L),
            0x76 => Instruction::Halt,
            0x77 => Instruction::LoadR8R8(R8::R8HLRef, R8::R8A),
            0x78 => Instruction::LoadR8R8(R8::R8A, R8::R8B),
            0x79 => Instruction::LoadR8R8(R8::R8A, R8::R8C),
            0x7a => Instruction::LoadR8R8(R8::R8A, R8::R8D),
            0x7b => Instruction::LoadR8R8(R8::R8A, R8::R8E),
            0x7c => Instruction::LoadR8R8(R8::R8A, R8::R8H),
            0x7d => Instruction::LoadR8R8(R8::R8A, R8::R8L),
            0x7e => Instruction::LoadR8R8(R8::R8A, R8::R8HLRef),
            0x7f => Instruction::LoadR8R8(R8::R8A, R8::R8A),
            0x80 => Instruction::AddR8(R8::R8B),
            0x81 => Instruction::AddR8(R8::R8C),
            0x82 => Instruction::AddR8(R8::R8D),
            0x83 => Instruction::AddR8(R8::R8E),
            0x84 => Instruction::AddR8(R8::R8H),
            0x85 => Instruction::AddR8(R8::R8L),
            0x86 => Instruction::AddR8(R8::R8HLRef),
            0x87 => Instruction::AddR8(R8::R8A),
            0x88 => Instruction::AdcR8(R8::R8B),
            0x89 => Instruction::AdcR8(R8::R8C),
            0x8a => Instruction::AdcR8(R8::R8D),
            0x8b => Instruction::AdcR8(R8::R8E),
            0x8c => Instruction::AdcR8(R8::R8H),
            0x8d => Instruction::AdcR8(R8::R8L),
            0x8e => Instruction::AdcR8(R8::R8HLRef),
            0x8f => Instruction::AdcR8(R8::R8A),
            0x90 => Instruction::SubR8(R8::R8B),
            0x91 => Instruction::SubR8(R8::R8C),
            0x92 => Instruction::SubR8(R8::R8D),
            0x93 => Instruction::SubR8(R8::R8E),
            0x94 => Instruction::SubR8(R8::R8H),
            0x95 => Instruction::SubR8(R8::R8L),
            0x96 => Instruction::SubR8(R8::R8HLRef),
            0x97 => Instruction::SubR8(R8::R8A),
            0x98 => Instruction::SbcR8(R8::R8B),
            0x99 => Instruction::SbcR8(R8::R8C),
            0x9a => Instruction::SbcR8(R8::R8D),
            0x9b => Instruction::SbcR8(R8::R8E),
            0x9c => Instruction::SbcR8(R8::R8H),
            0x9d => Instruction::SbcR8(R8::R8L),
            0x9e => Instruction::SbcR8(R8::R8HLRef),
            0x9f => Instruction::SbcR8(R8::R8A),
            0xa0 => Instruction::AndR8(R8::R8B),
            0xa1 => Instruction::AndR8(R8::R8C),
            0xa2 => Instruction::AndR8(R8::R8D),
            0xa3 => Instruction::AndR8(R8::R8E),
            0xa4 => Instruction::AndR8(R8::R8H),
            0xa5 => Instruction::AndR8(R8::R8L),
            0xa6 => Instruction::AndR8(R8::R8HLRef),
            0xa7 => Instruction::AndR8(R8::R8A),
            0xa8 => Instruction::XorR8(R8::R8B),
            0xa9 => Instruction::XorR8(R8::R8C),
            0xaa => Instruction::XorR8(R8::R8D),
            0xab => Instruction::XorR8(R8::R8E),
            0xac => Instruction::XorR8(R8::R8H),
            0xad => Instruction::XorR8(R8::R8L),
            0xae => Instruction::XorR8(R8::R8HLRef),
            0xaf => Instruction::XorR8(R8::R8A),
            0xb0 => Instruction::OrR8(R8::R8B),
            0xb1 => Instruction::OrR8(R8::R8C),
            0xb2 => Instruction::OrR8(R8::R8D),
            0xb3 => Instruction::OrR8(R8::R8E),
            0xb4 => Instruction::OrR8(R8::R8H),
            0xb5 => Instruction::OrR8(R8::R8L),
            0xb6 => Instruction::OrR8(R8::R8HLRef),
            0xb7 => Instruction::OrR8(R8::R8A),
            0xb8 => Instruction::CpR8(R8::R8B),
            0xb9 => Instruction::CpR8(R8::R8C),
            0xba => Instruction::CpR8(R8::R8D),
            0xbb => Instruction::CpR8(R8::R8E),
            0xbc => Instruction::CpR8(R8::R8H),
            0xbd => Instruction::CpR8(R8::R8L),
            0xbe => Instruction::CpR8(R8::R8HLRef),
            0xbf => Instruction::CpR8(R8::R8A),
            0xc0 => Instruction::RetCond(Cond::CondNZ),
            0xc1 => Instruction::PopR16Stack(R16Stack::R16StackBC),
            0xc2 => Instruction::JumpFarCond(Cond::CondNZ, Self::read_u16_helper(buf, addr + 1)),
            0xc3 => Instruction::JumpFar(Self::read_u16_helper(buf, addr + 1)),
            0xc4 => Instruction::CallCondA16(Cond::CondNZ, Self::read_u16_helper(buf, addr + 1)),
            0xc5 => Instruction::PushR16Stack(R16Stack::R16StackBC),
            0xc6 => Instruction::AddAN8(Self::read_u8_helper(buf, addr + 1)),
            0xc7 => Instruction::Rst(Tgt3::T0),
            0xc8 => Instruction::RetCond(Cond::CondZ),
            0xc9 => Instruction::Ret,
            0xca => Instruction::JumpFarCond(Cond::CondZ, Self::read_u16_helper(buf, addr + 1)),
            0xcc => Instruction::CallCondA16(Cond::CondZ, Self::read_u16_helper(buf, addr + 1)),
            0xcd => Instruction::CallA16(Self::read_u16_helper(buf, addr + 1)),
            0xce => Instruction::AdcAN8(Self::read_u8_helper(buf, addr + 1)),
            0xcf => Instruction::Rst(Tgt3::T1),
            0xd0 => Instruction::RetCond(Cond::CondNC),
            0xd1 => Instruction::PopR16Stack(R16Stack::R16StackDE),
            0xd2 => Instruction::JumpFarCond(Cond::CondNC, Self::read_u16_helper(buf, addr + 1)),
            0xd4 => Instruction::CallCondA16(Cond::CondNC, Self::read_u16_helper(buf, addr + 1)),
            0xd5 => Instruction::PushR16Stack(R16Stack::R16StackDE),
            0xd6 => Instruction::SubAN8(Self::read_u8_helper(buf, addr + 1)),
            0xd7 => Instruction::Rst(Tgt3::T2),
            0xd8 => Instruction::RetCond(Cond::CondC),
            0xd9 => Instruction::Reti,
            0xda => Instruction::JumpFarCond(Cond::CondC, Self::read_u16_helper(buf, addr + 1)),
            0xdc => Instruction::CallCondA16(Cond::CondC, Self::read_u16_helper(buf, addr + 1)),
            0xde => Instruction::SbcAN8(Self::read_u8_helper(buf, addr + 1)),
            0xdf => Instruction::Rst(Tgt3::T3),
            0xe0 => Instruction::StoreAA8H(Self::read_u8_helper(buf, addr + 1)),
            0xe1 => Instruction::PopR16Stack(R16Stack::R16StackHL),
            0xe2 => Instruction::StoreACH,
            0xe5 => Instruction::PushR16Stack(R16Stack::R16StackHL),
            0xe6 => Instruction::AndAN8(Self::read_u8_helper(buf, addr + 1)),
            0xe7 => Instruction::Rst(Tgt3::T4),
            0xe8 => Instruction::AddSPN8(Self::read_u8_helper(buf, addr + 1)),
            0xe9 => Instruction::JumpFarHL,
            0xea => Instruction::StoreAA16(Self::read_u16_helper(buf, addr + 1)),
            0xee => Instruction::XorAN8(Self::read_u8_helper(buf, addr + 1)),
            0xef => Instruction::Rst(Tgt3::T5),
            0xf0 => Instruction::LoadAA8H(Self::read_u8_helper(buf, addr + 1)),
            0xf1 => Instruction::PopR16Stack(R16Stack::R16StackAF),
            0xf2 => Instruction::LoadACH,
            0xf3 => Instruction::Di,
            0xf5 => Instruction::PushR16Stack(R16Stack::R16StackAF),
            0xf6 => Instruction::OrAN8(Self::read_u8_helper(buf, addr + 1)),
            0xf7 => Instruction::Rst(Tgt3::T6),
            0xf8 => Instruction::LoadHLSPN8(Self::read_u8_helper(buf, addr + 1)),
            0xf9 => Instruction::LoadHLSP,
            0xfa => Instruction::LoadAA16(Self::read_u16_helper(buf, addr + 1)),
            0xfb => Instruction::Ei,
            0xfe => Instruction::CpAN8(Self::read_u8_helper(buf, addr + 1)),
            0xff => Instruction::Rst(Tgt3::T7),
            0xcb => match buf[1] {
                0x00 => Instruction::Rlc(R8::R8B),
                0x01 => Instruction::Rlc(R8::R8C),
                0x02 => Instruction::Rlc(R8::R8D),
                0x03 => Instruction::Rlc(R8::R8E),
                0x04 => Instruction::Rlc(R8::R8H),
                0x05 => Instruction::Rlc(R8::R8L),
                0x06 => Instruction::Rlc(R8::R8HLRef),
                0x07 => Instruction::Rlc(R8::R8A),
                0x08 => Instruction::Rrc(R8::R8B),
                0x09 => Instruction::Rrc(R8::R8C),
                0x0a => Instruction::Rrc(R8::R8D),
                0x0b => Instruction::Rrc(R8::R8E),
                0x0c => Instruction::Rrc(R8::R8H),
                0x0d => Instruction::Rrc(R8::R8L),
                0x0e => Instruction::Rrc(R8::R8HLRef),
                0x0f => Instruction::Rrc(R8::R8A),
                0x10 => Instruction::Rl(R8::R8B),
                0x11 => Instruction::Rl(R8::R8C),
                0x12 => Instruction::Rl(R8::R8D),
                0x13 => Instruction::Rl(R8::R8E),
                0x14 => Instruction::Rl(R8::R8H),
                0x15 => Instruction::Rl(R8::R8L),
                0x16 => Instruction::Rl(R8::R8HLRef),
                0x17 => Instruction::Rl(R8::R8A),
                0x18 => Instruction::Rr(R8::R8B),
                0x19 => Instruction::Rr(R8::R8C),
                0x1a => Instruction::Rr(R8::R8D),
                0x1b => Instruction::Rr(R8::R8E),
                0x1c => Instruction::Rr(R8::R8H),
                0x1d => Instruction::Rr(R8::R8L),
                0x1e => Instruction::Rr(R8::R8HLRef),
                0x1f => Instruction::Rr(R8::R8A),
                0x20 => Instruction::Sla(R8::R8B),
                0x21 => Instruction::Sla(R8::R8C),
                0x22 => Instruction::Sla(R8::R8D),
                0x23 => Instruction::Sla(R8::R8E),
                0x24 => Instruction::Sla(R8::R8H),
                0x25 => Instruction::Sla(R8::R8L),
                0x26 => Instruction::Sla(R8::R8HLRef),
                0x27 => Instruction::Sla(R8::R8A),
                0x28 => Instruction::Sra(R8::R8B),
                0x29 => Instruction::Sra(R8::R8C),
                0x2a => Instruction::Sra(R8::R8D),
                0x2b => Instruction::Sra(R8::R8E),
                0x2c => Instruction::Sra(R8::R8H),
                0x2d => Instruction::Sra(R8::R8L),
                0x2e => Instruction::Sra(R8::R8HLRef),
                0x2f => Instruction::Sra(R8::R8A),
                0x30 => Instruction::Swap(R8::R8B),
                0x31 => Instruction::Swap(R8::R8C),
                0x32 => Instruction::Swap(R8::R8D),
                0x33 => Instruction::Swap(R8::R8E),
                0x34 => Instruction::Swap(R8::R8H),
                0x35 => Instruction::Swap(R8::R8L),
                0x36 => Instruction::Swap(R8::R8HLRef),
                0x37 => Instruction::Swap(R8::R8A),
                0x38 => Instruction::Srl(R8::R8B),
                0x39 => Instruction::Srl(R8::R8C),
                0x3a => Instruction::Srl(R8::R8D),
                0x3b => Instruction::Srl(R8::R8E),
                0x3c => Instruction::Srl(R8::R8H),
                0x3d => Instruction::Srl(R8::R8L),
                0x3e => Instruction::Srl(R8::R8HLRef),
                0x3f => Instruction::Srl(R8::R8A),
                0x40 => Instruction::Bit(BitRef::B0, R8::R8B),
                0x41 => Instruction::Bit(BitRef::B0, R8::R8C),
                0x42 => Instruction::Bit(BitRef::B0, R8::R8D),
                0x43 => Instruction::Bit(BitRef::B0, R8::R8E),
                0x44 => Instruction::Bit(BitRef::B0, R8::R8H),
                0x45 => Instruction::Bit(BitRef::B0, R8::R8L),
                0x46 => Instruction::Bit(BitRef::B0, R8::R8HLRef),
                0x47 => Instruction::Bit(BitRef::B0, R8::R8A),
                0x48 => Instruction::Bit(BitRef::B1, R8::R8B),
                0x49 => Instruction::Bit(BitRef::B1, R8::R8C),
                0x4a => Instruction::Bit(BitRef::B1, R8::R8D),
                0x4b => Instruction::Bit(BitRef::B1, R8::R8E),
                0x4c => Instruction::Bit(BitRef::B1, R8::R8H),
                0x4d => Instruction::Bit(BitRef::B1, R8::R8L),
                0x4e => Instruction::Bit(BitRef::B1, R8::R8HLRef),
                0x4f => Instruction::Bit(BitRef::B1, R8::R8A),
                0x50 => Instruction::Bit(BitRef::B2, R8::R8B),
                0x51 => Instruction::Bit(BitRef::B2, R8::R8C),
                0x52 => Instruction::Bit(BitRef::B2, R8::R8D),
                0x53 => Instruction::Bit(BitRef::B2, R8::R8E),
                0x54 => Instruction::Bit(BitRef::B2, R8::R8H),
                0x55 => Instruction::Bit(BitRef::B2, R8::R8L),
                0x56 => Instruction::Bit(BitRef::B2, R8::R8HLRef),
                0x57 => Instruction::Bit(BitRef::B2, R8::R8A),
                0x58 => Instruction::Bit(BitRef::B3, R8::R8B),
                0x59 => Instruction::Bit(BitRef::B3, R8::R8C),
                0x5a => Instruction::Bit(BitRef::B3, R8::R8D),
                0x5b => Instruction::Bit(BitRef::B3, R8::R8E),
                0x5c => Instruction::Bit(BitRef::B3, R8::R8H),
                0x5d => Instruction::Bit(BitRef::B3, R8::R8L),
                0x5e => Instruction::Bit(BitRef::B3, R8::R8HLRef),
                0x5f => Instruction::Bit(BitRef::B3, R8::R8A),
                0x60 => Instruction::Bit(BitRef::B4, R8::R8B),
                0x61 => Instruction::Bit(BitRef::B4, R8::R8C),
                0x62 => Instruction::Bit(BitRef::B4, R8::R8D),
                0x63 => Instruction::Bit(BitRef::B4, R8::R8E),
                0x64 => Instruction::Bit(BitRef::B4, R8::R8H),
                0x65 => Instruction::Bit(BitRef::B4, R8::R8L),
                0x66 => Instruction::Bit(BitRef::B4, R8::R8HLRef),
                0x67 => Instruction::Bit(BitRef::B4, R8::R8A),
                0x68 => Instruction::Bit(BitRef::B5, R8::R8B),
                0x69 => Instruction::Bit(BitRef::B5, R8::R8C),
                0x6a => Instruction::Bit(BitRef::B5, R8::R8D),
                0x6b => Instruction::Bit(BitRef::B5, R8::R8E),
                0x6c => Instruction::Bit(BitRef::B5, R8::R8H),
                0x6d => Instruction::Bit(BitRef::B5, R8::R8L),
                0x6e => Instruction::Bit(BitRef::B5, R8::R8HLRef),
                0x6f => Instruction::Bit(BitRef::B5, R8::R8A),
                0x70 => Instruction::Bit(BitRef::B6, R8::R8B),
                0x71 => Instruction::Bit(BitRef::B6, R8::R8C),
                0x72 => Instruction::Bit(BitRef::B6, R8::R8D),
                0x73 => Instruction::Bit(BitRef::B6, R8::R8E),
                0x74 => Instruction::Bit(BitRef::B6, R8::R8H),
                0x75 => Instruction::Bit(BitRef::B6, R8::R8L),
                0x76 => Instruction::Bit(BitRef::B6, R8::R8HLRef),
                0x77 => Instruction::Bit(BitRef::B6, R8::R8A),
                0x78 => Instruction::Bit(BitRef::B7, R8::R8B),
                0x79 => Instruction::Bit(BitRef::B7, R8::R8C),
                0x7a => Instruction::Bit(BitRef::B7, R8::R8D),
                0x7b => Instruction::Bit(BitRef::B7, R8::R8E),
                0x7c => Instruction::Bit(BitRef::B7, R8::R8H),
                0x7d => Instruction::Bit(BitRef::B7, R8::R8L),
                0x7e => Instruction::Bit(BitRef::B7, R8::R8HLRef),
                0x7f => Instruction::Bit(BitRef::B7, R8::R8A),
                0x80 => Instruction::Res(BitRef::B0, R8::R8B),
                0x81 => Instruction::Res(BitRef::B0, R8::R8C),
                0x82 => Instruction::Res(BitRef::B0, R8::R8D),
                0x83 => Instruction::Res(BitRef::B0, R8::R8E),
                0x84 => Instruction::Res(BitRef::B0, R8::R8H),
                0x85 => Instruction::Res(BitRef::B0, R8::R8L),
                0x86 => Instruction::Res(BitRef::B0, R8::R8HLRef),
                0x87 => Instruction::Res(BitRef::B0, R8::R8A),
                0x88 => Instruction::Res(BitRef::B1, R8::R8B),
                0x89 => Instruction::Res(BitRef::B1, R8::R8C),
                0x8a => Instruction::Res(BitRef::B1, R8::R8D),
                0x8b => Instruction::Res(BitRef::B1, R8::R8E),
                0x8c => Instruction::Res(BitRef::B1, R8::R8H),
                0x8d => Instruction::Res(BitRef::B1, R8::R8L),
                0x8e => Instruction::Res(BitRef::B1, R8::R8HLRef),
                0x8f => Instruction::Res(BitRef::B1, R8::R8A),
                0x90 => Instruction::Res(BitRef::B2, R8::R8B),
                0x91 => Instruction::Res(BitRef::B2, R8::R8C),
                0x92 => Instruction::Res(BitRef::B2, R8::R8D),
                0x93 => Instruction::Res(BitRef::B2, R8::R8E),
                0x94 => Instruction::Res(BitRef::B2, R8::R8H),
                0x95 => Instruction::Res(BitRef::B2, R8::R8L),
                0x96 => Instruction::Res(BitRef::B2, R8::R8HLRef),
                0x97 => Instruction::Res(BitRef::B2, R8::R8A),
                0x98 => Instruction::Res(BitRef::B3, R8::R8B),
                0x99 => Instruction::Res(BitRef::B3, R8::R8C),
                0x9a => Instruction::Res(BitRef::B3, R8::R8D),
                0x9b => Instruction::Res(BitRef::B3, R8::R8E),
                0x9c => Instruction::Res(BitRef::B3, R8::R8H),
                0x9d => Instruction::Res(BitRef::B3, R8::R8L),
                0x9e => Instruction::Res(BitRef::B3, R8::R8HLRef),
                0x9f => Instruction::Res(BitRef::B3, R8::R8A),
                0xa0 => Instruction::Res(BitRef::B4, R8::R8B),
                0xa1 => Instruction::Res(BitRef::B4, R8::R8C),
                0xa2 => Instruction::Res(BitRef::B4, R8::R8D),
                0xa3 => Instruction::Res(BitRef::B4, R8::R8E),
                0xa4 => Instruction::Res(BitRef::B4, R8::R8H),
                0xa5 => Instruction::Res(BitRef::B4, R8::R8L),
                0xa6 => Instruction::Res(BitRef::B4, R8::R8HLRef),
                0xa7 => Instruction::Res(BitRef::B4, R8::R8A),
                0xa8 => Instruction::Res(BitRef::B5, R8::R8B),
                0xa9 => Instruction::Res(BitRef::B5, R8::R8C),
                0xaa => Instruction::Res(BitRef::B5, R8::R8D),
                0xab => Instruction::Res(BitRef::B5, R8::R8E),
                0xac => Instruction::Res(BitRef::B5, R8::R8H),
                0xad => Instruction::Res(BitRef::B5, R8::R8L),
                0xae => Instruction::Res(BitRef::B5, R8::R8HLRef),
                0xaf => Instruction::Res(BitRef::B5, R8::R8A),
                0xb0 => Instruction::Res(BitRef::B6, R8::R8B),
                0xb1 => Instruction::Res(BitRef::B6, R8::R8C),
                0xb2 => Instruction::Res(BitRef::B6, R8::R8D),
                0xb3 => Instruction::Res(BitRef::B6, R8::R8E),
                0xb4 => Instruction::Res(BitRef::B6, R8::R8H),
                0xb5 => Instruction::Res(BitRef::B6, R8::R8L),
                0xb6 => Instruction::Res(BitRef::B6, R8::R8HLRef),
                0xb7 => Instruction::Res(BitRef::B6, R8::R8A),
                0xb8 => Instruction::Res(BitRef::B7, R8::R8B),
                0xb9 => Instruction::Res(BitRef::B7, R8::R8C),
                0xba => Instruction::Res(BitRef::B7, R8::R8D),
                0xbb => Instruction::Res(BitRef::B7, R8::R8E),
                0xbc => Instruction::Res(BitRef::B7, R8::R8H),
                0xbd => Instruction::Res(BitRef::B7, R8::R8L),
                0xbe => Instruction::Res(BitRef::B7, R8::R8HLRef),
                0xbf => Instruction::Res(BitRef::B7, R8::R8A),
                0xc0 => Instruction::Set(BitRef::B0, R8::R8B),
                0xc1 => Instruction::Set(BitRef::B0, R8::R8C),
                0xc2 => Instruction::Set(BitRef::B0, R8::R8D),
                0xc3 => Instruction::Set(BitRef::B0, R8::R8E),
                0xc4 => Instruction::Set(BitRef::B0, R8::R8H),
                0xc5 => Instruction::Set(BitRef::B0, R8::R8L),
                0xc6 => Instruction::Set(BitRef::B0, R8::R8HLRef),
                0xc7 => Instruction::Set(BitRef::B0, R8::R8A),
                0xc8 => Instruction::Set(BitRef::B1, R8::R8B),
                0xc9 => Instruction::Set(BitRef::B1, R8::R8C),
                0xca => Instruction::Set(BitRef::B1, R8::R8D),
                0xcb => Instruction::Set(BitRef::B1, R8::R8E),
                0xcc => Instruction::Set(BitRef::B1, R8::R8H),
                0xcd => Instruction::Set(BitRef::B1, R8::R8L),
                0xce => Instruction::Set(BitRef::B1, R8::R8HLRef),
                0xcf => Instruction::Set(BitRef::B1, R8::R8A),
                0xd0 => Instruction::Set(BitRef::B2, R8::R8B),
                0xd1 => Instruction::Set(BitRef::B2, R8::R8C),
                0xd2 => Instruction::Set(BitRef::B2, R8::R8D),
                0xd3 => Instruction::Set(BitRef::B2, R8::R8E),
                0xd4 => Instruction::Set(BitRef::B2, R8::R8H),
                0xd5 => Instruction::Set(BitRef::B2, R8::R8L),
                0xd6 => Instruction::Set(BitRef::B2, R8::R8HLRef),
                0xd7 => Instruction::Set(BitRef::B2, R8::R8A),
                0xd8 => Instruction::Set(BitRef::B3, R8::R8B),
                0xd9 => Instruction::Set(BitRef::B3, R8::R8C),
                0xda => Instruction::Set(BitRef::B3, R8::R8D),
                0xdb => Instruction::Set(BitRef::B3, R8::R8E),
                0xdc => Instruction::Set(BitRef::B3, R8::R8H),
                0xdd => Instruction::Set(BitRef::B3, R8::R8L),
                0xde => Instruction::Set(BitRef::B3, R8::R8HLRef),
                0xdf => Instruction::Set(BitRef::B3, R8::R8A),
                0xe0 => Instruction::Set(BitRef::B4, R8::R8B),
                0xe1 => Instruction::Set(BitRef::B4, R8::R8C),
                0xe2 => Instruction::Set(BitRef::B4, R8::R8D),
                0xe3 => Instruction::Set(BitRef::B4, R8::R8E),
                0xe4 => Instruction::Set(BitRef::B4, R8::R8H),
                0xe5 => Instruction::Set(BitRef::B4, R8::R8L),
                0xe6 => Instruction::Set(BitRef::B4, R8::R8HLRef),
                0xe7 => Instruction::Set(BitRef::B4, R8::R8A),
                0xe8 => Instruction::Set(BitRef::B5, R8::R8B),
                0xe9 => Instruction::Set(BitRef::B5, R8::R8C),
                0xea => Instruction::Set(BitRef::B5, R8::R8D),
                0xeb => Instruction::Set(BitRef::B5, R8::R8E),
                0xec => Instruction::Set(BitRef::B5, R8::R8H),
                0xed => Instruction::Set(BitRef::B5, R8::R8L),
                0xee => Instruction::Set(BitRef::B5, R8::R8HLRef),
                0xef => Instruction::Set(BitRef::B5, R8::R8A),
                0xf0 => Instruction::Set(BitRef::B6, R8::R8B),
                0xf1 => Instruction::Set(BitRef::B6, R8::R8C),
                0xf2 => Instruction::Set(BitRef::B6, R8::R8D),
                0xf3 => Instruction::Set(BitRef::B6, R8::R8E),
                0xf4 => Instruction::Set(BitRef::B6, R8::R8H),
                0xf5 => Instruction::Set(BitRef::B6, R8::R8L),
                0xf6 => Instruction::Set(BitRef::B6, R8::R8HLRef),
                0xf7 => Instruction::Set(BitRef::B6, R8::R8A),
                0xf8 => Instruction::Set(BitRef::B7, R8::R8B),
                0xf9 => Instruction::Set(BitRef::B7, R8::R8C),
                0xfa => Instruction::Set(BitRef::B7, R8::R8D),
                0xfb => Instruction::Set(BitRef::B7, R8::R8E),
                0xfc => Instruction::Set(BitRef::B7, R8::R8H),
                0xfd => Instruction::Set(BitRef::B7, R8::R8L),
                0xfe => Instruction::Set(BitRef::B7, R8::R8HLRef),
                0xff => Instruction::Set(BitRef::B7, R8::R8A),
            },
            0xd3 | 0xdb | 0xdd | 0xe3 | 0xe4 | 0xeb | 0xec | 0xed | 0xf4 | 0xfc | 0xfd => {
                unreachable!()
            }
        };
        Ok((insn, size))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use serde::{Deserialize, Serialize};

    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    struct Opcode {
        mnemonic: String,
        bytes: usize,
        cycles: Vec<usize>,
        // operands: Vec<String>,
        immediate: bool,
        flags: HashMap<String, String>,
    }

    #[derive(Debug, Serialize, Deserialize)]
    struct OpcodeMap {
        pub unprefixed: HashMap<String, Opcode>,
        pub cbprefixed: HashMap<String, Opcode>,
    }

    #[test]
    fn block0() {
        assert_eq!(Instruction::size_header(0x0).unwrap(), 1);

        assert_eq!(Instruction::size_header(0x7).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x7], 0, 1).unwrap(),
            (Instruction::RlcA, 1)
        );
        assert_eq!(Instruction::size_header(0xF).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xF], 0, 1).unwrap(),
            (Instruction::RrcA, 1)
        );
        assert_eq!(Instruction::size_header(0x17).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x17], 0, 1).unwrap(),
            (Instruction::RlA, 1)
        );
        assert_eq!(Instruction::size_header(0x1F).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x1F], 0, 1).unwrap(),
            (Instruction::RrA, 1)
        );
        assert_eq!(Instruction::size_header(0x27).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x27], 0, 1).unwrap(),
            (Instruction::Daa, 1)
        );
        assert_eq!(Instruction::size_header(0x2F).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x2F], 0, 1).unwrap(),
            (Instruction::Cpl, 1)
        );
        assert_eq!(Instruction::size_header(0x37).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x37], 0, 1).unwrap(),
            (Instruction::Scf, 1)
        );
        assert_eq!(Instruction::size_header(0x3F).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x3F], 0, 1).unwrap(),
            (Instruction::Ccf, 1)
        );

        assert_eq!(Instruction::size_header(0x9).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x9], 0, 1).unwrap(),
            (Instruction::AddHLR16(R16::R16BC), 1)
        );

        assert_eq!(Instruction::size_header(0x1).unwrap(), 3);
        assert_eq!(
            Instruction::from_u8_slice(&[0x1, 0x34, 0x12], 0, 3).unwrap(),
            (Instruction::LoadR16N16(R16::R16BC, 0x1234), 3)
        );

        assert_eq!(Instruction::size_header(0x2).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x2], 0, 1).unwrap(),
            (Instruction::StoreAR16Mem(R16Mem::R16MemBC), 1)
        );

        assert_eq!(Instruction::size_header(0xa).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xa], 0, 1).unwrap(),
            (Instruction::LoadAR16Mem(R16Mem::R16MemBC), 1)
        );

        assert_eq!(Instruction::size_header(0x8).unwrap(), 3);
        assert_eq!(
            Instruction::from_u8_slice(&[0x8, 0x34, 0x12], 0, 3).unwrap(),
            (Instruction::StoreSPA16(0x1234), 3)
        );

        assert_eq!(Instruction::size_header(0x3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x3], 0, 1).unwrap(),
            (Instruction::IncR16(R16::R16BC), 1)
        );

        assert_eq!(Instruction::size_header(0xb).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xb], 0, 1).unwrap(),
            (Instruction::DecR16(R16::R16BC), 1)
        );

        assert_eq!(Instruction::size_header(0x4).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x4], 0, 1).unwrap(),
            (Instruction::IncR8(R8::R8B), 1)
        );

        assert_eq!(Instruction::size_header(0x5).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x5], 0, 1).unwrap(),
            (Instruction::DecR8(R8::R8B), 1)
        );

        assert_eq!(Instruction::size_header(0x6).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0x6, 0x34], 0, 2).unwrap(),
            (Instruction::LoadR8N8(R8::R8B, 0x34), 2)
        );

        assert_eq!(Instruction::size_header(0x18).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0x18, 0x34], 0, 2).unwrap(),
            (Instruction::JumpNear(0x34), 2)
        );
        assert_eq!(Instruction::size_header(0x38).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0x38, 0x34], 0, 2).unwrap(),
            (Instruction::JumpNearCond(Cond::CondC, 0x34), 2)
        );

        assert_eq!(Instruction::size_header(0xC3).unwrap(), 3);
        assert_eq!(
            Instruction::from_u8_slice(&[0xC3, 0x34, 0x12], 0, 3).unwrap(),
            (Instruction::JumpFar(0x1234), 3)
        );
        assert_eq!(Instruction::size_header(0xCA).unwrap(), 3);
        assert_eq!(
            Instruction::from_u8_slice(&[0xCA, 0x34, 0x12], 0, 3).unwrap(),
            (Instruction::JumpFarCond(Cond::CondZ, 0x1234), 3)
        );

        assert_eq!(Instruction::size_header(0x10).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0x10, 0x34], 0, 2).unwrap(),
            (Instruction::StopN8(0x34), 2)
        );
    }

    #[test]
    fn block1() {
        assert_eq!(Instruction::size_header(0x40).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x40], 0, 1).unwrap(),
            (Instruction::LoadR8R8(R8::R8B, R8::R8B), 1)
        );

        assert_eq!(Instruction::size_header(0x76).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x76], 0, 1).unwrap(),
            (Instruction::Halt, 1)
        );
    }

    #[test]
    fn block2() {
        assert_eq!(Instruction::size_header(0x10 << 3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x10 << 3], 0, 1).unwrap(),
            (Instruction::AddR8(R8::R8B), 1)
        );

        assert_eq!(Instruction::size_header(0x11 << 3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x11 << 3], 0, 1).unwrap(),
            (Instruction::AdcR8(R8::R8B), 1)
        );
        assert_eq!(Instruction::size_header(0x12 << 3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x12 << 3], 0, 1).unwrap(),
            (Instruction::SubR8(R8::R8B), 1)
        );
        assert_eq!(Instruction::size_header(0x13 << 3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x13 << 3], 0, 1).unwrap(),
            (Instruction::SbcR8(R8::R8B), 1)
        );
        assert_eq!(Instruction::size_header(0x14 << 3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x14 << 3], 0, 1).unwrap(),
            (Instruction::AndR8(R8::R8B), 1)
        );
        assert_eq!(Instruction::size_header(0x15 << 3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x15 << 3], 0, 1).unwrap(),
            (Instruction::XorR8(R8::R8B), 1)
        );
        assert_eq!(Instruction::size_header(0x16 << 3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x16 << 3], 0, 1).unwrap(),
            (Instruction::OrR8(R8::R8B), 1)
        );
        assert_eq!(Instruction::size_header(0x17 << 3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x17 << 3], 0, 1).unwrap(),
            (Instruction::CpR8(R8::R8B), 1)
        );
    }

    #[test]
    fn block3() {
        assert_eq!(Instruction::size_header(0xC6).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0xC6, 0x12], 0, 2).unwrap(),
            (Instruction::AddAN8(0x12), 2)
        );
        assert_eq!(Instruction::size_header(0xCE).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0xCE, 0x12], 0, 2).unwrap(),
            (Instruction::AdcAN8(0x12), 2)
        );
        assert_eq!(Instruction::size_header(0xD6).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0xD6, 0x12], 0, 2).unwrap(),
            (Instruction::SubAN8(0x12), 2)
        );
        assert_eq!(Instruction::size_header(0xDE).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0xDE, 0x12], 0, 2).unwrap(),
            (Instruction::SbcAN8(0x12), 2)
        );
        assert_eq!(Instruction::size_header(0xE6).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0xE6, 0x12], 0, 2).unwrap(),
            (Instruction::AndAN8(0x12), 2)
        );
        assert_eq!(Instruction::size_header(0xEE).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0xEE, 0x12], 0, 2).unwrap(),
            (Instruction::XorAN8(0x12), 2)
        );
        assert_eq!(Instruction::size_header(0xF6).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0xF6, 0x12], 0, 2).unwrap(),
            (Instruction::OrAN8(0x12), 2)
        );
        assert_eq!(Instruction::size_header(0xFE).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0xFE, 0x12], 0, 2).unwrap(),
            (Instruction::CpAN8(0x12), 2)
        );

        assert_eq!(Instruction::size_header(0xD9).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xD9], 0, 1).unwrap(),
            (Instruction::Reti, 1)
        );
        assert_eq!(Instruction::size_header(0xF3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xF3], 0, 1).unwrap(),
            (Instruction::Di, 1)
        );
        assert_eq!(Instruction::size_header(0xFB).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xFb], 0, 1).unwrap(),
            (Instruction::Ei, 1)
        );

        assert_eq!(Instruction::size_header(0xc9).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xc9], 0, 1).unwrap(),
            (Instruction::Ret, 1)
        );
        assert_eq!(Instruction::size_header(0xc0).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xc0], 0, 1).unwrap(),
            (Instruction::RetCond(Cond::CondNZ), 1)
        );

        assert_eq!(Instruction::size_header(0xdf).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xdf], 0, 1).unwrap(),
            (Instruction::Rst(Tgt3::T3), 1)
        );

        assert_eq!(Instruction::size_header(0xcd).unwrap(), 3);
        assert_eq!(
            Instruction::from_u8_slice(&[0xcd, 0x34, 0x12], 0, 3).unwrap(),
            (Instruction::CallA16(0x1234), 3)
        );

        assert_eq!(Instruction::size_header(0xc4).unwrap(), 3);
        assert_eq!(
            Instruction::from_u8_slice(&[0xc4, 0x34, 0x12], 0, 3).unwrap(),
            (Instruction::CallCondA16(Cond::CondNZ, 0x1234), 3)
        );

        assert_eq!(Instruction::size_header(0xc1).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xc1], 0, 1).unwrap(),
            (Instruction::PopR16Stack(R16Stack::R16StackBC), 1)
        );

        assert_eq!(Instruction::size_header(0xc5).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xc5], 0, 1).unwrap(),
            (Instruction::PushR16Stack(R16Stack::R16StackBC), 1)
        );

        assert_eq!(Instruction::size_header(0xe2).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xe2], 0, 1).unwrap(),
            (Instruction::StoreACH, 1)
        );

        assert_eq!(Instruction::size_header(0xe0).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0xe0, 0x12], 0, 2).unwrap(),
            (Instruction::StoreAA8H(0x12), 2)
        );

        assert_eq!(Instruction::size_header(0xea).unwrap(), 3);
        assert_eq!(
            Instruction::from_u8_slice(&[0xea, 0x34, 0x12], 0, 3).unwrap(),
            (Instruction::StoreAA16(0x1234), 3)
        );

        assert_eq!(Instruction::size_header(0xf2).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xf2], 0, 1).unwrap(),
            (Instruction::LoadACH, 1)
        );
        assert_eq!(Instruction::size_header(0xf0).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0xf0, 0x12], 0, 2).unwrap(),
            (Instruction::LoadAA8H(0x12), 2)
        );
        assert_eq!(Instruction::size_header(0xfa).unwrap(), 3);
        assert_eq!(
            Instruction::from_u8_slice(&[0xfa, 0x34, 0x12], 0, 3).unwrap(),
            (Instruction::LoadAA16(0x1234), 3)
        );

        assert_eq!(Instruction::size_header(0xC3).unwrap(), 3);
        assert_eq!(
            Instruction::from_u8_slice(&[0xC3, 0x34, 0x12], 0, 3).unwrap(),
            (Instruction::JumpFar(0x1234), 3)
        );

        assert_eq!(Instruction::size_header(0xCA).unwrap(), 3);
        assert_eq!(
            Instruction::from_u8_slice(&[0xCA, 0x34, 0x12], 0, 3).unwrap(),
            (Instruction::JumpFarCond(Cond::CondZ, 0x1234), 3)
        );

        assert_eq!(Instruction::size_header(0xF8).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0xF8, 0x12], 0, 2).unwrap(),
            (Instruction::LoadHLSPN8(0x12), 2)
        );

        assert_eq!(Instruction::size_header(0xE9).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xE9], 0, 1).unwrap(),
            (Instruction::JumpFarHL, 1)
        );
        assert_eq!(Instruction::size_header(0xF9).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xF9], 0, 1).unwrap(),
            (Instruction::LoadHLSP, 1)
        );
    }

    #[test]
    fn prefixed() {
        assert_eq!(Instruction::size_header(0xCB).unwrap(), 2);

        assert_eq!(
            Instruction::from_u8_slice(&[0xCB, 0x00], 0, 2).unwrap(),
            (Instruction::Rlc(R8::R8B), 2)
        );
        assert_eq!(
            Instruction::from_u8_slice(&[0xCB, 0x08], 0, 2).unwrap(),
            (Instruction::Rrc(R8::R8B), 2)
        );
        assert_eq!(
            Instruction::from_u8_slice(&[0xCB, 0x10], 0, 2).unwrap(),
            (Instruction::Rl(R8::R8B), 2)
        );
        assert_eq!(
            Instruction::from_u8_slice(&[0xCB, 0x18], 0, 2).unwrap(),
            (Instruction::Rr(R8::R8B), 2)
        );
        assert_eq!(
            Instruction::from_u8_slice(&[0xCB, 0x20], 0, 2).unwrap(),
            (Instruction::Sla(R8::R8B), 2)
        );
        assert_eq!(
            Instruction::from_u8_slice(&[0xCB, 0x28], 0, 2).unwrap(),
            (Instruction::Sra(R8::R8B), 2)
        );
        assert_eq!(
            Instruction::from_u8_slice(&[0xCB, 0x30], 0, 2).unwrap(),
            (Instruction::Swap(R8::R8B), 2)
        );
        assert_eq!(
            Instruction::from_u8_slice(&[0xCB, 0x38], 0, 2).unwrap(),
            (Instruction::Srl(R8::R8B), 2)
        );

        assert_eq!(
            Instruction::from_u8_slice(&[0xCB, 0x40], 0, 2).unwrap(),
            (Instruction::Bit(BitRef::B0, R8::R8B), 2)
        );
        assert_eq!(
            Instruction::from_u8_slice(&[0xCB, 0x80], 0, 2).unwrap(),
            (Instruction::Res(BitRef::B0, R8::R8B), 2)
        );
        assert_eq!(
            Instruction::from_u8_slice(&[0xCB, 0xC0], 0, 2).unwrap(),
            (Instruction::Set(BitRef::B0, R8::R8B), 2)
        );
    }

    // regression tests
    #[test]
    fn decode_bc_store() {
        assert_eq!(Instruction::size_header(0x2).unwrap(), 1);
        let buf = [0x2];
        let (insn, size) = Instruction::from_u8_slice(&buf, 0, 1).unwrap();
        assert_eq!(size, 1);
        assert_eq!(insn, Instruction::StoreAR16Mem(R16Mem::R16MemBC));
        let encoded = insn.encode();
        assert_eq!(encoded, buf);
    }

    #[test]
    fn dec_b() {
        assert_eq!(Instruction::size_header(0x5).unwrap(), 1);
        let buf = [0x5];
        let (insn, size) = Instruction::from_u8_slice(&buf, 0, 1).unwrap();
        assert_eq!(size, 1);
        assert_eq!(insn, Instruction::DecR8(R8::R8B));
        let encoded = insn.encode();
        assert_eq!(encoded, buf);
    }

    #[test]
    fn load_from_bc_ref() {
        assert_eq!(Instruction::size_header(0xa).unwrap(), 1);
        let buf = [0xa];
        let (insn, size) = Instruction::from_u8_slice(&buf, 0, 1).unwrap();
        assert_eq!(size, 1);
        assert_eq!(insn, Instruction::LoadAR16Mem(R16Mem::R16MemBC));
        let encoded = insn.encode();
        assert_eq!(encoded, buf);
    }

    #[test]
    fn stop() {
        assert_eq!(Instruction::size_header(0x10).unwrap(), 2);
        let buf = [0x10, 0x34];
        let (insn, size) = Instruction::from_u8_slice(&buf, 0, 2).unwrap();
        assert_eq!(size, 2);
        assert_eq!(insn, Instruction::StopN8(0x34));
        let encoded = insn.encode();
        assert_eq!(encoded, buf);
    }

    #[test]
    fn xor_b() {
        assert_eq!(Instruction::size_header(0xa8).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xa8], 0, 1).unwrap(),
            (Instruction::XorR8(R8::R8B), 1)
        );
    }

    #[test]
    fn reencode() {
        let payload = [0x34, 0x12];
        for opcode in 0..=0xFF {
            if opcode == 0xCB || ILLEGAL_INSTRUCTIONS.contains(&opcode) {
                continue;
            }
            let size = Instruction::size_header(opcode).unwrap() as usize;
            let buf = &[opcode, payload[0], payload[1]][..size];
            let (insn, _) = Instruction::from_u8_slice(buf, 0, size).unwrap();
            let encoded = insn.encode();
            assert_eq!(encoded, buf);
        }
        for opcode in 0..=0xFF {
            let (insn, _) = Instruction::from_u8_slice(&[0xCB, opcode], 0, 2).unwrap();
            let encoded = insn.encode();
            assert_eq!(encoded, [0xCB, opcode]);
        }
    }
}
