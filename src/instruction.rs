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
    BC,
    DE,
    HL,
    SP,
}

impl R16 {
    fn as_operand(&self) -> u8 {
        match self {
            R16::BC => 0b00,
            R16::DE => 0b01,
            R16::HL => 0b10,
            R16::SP => 0b11,
        }
    }
    fn from_operand(operand: u8) -> Self {
        match operand {
            0b00 => R16::BC,
            0b01 => R16::DE,
            0b10 => R16::HL,
            0b11 => R16::SP,
            _ => unimplemented!("R16::from_operand({:#04x})", operand),
        }
    }
}

impl Display for R16 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            R16::BC => write!(f, "BC"),
            R16::DE => write!(f, "DE"),
            R16::HL => write!(f, "HL"),
            R16::SP => write!(f, "SP"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum R16Mem {
    BC,
    DE,
    HLInc,
    HLDec,
}

impl R16Mem {
    fn as_operand(&self) -> u8 {
        match self {
            R16Mem::BC => 0b00,
            R16Mem::DE => 0b01,
            R16Mem::HLInc => 0b10,
            R16Mem::HLDec => 0b11,
        }
    }

    fn from_operand(operand: u8) -> Self {
        match operand {
            0b00 => R16Mem::BC,
            0b01 => R16Mem::DE,
            0b10 => R16Mem::HLInc,
            0b11 => R16Mem::HLDec,
            _ => unimplemented!("R16Mem::from_operand({:#04x})", operand),
        }
    }
}

impl Display for R16Mem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            R16Mem::BC => write!(f, "[BC]"),
            R16Mem::DE => write!(f, "[DE]"),
            R16Mem::HLInc => write!(f, "[HL+]"),
            R16Mem::HLDec => write!(f, "[HL-]"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum R8 {
    B,
    C,
    D,
    E,
    H,
    L,
    HLRef, // (HL)
    A,
}

impl Display for R8 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            R8::B => write!(f, "B"),
            R8::C => write!(f, "C"),
            R8::D => write!(f, "D"),
            R8::E => write!(f, "E"),
            R8::H => write!(f, "H"),
            R8::L => write!(f, "L"),
            R8::HLRef => write!(f, "(HL)"),
            R8::A => write!(f, "A"),
        }
    }
}

impl R8 {
    fn as_operand(&self) -> u8 {
        match self {
            R8::B => 0b000,
            R8::C => 0b001,
            R8::D => 0b010,
            R8::E => 0b011,
            R8::H => 0b100,
            R8::L => 0b101,
            R8::HLRef => 0b110,
            R8::A => 0b111,
        }
    }

    fn from_operand(operand: u8) -> Self {
        match operand {
            0b000 => R8::B,
            0b001 => R8::C,
            0b010 => R8::D,
            0b011 => R8::E,
            0b100 => R8::H,
            0b101 => R8::L,
            0b110 => R8::HLRef,
            0b111 => R8::A,
            _ => unimplemented!("LoadOperand::from_operand({:#04x})", operand),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum R16Stack {
    BC,
    DE,
    HL,
    AF,
}

impl R16Stack {
    fn as_operand(&self) -> u8 {
        match self {
            R16Stack::BC => 0b00,
            R16Stack::DE => 0b01,
            R16Stack::HL => 0b10,
            R16Stack::AF => 0b11,
        }
    }
}

impl Display for R16Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            R16Stack::BC => write!(f, "BC"),
            R16Stack::DE => write!(f, "DE"),
            R16Stack::HL => write!(f, "HL"),
            R16Stack::AF => write!(f, "AF"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Condition {
    NZ,
    Z,
    NC,
    C,
}

impl Condition {
    fn from_operand(operand: u8) -> Self {
        match operand {
            0b00 => Condition::NZ,
            0b01 => Condition::Z,
            0b10 => Condition::NC,
            0b11 => Condition::C,
            _ => unimplemented!("Conditions::from_operand({:#04x})", operand),
        }
    }

    fn as_operand(&self) -> u8 {
        match self {
            Condition::NZ => 0b00,
            Condition::Z => 0b01,
            Condition::NC => 0b10,
            Condition::C => 0b11,
        }
    }
}

impl Display for Condition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Condition::NZ => write!(f, "NZ"),
            Condition::Z => write!(f, "Z"),
            Condition::NC => write!(f, "NC"),
            Condition::C => write!(f, "C"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Instruction {
    Nop,                  // 0x0
    LoadR16N16(R16, N16), // 0x1
    StoreAR16Mem(R16Mem), // 0x2
    IncR16(R16),          // 0x3
    IncR8(R8),            // 0x4
    DecR8(R8),            // 0x5
    LoadR8N8(R8, N8),     // 0x6
    // rlca - 0x7
    StoreSPA16(A16), // 0x8
    // ADDHLR16 - 0x9
    LoadAR16Mem(R16Mem), // 0xA
    DecR16(R16),         // 0xB
    // rrca - 0xF
    StopN8(N8), // 0x10
    // RLA - 0x17
    JumpNear(E8), // 0x18
    // RRA - 0x1f
    JumpNearCond(E8, Condition), // 0x20
    // daa - 0x27
    // cpl - 0x2f
    // scf - 0x37
    // ccf - 0x3f
    LoadR8R8(R8, R8),      // 0x40
    Halt,                  // 0x76
    ADDR8(R8),             // 0x80
    ADCR8(R8),             // 0x88
    SUBR8(R8),             // 0x90
    SBCR8(R8),             // 0x98
    ANDR8(R8),             // 0xA0
    XORR8(R8),             // 0xA8
    ORR8(R8),              // 0xB0
    CPR8(R8),              // 0xB8
    RetCond(Condition),    // 0xc0
    PopR16Stack(R16Stack), // 0xc1
    // jp cond // 0xC2
    // jp // 0xc3
    CallCondA16(Condition, A16), // 0xc4
    PushR16Stack(R16Stack),      // 0xC5
    // ADDAImm8 // 0xC6
    // rst tgt3 // 0xC7
    Ret, // 0xc0
    // prefix // 0xcb
    CallA16(A16), // 0xcd
    // ADCAImm8 // 0xce
    // SUBAImm8 // 0xD6
    // reti  // 0xD9
    // SBCAImm8 // 0xDE
    StoreAA8H(A8), // 0xE0
    StoreACH,      // 0xE2
    // ANDAImm8 // 0xE6
    // ADDSPImm8 // 0xE8
    // jp hl // 0xE9
    StoreAA16(A16), // 0xEA
    // XORAImm8 // 0xEE
    LoadAA8H(N8), // 0xF0
    LoadACH,      // 0xF2
    // di // 0xF3
    // ORAImm8 // 0xF6
    // LoadHLSPImm8 // 0xF8
    // LoadHLSP // 0xF9
    LoadAA16(A16), // 0xFA
    // ei // 0xFB
    // CPAImm8 // 0xFE

    // Prefixed instructions
    // RLC  - 0x00
    // RRC  - 0x08
    // RL   - 0x10
    // RR   - 0x18
    // SLA  - 0x20
    // SRA  - 0x28
    // SWAP - 0x30
    // SRL  - 0x38
    BIT(BitRef, R8),   // 0x40
    RESET(BitRef, R8), // 0x80
    SET(BitRef, R8),   // 0xC0
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
            Instruction::Nop => write!(f, "NOP"),
            Instruction::Halt => write!(f, "HALT"),
            Instruction::StopN8(val) => write!(f, "STOP {:#02x}", val),

            Instruction::LoadR16N16(r16, imm16) => write!(f, "LD {}, {}", r16, imm16),
            Instruction::LoadAR16Mem(r16mem) => write!(f, "LD A, [{}]", r16mem),
            Instruction::LoadAA16(imm16) => write!(f, "LD A, [{}]", imm16),
            Instruction::LoadR8N8(r8, imm8) => write!(f, "LD {}, {}", r8, imm8),
            Instruction::LoadR8R8(r8_dst, r8_src) => write!(f, "LD {}, {}", r8_dst, r8_src),
            Instruction::LoadACH => write!(f, "LDH A, [C]"),
            Instruction::LoadAA8H(imm8) => write!(f, "LDH A, [0x{:02x}]", imm8),

            Instruction::StoreAA16(imm16) => write!(f, "LD [{}], A", imm16),
            Instruction::StoreACH => write!(f, "LDH [C], A"),
            Instruction::StoreAR16Mem(dest) => write!(f, "LD [{}], A", dest),
            Instruction::StoreAA8H(imm8) => write!(f, "LDH [0x{:02x}], A", imm8),
            Instruction::StoreSPA16(imm16) => write!(f, "LD [{}], SP", imm16),

            Instruction::PushR16Stack(operand) => write!(f, "PUSH {}", operand),
            Instruction::PopR16Stack(operand) => write!(f, "POP {}", operand),
            Instruction::CallA16(a16) => write!(f, "CALL {:#04x}", a16),
            Instruction::CallCondA16(cond, a16) => write!(f, "CALL {}, {:#04x}", cond, a16),
            Instruction::JumpNear(e8) => write!(f, "JR {:+}", e8),
            Instruction::JumpNearCond(e8, cond) => write!(f, "JR {}, {:+}", cond, e8),
            Instruction::Ret => write!(f, "RET"),
            Instruction::RetCond(cond) => write!(f, "RET {}", cond),
            Instruction::IncR16(r16) => write!(f, "INC {}", r16),
            Instruction::IncR8(r8) => write!(f, "INC {}", r8),
            Instruction::DecR16(r16) => write!(f, "DEC {}", r16),
            Instruction::DecR8(r8) => write!(f, "DEC {}", r8),

            Instruction::ADDR8(source) => write!(f, "ADD A, {}", source),
            Instruction::ADCR8(source) => write!(f, "ADC A, {}", source),
            Instruction::SUBR8(source) => write!(f, "SUB A, {}", source),
            Instruction::SBCR8(source) => write!(f, "SBC A, {}", source),
            Instruction::ANDR8(source) => write!(f, "AND A, {}", source),
            Instruction::XORR8(source) => write!(f, "XOR A, {}", source),
            Instruction::ORR8(source) => write!(f, "OR A, {}", source),
            Instruction::CPR8(source) => write!(f, "CP A, {}", source),

            Instruction::BIT(b3, r8) => write!(f, "BIT {}, {}", b3, r8),
            Instruction::RESET(b3, r8) => write!(f, "RES {}, {}", b3, r8),
            Instruction::SET(b3, r8) => write!(f, "SET {}, {}", b3, r8),
        }
    }
}

impl Instruction {
    pub fn size_header(insn: u8) -> Result<u8, InstructionError> {
        if ILLEGAL_INSTRUCTIONS.contains(&insn) {
            return Err(InstructionError::Illegal(insn).into());
        }
        let size = if insn == 0xCB {
            2
        } else {
            match insn >> 6 {
                0b00 => match insn & 0x0F {
                    0x0 if insn == 0x0 => 1,  // NOP
                    0x0 if insn == 0x10 => 2, // STOP
                    0x1 => 3,                 // LD r16, imm16
                    0x2 => 1,                 // LD [r16mem], A
                    0x3 => 1,                 // INC r16
                    0x8 if insn == 0x8 => 3,  // LD [imm16], SP
                    0xa => 1,                 // LD A, [r16mem]
                    0xb => 1,                 // DEC r16
                    _ => match insn & 0x7 {
                        0x0 => 2, // JR e8
                        0x4 => 1, // INC r8
                        0x5 => 1, // DEC r8
                        0x6 => 2, // LD r8, imm8
                        _ => return Err(InstructionError::Unknown(insn).into()),
                    },
                },
                0b01 => 1,
                0b10 => 1,
                0b11 => match insn {
                    0xe0 => 2,
                    0xcd => 3,
                    0xea => 3, // ld [imm16], a
                    0xf0 => 2, // ldh a, [imm8]
                    0xfa => 3, // ld a, [imm16]
                    0xe2 | 0xf2 => 1,
                    0xc4 => 3,                             // call cond, imm16
                    0xf5 | 0xe5 | 0xd5 | 0xc5 => 1,        // push
                    0xc1 | 0xd1 | 0xe1 | 0xf1 => 1,        // pop
                    0xc0 | 0xc8 | 0xd0 | 0xd8 | 0xc9 => 1, // ret
                    _ => return Err(InstructionError::Unknown(insn).into()),
                },
                _ => return Err(InstructionError::Unknown(insn).into()),
            }
        };
        // println!("size_header({:#02x}) = {}", insn, size);
        Ok(size)
    }

    pub fn encode(&self) -> Vec<u8> {
        match self {
            Instruction::Nop => vec![0x0],
            Instruction::Halt => vec![0x76],
            Instruction::StopN8(n8) => vec![0x10, *n8],
            Instruction::CallA16(a16) => vec![0xcd, *a16 as u8, (*a16 >> 8) as u8],
            Instruction::CallCondA16(cond, a16) => {
                vec![0xc4 | cond.as_operand(), *a16 as u8, (*a16 >> 8) as u8]
            }
            Instruction::JumpNear(e8) => vec![0x18, *e8 as u8],
            Instruction::JumpNearCond(e8, cond) => {
                let cond = match cond {
                    Condition::NZ => 0b00,
                    Condition::Z => 0b01,
                    Condition::NC => 0b10,
                    Condition::C => 0b11,
                };
                vec![0x20 | cond << 3, *e8 as u8]
            }
            Instruction::Ret => vec![0xc9],
            Instruction::RetCond(cond) => {
                let cond = match cond {
                    Condition::NZ => 0b00,
                    Condition::Z => 0b01,
                    Condition::NC => 0b10,
                    Condition::C => 0b11,
                };
                vec![0xc0 | cond << 3]
            }
            Instruction::LoadR16N16(op, val) => {
                vec![0x01 | op.as_operand() << 4, *val as u8, (*val >> 8) as u8]
            }
            Instruction::LoadAR16Mem(mem) => vec![0xa | mem.as_operand() << 4],
            Instruction::LoadAA16(addr) => vec![0xfb, *addr as u8, (*addr >> 8) as u8],
            Instruction::LoadR8N8(dest, val) => {
                vec![0x06 | dest.as_operand() << 3, *val]
            }
            Instruction::LoadR8R8(target, source) => {
                vec![0b01_000_000 | target.as_operand() << 3 | source.as_operand()]
            }
            Instruction::LoadACH => vec![0xf2],
            Instruction::LoadAA8H(addr) => vec![0xf0, *addr],
            Instruction::StoreACH => vec![0xe2],
            Instruction::StoreAA16(addr) => {
                vec![0xEA, *addr as u8, (*addr >> 8) as u8]
            }
            Instruction::StoreAR16Mem(dest) => {
                vec![0x2 | dest.as_operand() << 4]
            }
            Instruction::StoreAA8H(addr) => {
                vec![0xe0, (*addr & 0xff) as u8]
            }
            Instruction::StoreSPA16(imm16) => {
                vec![0x08, *imm16 as u8, (*imm16 >> 8) as u8]
            }
            Instruction::PushR16Stack(operand) => {
                vec![operand.as_operand() << 4 | 0b11_000_101]
            }
            Instruction::PopR16Stack(operand) => {
                vec![operand.as_operand() << 4 | 0b11_000_001]
            }
            Instruction::IncR16(r16) => {
                vec![0x3 | r16.as_operand() << 4]
            }
            Instruction::IncR8(r8) => {
                vec![0x4 | r8.as_operand() << 3]
            }
            Instruction::DecR16(r16) => {
                vec![0xb | r16.as_operand() << 4]
            }
            Instruction::DecR8(r8) => {
                vec![0x5 | r8.as_operand() << 3]
            }

            Instruction::ADDR8(source) => vec![0x10 << 3 | source.as_operand()],
            Instruction::ADCR8(source) => vec![0x11 << 3 | source.as_operand()],
            Instruction::SUBR8(source) => vec![0x12 << 3 | source.as_operand()],
            Instruction::SBCR8(source) => vec![0x13 << 3 | source.as_operand()],
            Instruction::ANDR8(source) => vec![0x14 << 3 | source.as_operand()],
            Instruction::XORR8(source) => vec![0x15 << 3 | source.as_operand()],
            Instruction::ORR8(source) => vec![0x16 << 3 | source.as_operand()],
            Instruction::CPR8(source) => vec![0x17 << 3 | source.as_operand()],
            Instruction::BIT(b3, r8) => {
                vec![0xCB, 0b01_000_000 | b3.encode() << 3 | r8.as_operand()]
            }
            Instruction::RESET(b3, r8) => {
                vec![0xCB, 0b10_000_000 | b3.encode() << 3 | r8.as_operand()]
            }
            Instruction::SET(b3, r8) => {
                vec![0xCB, 0b11_000_000 | b3.encode() << 3 | r8.as_operand()]
            }
        }
    }

    pub fn size(&self) -> u8 {
        match self {
            Instruction::Nop => 1,
            Instruction::Halt => 1,
            Instruction::StopN8(_) => 2,
            Instruction::CallA16(_) => 3,
            Instruction::CallCondA16(_, _) => 3,
            Instruction::JumpNear(_) => 2,
            Instruction::JumpNearCond(_, _) => 2,
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

            Instruction::ADDR8(_) => 1,
            Instruction::ADCR8(_) => 1,
            Instruction::SUBR8(_) => 1,
            Instruction::SBCR8(_) => 1,
            Instruction::ANDR8(_) => 1,
            Instruction::XORR8(_) => 1,
            Instruction::ORR8(_) => 1,
            Instruction::CPR8(_) => 1,

            Instruction::BIT(_, _) => 2,
            Instruction::RESET(_, _) => 2,
            Instruction::SET(_, _) => 2,
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
            Instruction::CallA16(_) => (24, 24),
            Instruction::CallCondA16(_, _) => (24, 12),
            Instruction::JumpNear(_) => (12, 8),
            Instruction::JumpNearCond(_, _) => (12, 8),
            Instruction::Ret => (16, 16),
            Instruction::RetCond(_) => (20, 8),

            Instruction::LoadAR16Mem(_) => (8, 8),
            Instruction::LoadR16N16(_, _) => (12, 12),
            Instruction::LoadAA16(_) => (16, 16),
            Instruction::LoadR8N8(_, _) => (8, 8),
            Instruction::LoadR8R8(R8::HLRef, _) => (8, 8),
            Instruction::LoadR8R8(_, R8::HLRef) => (8, 8),
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

            Instruction::ADDR8(_) => (4, 4),
            Instruction::ADCR8(_) => (4, 4),
            Instruction::SUBR8(_) => (4, 4),
            Instruction::SBCR8(_) => (4, 4),
            Instruction::ANDR8(_) => (4, 4),
            Instruction::XORR8(_) => (4, 4),
            Instruction::ORR8(_) => (4, 4),
            Instruction::CPR8(_) => (4, 4),

            Instruction::BIT(_, _) => (8, 8),
            Instruction::RESET(_, _) => (8, 8),
            Instruction::SET(_, _) => (8, 8),
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

    pub fn from_u8_slice(
        buf: &[u8],
        addr: u16,
        min_size: usize,
    ) -> Result<(Instruction, u8), InstructionError> {
        if addr as usize + min_size > buf.len() {
            return Err(InstructionError::Incomplete(buf[addr as usize..].to_vec()).into());
        }

        let byte = buf[addr as usize];
        if ILLEGAL_INSTRUCTIONS.contains(&byte) {
            return Err(InstructionError::Illegal(byte).into());
        }
        let quad = byte >> 6;
        let result = match quad & 0b11 {
            // load quadrant
            0b00 => match byte & 0x0F {
                0x0 if byte == 0x0 => Instruction::Nop,
                0x0 if byte == 0x10 => Instruction::StopN8(Self::read_u8_helper(buf, addr + 1)), // STOP
                0x1 => Instruction::LoadR16N16(
                    R16::from_operand(byte >> 4 & 0xf),
                    Self::read_u16_helper(buf, addr + 1),
                ),
                0x2 => Instruction::StoreAR16Mem(R16Mem::from_operand(byte >> 4 & 0xf)),
                0x3 => Instruction::IncR16(R16::from_operand(byte >> 4 & 0x3)),
                0x8 if byte == 0x8 => Instruction::StoreSPA16(Self::read_u16_helper(buf, addr + 1)),
                0xa => Instruction::LoadAR16Mem(R16Mem::from_operand(byte >> 4 & 0xf)),
                0xb => Instruction::DecR16(R16::from_operand(byte >> 4 & 0x3)),
                _ => match byte & 0x7 {
                    0x0 => {
                        if byte & 0b0010_0000 == 0 {
                            Instruction::JumpNear(Self::read_u8_helper(buf, addr + 1) as i8)
                        } else {
                            Instruction::JumpNearCond(
                                Self::read_u8_helper(buf, addr + 1) as i8,
                                Condition::from_operand((byte >> 3) & 0x3),
                            )
                        }
                    }
                    0x4 => Instruction::IncR8(R8::from_operand(byte >> 3 & 0x7)),
                    0x5 => Instruction::DecR8(R8::from_operand(byte >> 3 & 0x7)),
                    0x6 => Instruction::LoadR8N8(
                        R8::from_operand(byte >> 3),
                        Self::read_u8_helper(buf, addr + 1),
                    ),
                    _ => return Err(InstructionError::Unknown(byte).into()),
                },
            },
            0b01 => match byte {
                0x76 => Instruction::Halt,
                _ => {
                    let target_operand = R8::from_operand((byte >> 3) & 0b111);
                    let source_operand = R8::from_operand(byte & 0b111);
                    Instruction::LoadR8R8(target_operand, source_operand)
                }
            },
            0b10 => {
                let source_operand = R8::from_operand(byte & 0x3);
                match byte >> 3 {
                    0x10 => Instruction::ADDR8(source_operand),
                    0x11 => Instruction::ADCR8(source_operand),
                    0x12 => Instruction::SUBR8(source_operand),
                    0x13 => Instruction::SBCR8(source_operand),
                    0x14 => Instruction::ANDR8(source_operand),
                    0x15 => Instruction::XORR8(source_operand),
                    0x16 => Instruction::ORR8(source_operand),
                    0x17 => Instruction::CPR8(source_operand),
                    _ => return Err(InstructionError::Unknown(byte).into()),
                }
            }
            0b11 => match byte {
                0xe0 => Instruction::StoreAA8H(Self::read_u8_helper(buf, addr + 1)),
                0xe2 => Instruction::StoreACH,
                0xea => Instruction::StoreAA16(Self::read_u16_helper(buf, addr + 1)),
                0xf0 => Instruction::LoadAA8H(Self::read_u8_helper(buf, addr + 1)),
                0xf2 => Instruction::LoadACH,
                0xfa => Instruction::LoadAA16(Self::read_u16_helper(buf, addr + 1)),
                0xcb => {
                    let byte = buf[addr as usize + 1];
                    match byte >> 6 {
                        0b01 => Instruction::BIT(
                            BitRef::decode((byte >> 3) & 0x7),
                            R8::from_operand(byte & 0x7),
                        ),
                        0b10 => Instruction::RESET(
                            BitRef::decode((byte >> 3) >> 0x7),
                            R8::from_operand(byte & 0x7),
                        ),
                        0b11 => Instruction::SET(
                            BitRef::decode((byte >> 3) >> 0x7),
                            R8::from_operand(byte & 0x7),
                        ),
                        _ => return Err(InstructionError::Unknown(byte).into()),
                    }
                }
                0xcd => Instruction::CallA16(Self::read_u16_helper(buf, addr + 1)),
                0xc4 => Instruction::CallCondA16(
                    Condition::from_operand(byte >> 3 & 0x3),
                    Self::read_u16_helper(buf, addr + 1),
                ),
                0xc5 => Instruction::PushR16Stack(R16Stack::BC),
                0xd5 => Instruction::PushR16Stack(R16Stack::DE),
                0xe5 => Instruction::PushR16Stack(R16Stack::HL),
                0xf5 => Instruction::PushR16Stack(R16Stack::AF),
                0xc1 => Instruction::PopR16Stack(R16Stack::BC),
                0xd1 => Instruction::PopR16Stack(R16Stack::DE),
                0xe1 => Instruction::PopR16Stack(R16Stack::HL),
                0xf1 => Instruction::PopR16Stack(R16Stack::AF),
                0xc0 => Instruction::RetCond(Condition::NZ),
                0xc8 => Instruction::RetCond(Condition::Z),
                0xc9 => Instruction::Ret,
                0xd0 => Instruction::RetCond(Condition::NC),
                0xd8 => Instruction::RetCond(Condition::C),
                _ => return Err(InstructionError::Unknown(byte).into()),
            },
            _ => unreachable!(),
        };

        let insn_size = result.size();
        Ok((result, insn_size))
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

        assert_eq!(Instruction::size_header(0x1).unwrap(), 3);
        assert_eq!(
            Instruction::from_u8_slice(&[0x1, 0x34, 0x12], 0, 3).unwrap(),
            (Instruction::LoadR16N16(R16::BC, 0x1234), 3)
        );

        assert_eq!(Instruction::size_header(0x2).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x2], 0, 1).unwrap(),
            (Instruction::StoreAR16Mem(R16Mem::BC), 1)
        );

        assert_eq!(Instruction::size_header(0xa).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xa], 0, 1).unwrap(),
            (Instruction::LoadAR16Mem(R16Mem::BC), 1)
        );

        assert_eq!(Instruction::size_header(0x8).unwrap(), 3);
        assert_eq!(
            Instruction::from_u8_slice(&[0x8, 0x34, 0x12], 0, 3).unwrap(),
            (Instruction::StoreSPA16(0x1234), 3)
        );

        assert_eq!(Instruction::size_header(0x3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x3], 0, 1).unwrap(),
            (Instruction::IncR16(R16::BC), 1)
        );

        assert_eq!(Instruction::size_header(0xb).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xb], 0, 1).unwrap(),
            (Instruction::DecR16(R16::BC), 1)
        );

        assert_eq!(Instruction::size_header(0x4).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x4], 0, 1).unwrap(),
            (Instruction::IncR8(R8::B), 1)
        );

        assert_eq!(Instruction::size_header(0x5).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x5], 0, 1).unwrap(),
            (Instruction::DecR8(R8::B), 1)
        );

        assert_eq!(Instruction::size_header(0x6).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0x6, 0x34], 0, 2).unwrap(),
            (Instruction::LoadR8N8(R8::B, 0x34), 2)
        );

        assert_eq!(Instruction::size_header(0x18).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0x18, 0x34], 0, 2).unwrap(),
            (Instruction::JumpNear(0x34), 2)
        );
        assert_eq!(Instruction::size_header(0x38).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0x38, 0x34], 0, 2).unwrap(),
            (Instruction::JumpNearCond(0x34, Condition::C), 2)
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
            (Instruction::LoadR8R8(R8::B, R8::B), 1)
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
            (Instruction::ADDR8(R8::B), 1)
        );

        assert_eq!(Instruction::size_header(0x11 << 3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x11 << 3], 0, 1).unwrap(),
            (Instruction::ADCR8(R8::B), 1)
        );
        assert_eq!(Instruction::size_header(0x12 << 3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x12 << 3], 0, 1).unwrap(),
            (Instruction::SUBR8(R8::B), 1)
        );
        assert_eq!(Instruction::size_header(0x13 << 3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x13 << 3], 0, 1).unwrap(),
            (Instruction::SBCR8(R8::B), 1)
        );
        assert_eq!(Instruction::size_header(0x14 << 3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x14 << 3], 0, 1).unwrap(),
            (Instruction::ANDR8(R8::B), 1)
        );
        assert_eq!(Instruction::size_header(0x15 << 3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x15 << 3], 0, 1).unwrap(),
            (Instruction::XORR8(R8::B), 1)
        );
        assert_eq!(Instruction::size_header(0x16 << 3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x16 << 3], 0, 1).unwrap(),
            (Instruction::ORR8(R8::B), 1)
        );
        assert_eq!(Instruction::size_header(0x17 << 3).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0x17 << 3], 0, 1).unwrap(),
            (Instruction::CPR8(R8::B), 1)
        );
    }

    #[test]
    fn block3() {
        assert_eq!(Instruction::size_header(0xc9).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xc9], 0, 1).unwrap(),
            (Instruction::Ret, 1)
        );
        assert_eq!(Instruction::size_header(0xc0).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xc0], 0, 1).unwrap(),
            (Instruction::RetCond(Condition::NZ), 1)
        );

        assert_eq!(Instruction::size_header(0xcd).unwrap(), 3);
        assert_eq!(
            Instruction::from_u8_slice(&[0xcd, 0x34, 0x12], 0, 3).unwrap(),
            (Instruction::CallA16(0x1234), 3)
        );

        assert_eq!(Instruction::size_header(0xc4).unwrap(), 3);
        assert_eq!(
            Instruction::from_u8_slice(&[0xc4, 0x34, 0x12], 0, 3).unwrap(),
            (Instruction::CallCondA16(Condition::NZ, 0x1234), 3)
        );

        assert_eq!(Instruction::size_header(0xc1).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xc1], 0, 1).unwrap(),
            (Instruction::PopR16Stack(R16Stack::BC), 1)
        );

        assert_eq!(Instruction::size_header(0xc5).unwrap(), 1);
        assert_eq!(
            Instruction::from_u8_slice(&[0xc5], 0, 1).unwrap(),
            (Instruction::PushR16Stack(R16Stack::BC), 1)
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
    }

    #[test]
    fn prefixed() {
        assert_eq!(Instruction::size_header(0xCB).unwrap(), 2);
        assert_eq!(
            Instruction::from_u8_slice(&[0xCB, 0x40], 0, 2).unwrap(),
            (Instruction::BIT(BitRef::B0, R8::B), 2)
        );
        assert_eq!(
            Instruction::from_u8_slice(&[0xCB, 0x80], 0, 2).unwrap(),
            (Instruction::RESET(BitRef::B0, R8::B), 2)
        );
        assert_eq!(
            Instruction::from_u8_slice(&[0xCB, 0xC0], 0, 2).unwrap(),
            (Instruction::SET(BitRef::B0, R8::B), 2)
        );
    }

    // regression tests
    #[test]
    fn decode_bc_store() {
        assert_eq!(Instruction::size_header(0x2).unwrap(), 1);
        let buf = [0x2];
        let (insn, size) = Instruction::from_u8_slice(&buf, 0, 1).unwrap();
        assert_eq!(size, 1);
        assert_eq!(insn, Instruction::StoreAR16Mem(R16Mem::BC));
        let encoded = insn.encode();
        assert_eq!(encoded, buf);
    }

    #[test]
    fn dec_b() {
        assert_eq!(Instruction::size_header(0x5).unwrap(), 1);
        let buf = [0x5];
        let (insn, size) = Instruction::from_u8_slice(&buf, 0, 1).unwrap();
        assert_eq!(size, 1);
        assert_eq!(insn, Instruction::DecR8(R8::B));
        let encoded = insn.encode();
        assert_eq!(encoded, buf);
    }

    #[test]
    fn load_from_bc_ref() {
        assert_eq!(Instruction::size_header(0xa).unwrap(), 1);
        let buf = [0xa];
        let (insn, size) = Instruction::from_u8_slice(&buf, 0, 1).unwrap();
        assert_eq!(size, 1);
        assert_eq!(insn, Instruction::LoadAR16Mem(R16Mem::BC));
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
            (Instruction::XORR8(R8::B), 1)
        );
    }
}
