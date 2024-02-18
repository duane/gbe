// https://gbdev.io/gb-opcodes/optables/
// https://gbdev.io/pandocs/CPU_Instruction_Set.html

use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
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

pub enum Instruction {
    Nop,
    Halt,
    Stop(u8),

    Call(u16),
    JR(i8, Option<Condition>),
    Ret(Option<Condition>),

    Load16Imm(R16, u16),
    Load16Mem(R16Mem),
    Load8Imm(R8, u8),
    Load8C,
    Store8(R16Mem),
    Store8C,
    Store8H(u16),
    StoreSP(u16),
    Load8(R8, R8),
    Push(R16Stack),
    Pop(R16Stack),

    INC16(R16),
    INC8(R8),

    DEC16(R16),
    DEC8(R8),

    ADD(R8),
    ADC(R8),
    SUB(R8),
    SBC(R8),
    AND(R8),
    XOR(R8),
    OR(R8),
    CP(R8),

    BIT(BitRef, R8),
    SET(BitRef, R8),
    RESET(BitRef, R8),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Nop => write!(f, "NOP"),
            Instruction::Halt => write!(f, "HALT"),
            Instruction::Stop(val) => write!(f, "STOP {:#04x}", val),
            Instruction::Load16Imm(target, source) => write!(f, "LD {}, {}", target, source),
            Instruction::Load16Mem(mem) => write!(f, "LD A, {}", mem),
            Instruction::Load8Imm(target, source) => write!(f, "LD {}, {}", target, source),
            Instruction::Load8(target, source) => write!(f, "LD {}, {}", target, source),
            Instruction::Load8C => write!(f, "LD C, [A]"),
            Instruction::Store8C => write!(f, "LD [C], A"),
            Instruction::Store8(dest) => write!(f, "LD [{}], A", dest),
            Instruction::Store8H(addr) => write!(f, "LDH [0x{:04x}], A", addr),
            Instruction::StoreSP(addr) => write!(f, "LD {}, SP", addr),
            Instruction::Push(operand) => write!(f, "PUSH {}", operand),
            Instruction::Pop(operand) => write!(f, "POP {}", operand),
            Instruction::Call(addr) => write!(f, "CALL {:#08x}", addr),
            Instruction::JR(offset, cond) => write!(
                f,
                "JR {}{:+}",
                match cond {
                    Some(cond) => format!("{}, ", cond),
                    None => "".to_string(),
                },
                offset
            ),
            Instruction::Ret(cond) => write!(
                f,
                "RET{}",
                match cond {
                    Some(cond) => format!(" {}, ", cond),
                    None => "".to_string(),
                }
            ),
            Instruction::INC16(r16) => write!(f, "INC {}", r16),
            Instruction::INC8(r8) => write!(f, "INC {}", r8),
            Instruction::DEC16(r16) => write!(f, "DEC {}", r16),
            Instruction::DEC8(r8) => write!(f, "DEC {}", r8),

            Instruction::ADD(source) => write!(f, "ADD A, {}", source),
            Instruction::ADC(source) => write!(f, "ADC A, {}", source),
            Instruction::SUB(source) => write!(f, "SUB A, {}", source),
            Instruction::SBC(source) => write!(f, "SBC A, {}", source),
            Instruction::AND(source) => write!(f, "AND A, {}", source),
            Instruction::XOR(source) => write!(f, "XOR A, {}", source),
            Instruction::OR(source) => write!(f, "OR A, {}", source),
            Instruction::CP(source) => write!(f, "CP A, {}", source),

            Instruction::BIT(b3, r8) => write!(f, "BIT {}, {}", b3, r8),
            Instruction::SET(b3, r8) => write!(f, "SET {}, {}", b3, r8),
            Instruction::RESET(b3, r8) => write!(f, "RES {}, {}", b3, r8),
        }
    }
}

impl Instruction {
    pub fn size_header(insn: u8) -> u8 {
        let size = if insn == 0xCB {
            2
        } else {
            match insn >> 6 {
                0b00 => match insn & 0x0F {
                    0x0 if insn == 0x0 => 1, // NOP
                    0x1 => 3,                // LD r16, imm16
                    0x2 => 1,                // LD [r16mem], A
                    0x3 => 1,                // INC r16
                    0x4 => 1,                // INC r8
                    0x5 => 1,                // DEC r8
                    0x8 if insn == 0x8 => 3, // LD [imm16], SP
                    0xa => 1,                // LD A, [r16mem]
                    0xb => 1,                // DEC r16
                    _ => match insn & 0x7 {
                        0x0 => 2, // JR e8
                        0x6 => 2, // LD r8, imm8
                        _ => unimplemented!("Instruction::size({:#02x})", insn),
                    },
                },
                0b01 => 1,
                0b10 => 1,
                0b11 => match insn {
                    0xe0 => 2,
                    0xcd => 3,
                    0xe2 | 0xf2 => 1,
                    0xf5 | 0xe5 | 0xd5 | 0xc5 => 1,        // push
                    0xc1 | 0xd1 | 0xe1 | 0xf1 => 1,        // pop
                    0xc0 | 0xc8 | 0xd0 | 0xd8 | 0xc9 => 1, // ret
                    _ => unimplemented!("Instruction::size({:#02x})", insn),
                },
                _ => unimplemented!("Instruction::size({:#02x})", insn),
            }
        };
        // println!("size_header({:#02x}) = {}", insn, size);
        size
    }

    pub fn encode(&self) -> Vec<u8> {
        match self {
            Instruction::Nop => vec![0x0],
            Instruction::Halt => vec![0x76],
            Instruction::Stop(val) => vec![0x10, *val],
            Instruction::Call(val) => vec![0xcd, *val as u8, (*val >> 8) as u8],
            Instruction::JR(offset, None) => vec![0x18, *offset as u8],
            Instruction::JR(offset, Some(cond)) => {
                let cond = match cond {
                    Condition::NZ => 0b00,
                    Condition::Z => 0b01,
                    Condition::NC => 0b10,
                    Condition::C => 0b11,
                };
                vec![0x20 | cond << 3, *offset as u8]
            }
            Instruction::Ret(None) => vec![0xc9],
            Instruction::Ret(Some(cond)) => {
                let cond = match cond {
                    Condition::NZ => 0b00,
                    Condition::Z => 0b01,
                    Condition::NC => 0b10,
                    Condition::C => 0b11,
                };
                vec![0xc0 | cond << 3]
            }
            Instruction::Load16Imm(op, val) => {
                vec![0x01 | op.as_operand() << 4, *val as u8, (*val >> 8) as u8]
            }
            Instruction::Load16Mem(mem) => vec![0xb | mem.as_operand() << 4],
            Instruction::Load8Imm(dest, val) => {
                vec![0x06 | dest.as_operand() << 3, *val]
            }
            Instruction::Load8(target, source) => {
                vec![0b01_000_000 | target.as_operand() << 3 | source.as_operand()]
            }
            Instruction::Store8C => vec![0xe3],
            Instruction::Load8C => vec![0xf3],
            Instruction::Store8(dest) => {
                vec![0x3 | dest.as_operand() << 4]
            }
            Instruction::Store8H(addr) => {
                vec![0xe0, (*addr & 0xff) as u8]
            }
            Instruction::StoreSP(imm16) => {
                vec![0x08, *imm16 as u8, (*imm16 >> 8) as u8]
            }
            Instruction::Push(operand) => {
                vec![operand.as_operand() << 4 | 0b11_000_101]
            }
            Instruction::Pop(operand) => {
                vec![operand.as_operand() << 4 | 0b11_000_001]
            }
            Instruction::INC16(r16) => {
                vec![0x3 | r16.as_operand() << 4]
            }
            Instruction::INC8(r8) => {
                vec![0x4 | r8.as_operand() << 3]
            }
            Instruction::DEC16(r16) => {
                vec![0xb | r16.as_operand() << 4]
            }
            Instruction::DEC8(r8) => {
                vec![0x9 | r8.as_operand() << 3]
            }

            Instruction::ADD(source) => vec![0b10_000_000 | source.as_operand()],
            Instruction::ADC(source) => vec![0b10_001_000 | source.as_operand()],
            Instruction::SUB(source) => vec![0b10_010_000 | source.as_operand()],
            Instruction::SBC(source) => vec![0b10_011_000 | source.as_operand()],
            Instruction::AND(source) => vec![0b10_100_000 | source.as_operand()],
            Instruction::XOR(source) => vec![0b10_101_000 | source.as_operand()],
            Instruction::OR(source) => vec![0b10_110_000 | source.as_operand()],
            Instruction::CP(source) => vec![0b10_111_000 | source.as_operand()],
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
            Instruction::Stop(_) => 2,
            Instruction::Call(_) => 3,
            Instruction::JR(_, _) => 2,
            Instruction::Ret(_) => 1,

            Instruction::Load16Mem(_) => 1,
            Instruction::Load16Imm(_, _) => 3,
            Instruction::Load8Imm(_, _) => 2,
            Instruction::Load8(_, _) => 1,
            Instruction::Load8C => 1,
            Instruction::Store8C => 1,
            Instruction::Store8(_) => 1,
            Instruction::Store8H(_) => 2,
            Instruction::StoreSP(_) => 3,
            Instruction::Push(_) => 1,
            Instruction::Pop(_) => 1,

            Instruction::INC16(_) => 1,
            Instruction::INC8(_) => 1,
            Instruction::DEC16(_) => 1,
            Instruction::DEC8(_) => 1,

            Instruction::ADD(_) => 1,
            Instruction::ADC(_) => 1,
            Instruction::SUB(_) => 1,
            Instruction::SBC(_) => 1,
            Instruction::AND(_) => 1,
            Instruction::XOR(_) => 1,
            Instruction::OR(_) => 1,
            Instruction::CP(_) => 1,

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
            Instruction::Stop(_) => (4, 4),
            Instruction::Call(_) => (24, 24),
            Instruction::JR(_, _) => (12, 8),
            Instruction::Ret(None) => (16, 16),
            Instruction::Ret(Some(_)) => (20, 8),

            Instruction::Load16Mem(_) => (8, 8),
            Instruction::Load16Imm(_, _) => (12, 12),
            Instruction::Load8Imm(_, _) => (8, 8),
            Instruction::Load8(R8::HLRef, _) => (8, 8),
            Instruction::Load8(_, R8::HLRef) => (8, 8),
            Instruction::Load8(_, _) => (4, 4),
            Instruction::Load8C => (8, 8),
            Instruction::Store8C => (8, 8),
            Instruction::Store8(_) => (8, 8),
            Instruction::Store8H(_) => (12, 12),
            Instruction::StoreSP(_) => (20, 20),
            Instruction::Push(_) => (16, 16),
            Instruction::Pop(_) => (16, 16),

            Instruction::INC16(_) => (8, 8),
            Instruction::INC8(_) => (4, 4),
            Instruction::DEC16(_) => (8, 8),
            Instruction::DEC8(_) => (4, 4),

            Instruction::ADD(_) => (4, 4),
            Instruction::ADC(_) => (4, 4),
            Instruction::SUB(_) => (4, 4),
            Instruction::SBC(_) => (4, 4),
            Instruction::AND(_) => (4, 4),
            Instruction::XOR(_) => (4, 4),
            Instruction::OR(_) => (4, 4),
            Instruction::CP(_) => (4, 4),

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

    pub fn from_u8_slice(buf: &[u8], addr: u16) -> Self {
        let byte = buf[addr as usize];
        let quad = byte >> 6;
        match quad & 0b11 {
            // load quadrant
            0b00 => match byte & 0x0F {
                0x0 if byte == 0x0 => Instruction::Nop,
                0x1 => Instruction::Load16Imm(
                    R16::from_operand(byte >> 4 & 0xf),
                    Self::read_u16_helper(buf, addr + 1),
                ),
                0x2 => Instruction::Store8(R16Mem::from_operand(byte >> 4 & 0xf)),
                0x3 => Instruction::INC16(R16::from_operand(byte >> 4 & 0x3)),
                0x4 => Instruction::INC8(R8::from_operand(byte >> 3 & 0x7)),
                0x5 => Instruction::DEC8(R8::from_operand(byte >> 3 & 0x7)),
                0x8 if byte == 0x8 => Instruction::StoreSP(Self::read_u16_helper(buf, addr + 1)),
                0xa => Instruction::Load16Mem(R16Mem::from_operand(byte >> 4 & 0xf)),
                0xb => Instruction::DEC16(R16::from_operand(byte >> 4 & 0x3)),
                _ => match byte & 0x7 {
                    0x0 => {
                        if byte & 0b0010_0000 == 0 {
                            Instruction::JR(Self::read_u8_helper(buf, addr + 1) as i8, None)
                        } else {
                            Instruction::JR(
                                Self::read_u8_helper(buf, addr + 1) as i8,
                                Some(Condition::from_operand((byte >> 3) & 0x3)),
                            )
                        }
                    }
                    0x6 => Instruction::Load8Imm(
                        R8::from_operand(byte >> 3),
                        Self::read_u8_helper(buf, addr + 1),
                    ),
                    _ => unimplemented!("Instruction::from_u8({:#04x})", byte),
                },
            },
            0b01 => match byte {
                0x76 => Instruction::Halt,
                _ => {
                    let target_operand = R8::from_operand((byte >> 3) & 0b111);
                    let source_operand = R8::from_operand(byte & 0b111);
                    Instruction::Load8(target_operand, source_operand)
                }
            },
            0b10 => {
                let source_operand = R8::from_operand(byte & 0b111);
                match (byte >> 3) & 0b111 {
                    0b000 => Instruction::ADD(source_operand),
                    0b001 => Instruction::ADC(source_operand),
                    0b010 => Instruction::SUB(source_operand),
                    0b011 => Instruction::SBC(source_operand),
                    0b100 => Instruction::AND(source_operand),
                    0b101 => Instruction::XOR(source_operand),
                    0b110 => Instruction::OR(source_operand),
                    0b111 => Instruction::CP(source_operand),
                    _ => unimplemented!("Instruction::from_u8({:#04x})", byte),
                }
            }
            0b11 => match byte {
                0xe0 => Instruction::Store8H(Self::read_u8_helper(buf, addr + 1) as u16 | 0xFF00),
                0xe2 => Instruction::Store8C,
                0xf2 => Instruction::Load8C,
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
                        _ => unimplemented!("Instruction::from_u8({:#04x})", byte),
                    }
                }
                0xcd => Instruction::Call(Self::read_u16_helper(buf, addr + 1)),
                0xc5 => Instruction::Push(R16Stack::BC),
                0xd5 => Instruction::Push(R16Stack::DE),
                0xe5 => Instruction::Push(R16Stack::HL),
                0xf5 => Instruction::Push(R16Stack::AF),
                0xc1 => Instruction::Pop(R16Stack::BC),
                0xd1 => Instruction::Pop(R16Stack::DE),
                0xe1 => Instruction::Pop(R16Stack::HL),
                0xf1 => Instruction::Pop(R16Stack::AF),
                0xc0 => Instruction::Ret(Some(Condition::NZ)),
                0xc8 => Instruction::Ret(Some(Condition::Z)),
                0xc9 => Instruction::Ret(None),
                0xd0 => Instruction::Ret(Some(Condition::NC)),
                0xd8 => Instruction::Ret(Some(Condition::C)),
                _ => unimplemented!("Instruction::from_u8({:#04x})", byte),
            },
            _ => unreachable!(),
        }
    }
}
