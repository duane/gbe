// https://gbdev.io/gb-opcodes/optables/
// https://gbdev.io/pandocs/CPU_Instruction_Set.html

use std::fmt::Display;

pub enum LoadOperand {
    B,
    C,
    D,
    E,
    H,
    L,
    HLRef, // (HL)
    A,
}

impl Display for LoadOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadOperand::B => write!(f, "B"),
            LoadOperand::C => write!(f, "C"),
            LoadOperand::D => write!(f, "D"),
            LoadOperand::E => write!(f, "E"),
            LoadOperand::H => write!(f, "H"),
            LoadOperand::L => write!(f, "L"),
            LoadOperand::HLRef => write!(f, "(HL)"),
            LoadOperand::A => write!(f, "A"),
        }
    }
}

impl LoadOperand {
    fn as_operand(&self) -> u8 {
        match self {
            LoadOperand::B => 0b000,
            LoadOperand::C => 0b001,
            LoadOperand::D => 0b010,
            LoadOperand::E => 0b011,
            LoadOperand::H => 0b100,
            LoadOperand::L => 0b101,
            LoadOperand::HLRef => 0b110,
            LoadOperand::A => 0b111,
        }
    }

    fn from_operand(operand: u8) -> Self {
        match operand {
            0b000 => LoadOperand::B,
            0b001 => LoadOperand::C,
            0b010 => LoadOperand::D,
            0b011 => LoadOperand::E,
            0b100 => LoadOperand::H,
            0b101 => LoadOperand::L,
            0b110 => LoadOperand::HLRef,
            0b111 => LoadOperand::A,
            _ => unimplemented!("LoadOperand::from_operand({:#04x})", operand),
        }
    }
}

pub enum LoadTarget {
    HL,
    SP,
}

pub enum LoadSource {
    Imm16(u16),
}

pub enum XORSource {
    A,
}

pub enum XORTarget {
    A,
}

pub struct N8 {
    pub val: Option<u8>,
}

pub enum ALUTarget {
    A,
}

impl Display for ALUTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ALUTarget::A => write!(f, "A"),
        }
    }
}

pub enum ALUOperand {
    B,
    C,
    D,
    E,
    H,
    L,
    HLRef, // (HL)
    A,
}

impl Display for ALUOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ALUOperand::B => write!(f, "B"),
            ALUOperand::C => write!(f, "C"),
            ALUOperand::D => write!(f, "D"),
            ALUOperand::E => write!(f, "E"),
            ALUOperand::H => write!(f, "H"),
            ALUOperand::L => write!(f, "L"),
            ALUOperand::HLRef => write!(f, "(HL)"),
            ALUOperand::A => write!(f, "A"),
        }
    }
}

impl ALUOperand {
    fn as_operand(&self) -> u8 {
        match self {
            ALUOperand::B => 0b000,
            ALUOperand::C => 0b001,
            ALUOperand::D => 0b010,
            ALUOperand::E => 0b011,
            ALUOperand::H => 0b100,
            ALUOperand::L => 0b101,
            ALUOperand::HLRef => 0b110,
            ALUOperand::A => 0b111,
        }
    }

    fn from_operand(operand: u8) -> Self {
        match operand {
            0b000 => ALUOperand::B,
            0b001 => ALUOperand::C,
            0b010 => ALUOperand::D,
            0b011 => ALUOperand::E,
            0b100 => ALUOperand::H,
            0b101 => ALUOperand::L,
            0b110 => ALUOperand::HLRef,
            0b111 => ALUOperand::A,
            _ => unimplemented!("ALUOperand::from_operand({:#04x})", operand),
        }
    }
}

pub enum Instruction {
    Nop,
    Stop(u8),
    Load16(LoadTarget, LoadSource),
    Load8(LoadOperand, LoadOperand),

    ADD(ALUTarget, ALUOperand),
    ADC(ALUTarget, ALUOperand),
    SUB(ALUTarget, ALUOperand),
    SBC(ALUTarget, ALUOperand),
    AND(ALUTarget, ALUOperand),
    XOR(ALUTarget, ALUOperand),
    OR(ALUTarget, ALUOperand),
    CP(ALUTarget, ALUOperand),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Nop => write!(f, "NOP"),
            Instruction::Stop(val) => write!(f, "STOP {:#04x}", val),
            Instruction::Load16(target, source) => match (target, source) {
                (LoadTarget::HL, LoadSource::Imm16(val)) => write!(f, "LD HL, {:#06x}", val),
                (LoadTarget::SP, LoadSource::Imm16(val)) => write!(f, "LD SP, {:#06x}", val),
            },
            Instruction::Load8(target, source) => write!(f, "LD {}, {}", target, source),

            Instruction::ADD(target, source) => write!(f, "ADD {}, {}", target, source),
            Instruction::ADC(target, source) => write!(f, "ADC {}, {}", target, source),
            Instruction::SUB(target, source) => write!(f, "SUB {}, {}", target, source),
            Instruction::SBC(target, source) => write!(f, "SBC {}, {}", target, source),
            Instruction::AND(target, source) => write!(f, "AND {}, {}", target, source),
            Instruction::XOR(target, source) => write!(f, "XOR {}, {}", target, source),
            Instruction::OR(target, source) => write!(f, "OR {}, {}", target, source),
            Instruction::CP(target, source) => write!(f, "CP {}, {}", target, source),
        }
    }
}

impl Instruction {
    pub fn size_header(insn: u8) -> u8 {
        match insn >> 6 {
            0b00 => match insn {
                0 => 1,
                0x10 => 2,
                0x20 => 2,
                0x30 => 2,
                0x11 => 3,
                0x21 => 3,
                0x31 => 3,
                _ => unimplemented!("Instruction::size({:#02x})", insn),
            },
            0b01 => 1,
            0b10 => 1,
            _ => unimplemented!("Instruction::size({:#02x})", insn),
        }
    }

    pub fn encode(&self) -> Vec<u8> {
        match self {
            Instruction::Nop => vec![0x0],
            Instruction::Stop(val) => vec![0x10, *val],
            Instruction::Load16(LoadTarget::HL, LoadSource::Imm16(val)) => {
                vec![0x21, *val as u8, (*val >> 8) as u8]
            }
            Instruction::Load16(LoadTarget::SP, LoadSource::Imm16(val)) => {
                vec![0x31, *val as u8, (*val >> 8) as u8]
            }
            Instruction::Load8(target, source) => {
                vec![0b01_000_000 | target.as_operand() << 3 | source.as_operand()]
            }

            Instruction::ADD(ALUTarget::A, source) => vec![0b10_000_000 | source.as_operand()],
            Instruction::ADC(ALUTarget::A, source) => vec![0b10_001_000 | source.as_operand()],
            Instruction::SUB(ALUTarget::A, source) => vec![0b10_010_000 | source.as_operand()],
            Instruction::SBC(ALUTarget::A, source) => vec![0b10_011_000 | source.as_operand()],
            Instruction::AND(ALUTarget::A, source) => vec![0b10_100_000 | source.as_operand()],
            Instruction::XOR(ALUTarget::A, source) => vec![0b10_101_000 | source.as_operand()],
            Instruction::OR(ALUTarget::A, source) => vec![0b10_110_000 | source.as_operand()],
            Instruction::CP(ALUTarget::A, source) => vec![0b10_111_000 | source.as_operand()],
        }
    }

    pub fn size(&self) -> u8 {
        match self {
            Instruction::Nop => 1,
            Instruction::Stop(_) => 2,
            Instruction::Load16(LoadTarget::HL, LoadSource::Imm16(_)) => 3,
            Instruction::Load16(LoadTarget::SP, LoadSource::Imm16(_)) => 3,
            Instruction::Load8(_, _) => 1,

            Instruction::ADD(_, _) => 1,
            Instruction::ADC(_, _) => 1,
            Instruction::SUB(_, _) => 1,
            Instruction::SBC(_, _) => 1,
            Instruction::AND(_, _) => 1,
            Instruction::XOR(_, _) => 1,
            Instruction::OR(_, _) => 1,
            Instruction::CP(_, _) => 1,
        }
    }

    pub fn m_cycles(&self) -> u8 {
        match self {
            Instruction::Nop => 1,
            Instruction::Stop(_) => 1,
            Instruction::Load16(_, _) => 7,
            Instruction::Load8(_, _) => 1,

            Instruction::ADD(_, _) => 1,
            Instruction::ADC(_, _) => 1,
            Instruction::SUB(_, _) => 1,
            Instruction::SBC(_, _) => 1,
            Instruction::AND(_, _) => 1,
            Instruction::XOR(_, _) => 1,
            Instruction::OR(_, _) => 1,
            Instruction::CP(_, _) => 1,
        }
    }

    pub fn t_cycles(&self) -> u8 {
        match self {
            Instruction::Nop => 4,
            Instruction::Stop(_) => 4,
            Instruction::Load16(LoadTarget::HL, LoadSource::Imm16(_)) => 12,
            Instruction::Load16(LoadTarget::SP, LoadSource::Imm16(_)) => 12,
            Instruction::Load8(_, _) => 4,

            Instruction::ADD(_, _) => 4,
            Instruction::ADC(_, _) => 4,
            Instruction::SUB(_, _) => 4,
            Instruction::SBC(_, _) => 4,
            Instruction::AND(_, _) => 4,
            Instruction::XOR(_, _) => 4,
            Instruction::OR(_, _) => 4,
            Instruction::CP(_, _) => 4,
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
            0b00 => match byte {
                0 => Instruction::Nop,
                0x10 => Instruction::Stop(Self::read_u8_helper(buf, addr + 1)),
                0x21 => Instruction::Load16(
                    LoadTarget::HL,
                    LoadSource::Imm16(Self::read_u16_helper(buf, addr + 1)),
                ),
                0x31 => Instruction::Load16(
                    LoadTarget::SP,
                    LoadSource::Imm16(Self::read_u16_helper(buf, addr + 1)),
                ),
                _ => unimplemented!("Instruction::from_u8({:#04x})", byte),
            },
            0b01 => {
                let target_operand = LoadOperand::from_operand((byte >> 3) & 0b111);
                let source_operand = LoadOperand::from_operand(byte & 0b111);
                Instruction::Load8(target_operand, source_operand)
            }
            0b10 => {
                let source_operand = ALUOperand::from_operand(byte & 0b111);
                match (byte >> 3) & 0b111 {
                    0b000 => Instruction::ADD(ALUTarget::A, source_operand),
                    0b001 => Instruction::ADC(ALUTarget::A, source_operand),
                    0b010 => Instruction::SUB(ALUTarget::A, source_operand),
                    0b011 => Instruction::SBC(ALUTarget::A, source_operand),
                    0b100 => Instruction::AND(ALUTarget::A, source_operand),
                    0b101 => Instruction::XOR(ALUTarget::A, source_operand),
                    0b110 => Instruction::OR(ALUTarget::A, source_operand),
                    0b111 => Instruction::CP(ALUTarget::A, source_operand),
                    _ => unimplemented!("Instruction::from_u8({:#04x})", byte),
                }
            }
            0b11 => {
                unimplemented!("Instruction::from_u8({:#04x})", byte);
            }
            _ => unimplemented!("Instruction::from_u8({:#04x})", byte),
        }
    }
}
