// https://gbdev.io/gb-opcodes/optables/
// https://gbdev.io/pandocs/CPU_Instruction_Set.html

use std::fmt::Display;

pub enum R16Mem {
    BC,
    DE,
    HLInc,
    HLDec,
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

pub enum LoadTarget16 {
    HL,
    SP,
}

impl Display for LoadTarget16 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadTarget16::HL => write!(f, "HL"),
            LoadTarget16::SP => write!(f, "SP"),
        }
    }
}

pub enum LoadTarget8 {
    A,
}

impl Display for LoadTarget8 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadTarget8::A => write!(f, "A"),
        }
    }
}

pub enum LoadSource {
    Imm8(u8),
    Imm16(u16),
}

impl Display for LoadSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadSource::Imm8(val) => write!(f, "{:#04x}", val),
            LoadSource::Imm16(val) => write!(f, "{:#08x}", val),
        }
    }
}

pub enum StoreSource {
    A,
}

impl Display for StoreSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StoreSource::A => write!(f, "A"),
        }
    }
}

pub enum StackOperand {
    BC,
    DE,
    HL,
    AF,
}

impl StackOperand {
    fn as_operand(&self) -> u8 {
        match self {
            StackOperand::BC => 0b00,
            StackOperand::DE => 0b01,
            StackOperand::HL => 0b10,
            StackOperand::AF => 0b11,
        }
    }
}

impl Display for StackOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackOperand::BC => write!(f, "BC"),
            StackOperand::DE => write!(f, "DE"),
            StackOperand::HL => write!(f, "HL"),
            StackOperand::AF => write!(f, "AF"),
        }
    }
}

pub enum Conditions {
    NZ,
    Z,
    NC,
    C,
}

impl Display for Conditions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Conditions::NZ => write!(f, "NZ"),
            Conditions::Z => write!(f, "Z"),
            Conditions::NC => write!(f, "NC"),
            Conditions::C => write!(f, "C"),
        }
    }
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
    Halt,
    Stop(u8),

    Call(u16),
    JR(i8, Option<Conditions>),
    Ret(Option<Conditions>),

    Load16Imm(LoadTarget16, u16),
    Load16Mem(R16Mem),
    Load8Imm(LoadTarget8, u8),
    Store8H(u16, StoreSource),
    Load8(LoadOperand, LoadOperand),
    Push(StackOperand),
    Pop(StackOperand),

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
            Instruction::Halt => write!(f, "HALT"),
            Instruction::Stop(val) => write!(f, "STOP {:#04x}", val),
            Instruction::Load16Imm(target, source) => write!(f, "LD {}, {}", target, source),
            Instruction::Load16Mem(mem) => write!(f, "LD A, {}", mem),
            Instruction::Load8Imm(target, source) => write!(f, "LD {}, {}", target, source),
            Instruction::Load8(target, source) => write!(f, "LD {}, {}", target, source),
            Instruction::Store8H(addr, source) => write!(f, "LD [0x{:04x}], {}", addr, source),
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
                0x0a | 0x1a | 0x2a | 0x3a => 1,
                0x18 | 0x20 | 0x28 | 0x30 | 0x38 => 2, // jr
                0x11 => 3,
                0x21 => 3,
                0x31 => 3,
                0x3e => 2,
                _ => unimplemented!("Instruction::size({:#02x})", insn),
            },
            0b01 => 1,
            0b10 => 1,
            0b11 => match insn {
                0xe0 => 2,
                0xcd => 3,
                0xf5 | 0xe5 | 0xd5 | 0xc5 => 1,        // push
                0xc1 | 0xd1 | 0xe1 | 0xf1 => 1,        // pop
                0xc0 | 0xc8 | 0xd0 | 0xd8 | 0xc9 => 1, // ret
                _ => unimplemented!("Instruction::size({:#02x})", insn),
            },
            _ => unimplemented!("Instruction::size({:#02x})", insn),
        }
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
                    Conditions::NZ => 0b00,
                    Conditions::Z => 0b01,
                    Conditions::NC => 0b10,
                    Conditions::C => 0b11,
                };
                vec![0x20 | cond << 3, *offset as u8]
            }
            Instruction::Ret(None) => vec![0xc9],
            Instruction::Ret(Some(cond)) => {
                let cond = match cond {
                    Conditions::NZ => 0b00,
                    Conditions::Z => 0b01,
                    Conditions::NC => 0b10,
                    Conditions::C => 0b11,
                };
                vec![0xc0 | cond << 3]
            }

            Instruction::Load16Imm(LoadTarget16::HL, val) => {
                vec![0x21, *val as u8, (*val >> 8) as u8]
            }
            Instruction::Load16Mem(mem) => match mem {
                R16Mem::BC => vec![0x0a],
                R16Mem::DE => vec![0x1a],
                R16Mem::HLInc => vec![0x2a],
                R16Mem::HLDec => vec![0x3a],
            },
            Instruction::Load16Imm(LoadTarget16::SP, val) => {
                vec![0x31, *val as u8, (*val >> 8) as u8]
            }
            Instruction::Load8Imm(LoadTarget8::A, val) => {
                vec![0x3e, *val]
            }
            Instruction::Load8(target, source) => {
                vec![0b01_000_000 | target.as_operand() << 3 | source.as_operand()]
            }
            Instruction::Store8H(addr, StoreSource::A) => {
                vec![0xe0, (*addr & 0xff) as u8]
            }
            Instruction::Push(operand) => {
                vec![operand.as_operand() << 4 | 0b11_000_101]
            }
            Instruction::Pop(operand) => {
                vec![operand.as_operand() << 4 | 0b11_000_001]
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
            Instruction::Halt => 1,
            Instruction::Stop(_) => 2,
            Instruction::Call(_) => 3,
            Instruction::JR(_, _) => 2,
            Instruction::Ret(_) => 1,

            Instruction::Load16Mem(_) => 1,
            Instruction::Load16Imm(_, _) => 3,
            Instruction::Load8Imm(_, _) => 2,
            Instruction::Load8(_, _) => 1,
            Instruction::Store8H(_, _) => 2,
            Instruction::Push(_) => 1,
            Instruction::Pop(_) => 1,

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
            Instruction::Halt => 1,
            Instruction::Stop(_) => 1,
            Instruction::Call(_) => 7,
            Instruction::JR(_, _) => 4,
            Instruction::Ret(_) => 1,

            Instruction::Load16Mem(_) => 7,
            Instruction::Load16Imm(_, _) => 7,
            Instruction::Load8Imm(_, _) => 4,
            Instruction::Load8(_, _) => 1,
            Instruction::Store8H(_, _) => 4,
            Instruction::Push(_) => 1,
            Instruction::Pop(_) => 1,

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
            Instruction::Halt => 4,
            Instruction::Stop(_) => 4,
            Instruction::Call(_) => 24,
            Instruction::JR(_, _) => 12,
            Instruction::Ret(_) => 16,

            Instruction::Load16Mem(_) => 8,
            Instruction::Load16Imm(_, _) => 12,
            Instruction::Load8Imm(_, _) => 8,
            Instruction::Load8(_, _) => 4,
            Instruction::Store8H(_, _) => 12,
            Instruction::Push(_) => 16,
            Instruction::Pop(_) => 16,

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
                0x0a => Instruction::Load16Mem(R16Mem::BC),
                0x10 => Instruction::Stop(Self::read_u8_helper(buf, addr + 1)),
                0x18 => Instruction::JR(Self::read_u8_helper(buf, addr + 1) as i8, None),
                0x1a => Instruction::Load16Mem(R16Mem::DE),
                0x20 => Instruction::JR(
                    Self::read_u8_helper(buf, addr + 1) as i8,
                    Some(Conditions::NZ),
                ),
                0x28 => Instruction::JR(
                    Self::read_u8_helper(buf, addr + 1) as i8,
                    Some(Conditions::Z),
                ),
                0x2a => Instruction::Load16Mem(R16Mem::HLInc),
                0x30 => Instruction::JR(
                    Self::read_u8_helper(buf, addr + 1) as i8,
                    Some(Conditions::NC),
                ),
                0x38 => Instruction::JR(
                    Self::read_u8_helper(buf, addr + 1) as i8,
                    Some(Conditions::C),
                ),
                0x3a => Instruction::Load16Mem(R16Mem::HLDec),
                0x21 => {
                    Instruction::Load16Imm(LoadTarget16::HL, Self::read_u16_helper(buf, addr + 1))
                }
                0x31 => {
                    Instruction::Load16Imm(LoadTarget16::SP, Self::read_u16_helper(buf, addr + 1))
                }
                0x3e => Instruction::Load8Imm(LoadTarget8::A, Self::read_u8_helper(buf, addr + 1)),
                _ => unimplemented!("Instruction::from_u8({:#04x})", byte),
            },
            0b01 => match byte {
                0x76 => Instruction::Halt,
                _ => {
                    let target_operand = LoadOperand::from_operand((byte >> 3) & 0b111);
                    let source_operand = LoadOperand::from_operand(byte & 0b111);
                    Instruction::Load8(target_operand, source_operand)
                }
            },
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
            0b11 => match byte {
                0xe0 => Instruction::Store8H(
                    Self::read_u8_helper(buf, addr + 1) as u16 | 0xFF00,
                    StoreSource::A,
                ),
                0xcd => Instruction::Call(Self::read_u16_helper(buf, addr + 1)),
                0xc5 => Instruction::Push(StackOperand::BC),
                0xd5 => Instruction::Push(StackOperand::DE),
                0xe5 => Instruction::Push(StackOperand::HL),
                0xf5 => Instruction::Push(StackOperand::AF),
                0xc1 => Instruction::Pop(StackOperand::BC),
                0xd1 => Instruction::Pop(StackOperand::DE),
                0xe1 => Instruction::Pop(StackOperand::HL),
                0xf1 => Instruction::Pop(StackOperand::AF),
                0xc0 => Instruction::Ret(Some(Conditions::NZ)),
                0xc8 => Instruction::Ret(Some(Conditions::Z)),
                0xc9 => Instruction::Ret(None),
                0xd0 => Instruction::Ret(Some(Conditions::NC)),
                0xd8 => Instruction::Ret(Some(Conditions::C)),
                _ => unimplemented!("Instruction::from_u8({:#04x})", byte),
            },
            _ => unreachable!(),
        }
    }
}
