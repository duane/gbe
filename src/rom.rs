use std::fmt::{Display, Formatter};

pub struct ROM {
    pub buf: Vec<u8>,
}

impl ROM {
    pub fn from_buf(buf: Vec<u8>) -> Self {
        Self { buf }
    }

    pub fn mbc(&self) -> MBCHeader {
        self.buf[0x147].into()
    }
}

impl From<u8> for MBCHeader {
    fn from(value: u8) -> Self {
        match value {
            0x00 => MBCHeader::ROMOnly,
            0x01 => MBCHeader::MBC1,
            0x02 => MBCHeader::MBC1RAM,
            0x03 => MBCHeader::MBC1RAMBattery,
            0x05 => MBCHeader::MBC2,
            0x06 => MBCHeader::MBC2Battery,
            0x08 => MBCHeader::ROMRAM,
            0x09 => MBCHeader::ROMRAMBattery,
            0x0B => MBCHeader::MMM01,
            0x0C => MBCHeader::MMM01RAM,
            0x0D => MBCHeader::MMM01RAMBattery,
            0x0F => MBCHeader::MBC3TimerBattery,
            0x10 => MBCHeader::MBC3TimerRAMBattery,
            0x11 => MBCHeader::MBC3,
            0x12 => MBCHeader::MBC3RAM,
            0x13 => MBCHeader::MBC3RAMBattery,
            0x15 => MBCHeader::MBC4,
            0x16 => MBCHeader::MBC4RAM,
            0x17 => MBCHeader::MBC4RAMBattery,
            0x19 => MBCHeader::MBC5,
            0x1A => MBCHeader::MBC5RAM,
            0x1B => MBCHeader::MBC5RAMBattery,
            0x1C => MBCHeader::MBC5Rumble,
            0x1D => MBCHeader::MBC5RumbleRAM,
            0x1E => MBCHeader::MBC5RumbleRAMBattery,
            0xFC => MBCHeader::PocketCamera,
            0xFD => MBCHeader::BandaiTAMA5,
            0xFE => MBCHeader::HuC3,
            0xFF => MBCHeader::HuC1RAMBattery,
            _ => panic!("Invalid MBC header: {:#04x}", value),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum MBCHeader {
    ROMOnly = 0x00,
    MBC1 = 0x01,
    MBC1RAM = 0x02,
    MBC1RAMBattery = 0x03,
    MBC2 = 0x05,
    MBC2Battery = 0x06,
    ROMRAM = 0x08,
    ROMRAMBattery = 0x09,
    MMM01 = 0x0B,
    MMM01RAM = 0x0C,
    MMM01RAMBattery = 0x0D,
    MBC3TimerBattery = 0x0F,
    MBC3TimerRAMBattery = 0x10,
    MBC3 = 0x11,
    MBC3RAM = 0x12,
    MBC3RAMBattery = 0x13,
    MBC4 = 0x15,
    MBC4RAM = 0x16,
    MBC4RAMBattery = 0x17,
    MBC5 = 0x19,
    MBC5RAM = 0x1A,
    MBC5RAMBattery = 0x1B,
    MBC5Rumble = 0x1C,
    MBC5RumbleRAM = 0x1D,
    MBC5RumbleRAMBattery = 0x1E,
    PocketCamera = 0xFC,
    BandaiTAMA5 = 0xFD,
    HuC3 = 0xFE,
    HuC1RAMBattery = 0xFF,
}

impl Display for MBCHeader {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MBCHeader::ROMOnly => write!(f, "ROM Only"),
            MBCHeader::MBC1 => write!(f, "MBC1"),
            MBCHeader::MBC1RAM => write!(f, "MBC1 + RAM"),
            MBCHeader::MBC1RAMBattery => write!(f, "MBC1 + RAM + Battery"),
            MBCHeader::MBC2 => write!(f, "MBC2"),
            MBCHeader::MBC2Battery => write!(f, "MBC2 + Battery"),
            MBCHeader::ROMRAM => write!(f, "ROM + RAM"),
            MBCHeader::ROMRAMBattery => write!(f, "ROM + RAM + Battery"),
            MBCHeader::MMM01 => write!(f, "MMM01"),
            MBCHeader::MMM01RAM => write!(f, "MMM01 + RAM"),
            MBCHeader::MMM01RAMBattery => write!(f, "MMM01 + RAM + Battery"),
            MBCHeader::MBC3TimerBattery => write!(f, "MBC3 + Timer + Battery"),
            MBCHeader::MBC3TimerRAMBattery => write!(f, "MBC3 + Timer + RAM + Battery"),
            MBCHeader::MBC3 => write!(f, "MBC3"),
            MBCHeader::MBC3RAM => write!(f, "MBC3 + RAM"),
            MBCHeader::MBC3RAMBattery => write!(f, "MBC3 + RAM + Battery"),
            MBCHeader::MBC4 => write!(f, "MBC4"),
            MBCHeader::MBC4RAM => write!(f, "MBC4 + RAM"),
            MBCHeader::MBC4RAMBattery => write!(f, "MBC4 + RAM + Battery"),
            MBCHeader::MBC5 => write!(f, "MBC5"),
            MBCHeader::MBC5RAM => write!(f, "MBC5 + RAM"),
            MBCHeader::MBC5RAMBattery => write!(f, "MBC5 + RAM + Battery"),
            MBCHeader::MBC5Rumble => write!(f, "MBC5 + Rumble"),
            MBCHeader::MBC5RumbleRAM => write!(f, "MBC5 + Rumble + RAM"),
            MBCHeader::MBC5RumbleRAMBattery => write!(f, "MBC5 + Rumble + RAM + Battery"),
            MBCHeader::PocketCamera => write!(f, "Pocket Camera"),
            MBCHeader::BandaiTAMA5 => write!(f, "Bandai TAMA5"),
            MBCHeader::HuC3 => write!(f, "HuC3"),
            MBCHeader::HuC1RAMBattery => write!(f, "HuC1 + RAM + Battery"),
        }
    }
}
