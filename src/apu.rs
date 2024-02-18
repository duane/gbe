use crate::mem_layout::*;
use bitfield_struct::bitfield;
use bitflags::bitflags;

#[bitfield(u8)]
#[derive(PartialEq, Eq, Hash)]
pub struct NR50Register {
    vin_left: bool,
    #[bits(3)]
    volume_left: u8,
    vin_right: bool,
    #[bits(3)]
    volume_right: u8,
}

bitflags! {
    #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct NR51Register: u8 {
        const LEFT_CHANNEL_4 = 0b1000_0000;
        const LEFT_CHANNEL_3 = 0b0100_0000;
        const LEFT_CHANNEL_2 = 0b0010_0000;
        const LEFT_CHANNEL_1 = 0b0001_0000;
        const RIGHT_CHANNEL_4 = 0b0000_1000;
        const RIGHT_CHANNEL_3 = 0b0000_0100;
        const RIGHT_CHANNEL_2 = 0b0000_0010;
        const RIGHT_CHANNEL_1 = 0b0000_0001;
    }
}

bitflags! {
    #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct NR52Register: u8 {
        const ALL_SOUND_ON = 0b1000_0000;
        const CH4_ON = 0b0000_1000;
        const CH3_ON = 0b0000_0100;
        const CH2_ON = 0b0000_0010;
        const CH1_ON = 0b0000_0001;
    }
}

#[derive(Debug, Default, Clone)]
pub struct APU {
    pub nr10: u8,
    pub nr11: u8,
    pub nr12: u8,
    pub nr13: u8,
    pub nr14: u8,
    pub nr21: u8,
    pub nr22: u8,
    pub nr23: u8,
    pub nr24: u8,
    pub nr30: u8,
    pub nr31: u8,
    pub nr32: u8,
    pub nr33: u8,
    pub nr34: u8,
    pub nr41: u8,
    pub nr42: u8,
    pub nr43: u8,
    pub nr44: u8,
    pub nr50: NR50Register,
    pub nr51: NR51Register,
    pub nr52: NR52Register,
    pub wave_ram: [u8; 0x10],
}

impl APU {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn write(&mut self, addr: u16, data: u8) {
        match addr {
            NR10 => self.nr10 = data,
            NR11 => self.nr11 = data,
            NR12 => self.nr12 = data,
            NR13 => self.nr13 = data,
            NR14 => self.nr14 = data,
            NR21 => self.nr21 = data,
            NR22 => self.nr22 = data,
            NR23 => self.nr23 = data,
            NR24 => self.nr24 = data,
            NR30 => self.nr30 = data,
            NR31 => self.nr31 = data,
            NR32 => self.nr32 = data,
            NR33 => self.nr33 = data,
            NR34 => self.nr34 = data,
            NR41 => self.nr41 = data,
            NR42 => self.nr42 = data,
            NR43 => self.nr43 = data,
            NR44 => self.nr44 = data,

            NR50 => self.nr50 = NR50Register::from_bits(data),
            NR51 => self.nr51 = NR51Register::from_bits_truncate(data),
            NR52 => self.nr52 = NR52Register::from_bits_truncate(data),
            WAVE_RAM..=WAVE_RAM_END => self.wave_ram[(addr - WAVE_RAM) as usize] = data,
            _ => panic!("Invalid audio register: {:#04x}", addr),
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            NR10 => self.nr10,
            NR11 => self.nr11,
            NR12 => self.nr12,
            NR13 => self.nr13,
            NR14 => self.nr14,
            NR21 => self.nr21,
            NR22 => self.nr22,
            NR23 => self.nr23,
            NR24 => self.nr24,
            NR30 => self.nr30,
            NR31 => self.nr31,
            NR32 => self.nr32,
            NR33 => self.nr33,
            NR34 => self.nr34,
            NR41 => self.nr41,
            NR42 => self.nr42,
            NR43 => self.nr43,
            NR44 => self.nr44,
            NR50 => self.nr50.into_bits(),
            NR51 => self.nr51.bits(),
            NR52 => self.nr52.bits(),
            WAVE_RAM..=WAVE_RAM_END => self.wave_ram[(addr - WAVE_RAM) as usize],
            _ => panic!("Invalid audio register: {:#04x}", addr),
        }
    }
}
