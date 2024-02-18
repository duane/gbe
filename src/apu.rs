use bitfield_struct::bitfield;
use bitflags::bitflags;

const NR10: u16 = 0xFF10; // Channel 1 sweep
const NR11: u16 = 0xFF11; // Channel 1 length timer & duty cycle
const NR12: u16 = 0xFF12; // Channel 1 volume & envelope
const NR13: u16 = 0xFF13; // Channel 1 period low [write-only]
const NR14: u16 = 0xFF14; // Channel 1 period high & control
const NR21: u16 = 0xFF16; // Channel 2 sweep
const NR22: u16 = 0xFF17; // Channel 2 length timer & duty cycle
const NR23: u16 = 0xFF18; // Channel 2 volume & envelope
const NR24: u16 = 0xFF19; // Channel 2 period low [write-only]
const NR30: u16 = 0xFF1a; // Channel 3 DAC enable
const NR31: u16 = 0xFF1b; // Channel 3 length timer [write-only]
const NR32: u16 = 0xFF1c; // Channel 3 output level
const NR33: u16 = 0xFF1d; // Channel 3 period low [write-only]
const NR34: u16 = 0xFF1e; // Channel 3 period high & control
const NR41: u16 = 0xFF20; // Channel 4 length timer [write-only]
const NR42: u16 = 0xFF21; // Channel 4 volume & envelope
const NR43: u16 = 0xFF22; // Channel 4 frequency & randomness
const NR44: u16 = 0xFF23; // Channel 4 control
const NR50: u16 = 0xFF24; // Master volume & VIN panning
const NR51: u16 = 0xFF25; // Sound panning
const NR52: u16 = 0xFF26; // Audio Master Control

#[bitfield(u8)]
#[derive(PartialEq, Eq, Hash)]
pub struct RegisterNR50 {
    vin_left: bool,
    #[bits(3)]
    volume_left: u8,
    vin_right: bool,
    #[bits(3)]
    volume_right: u8,
}

bitflags! {
    #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct RegisterNR51: u8 {
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
    pub struct RegisterNR52: u8 {
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
    pub nr50: RegisterNR50,
    pub nr51: RegisterNR51,
    pub nr52: RegisterNR52,
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

            NR50 => self.nr50 = RegisterNR50::from_bits(data),
            NR51 => self.nr51 = RegisterNR51::from_bits_truncate(data),
            NR52 => self.nr52 = RegisterNR52::from_bits_truncate(data),
            0xFF30..=0xFF3F => self.wave_ram[(addr - 0xFF30) as usize] = data,
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
            0xFF30..=0xFF3F => self.wave_ram[(addr - 0xFF30) as usize],
            _ => panic!("Invalid audio register: {:#04x}", addr),
        }
    }
}
