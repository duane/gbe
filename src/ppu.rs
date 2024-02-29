#[cfg(feature = "gfx")]
pub mod screen;
use crate::mem_layout::*;
use bitfield_struct::bitfield;

pub const DOTS_PER_FRAME: usize = 70224;
pub const DOTS_PER_SECOND: usize = 0x400000;

use bitflags::bitflags;

#[derive(Debug, Clone, Copy)]
pub enum Mode {
    HBlank,
    VBlank,
    OAMScan,
    Draw,
}

impl Mode {
    pub fn encoded(&self) -> u8 {
        match self {
            Mode::HBlank => 0,
            Mode::VBlank => 1,
            Mode::OAMScan => 2,
            Mode::Draw => 3,
        }
    }

    pub fn decode(encoded: u8) -> Self {
        match encoded {
            0 => Mode::HBlank,
            1 => Mode::VBlank,
            2 => Mode::OAMScan,
            3 => Mode::Draw,
            _ => panic!("Invalid mode {}", encoded),
        }
    }
}

bitflags! {
    pub struct LCDControl: u8 {
        const ENABLED = 0b1000_0000;
        const WTMAP_IDX = 0b0100_0000;
        const WINDOW_ENABLED = 0b0010_0000;
        const TILE_DATA_IDX = 0b0001_0000;
        const BTMAP_IDX = 0b0000_1000;
        const OBJ_SIZE = 0b0000_0100;
        const OBJS_ENABLED = 0b0000_0010;
        const BW_ENABLED = 0b0000_0001;
    }
}

pub struct PPU {
    pub vram: [u8; SCRN1_END as usize - VRAM as usize + 1],
    pub mode: Mode,
    pub dot_counter: usize,
    pub lcdc: LCDControl,
    pub bgp: BGPRegister,
    pub scy: u8,
    pub scx: u8,
    pub ly: u8,
    pub lyc: u8,
    pub stat: STATRegister,
}

impl PPU {
    pub fn new() -> PPU {
        PPU {
            vram: [0; SCRN1_END as usize - VRAM as usize + 1],
            mode: Mode::OAMScan,
            dot_counter: 0,
            lcdc: LCDControl::empty(),
            bgp: BGPRegister::default(),
            scy: 0,
            scx: 0,
            ly: 0,
            lyc: 0,
            stat: STATRegister::default(),
        }
    }

    pub fn reset(&mut self) {
        self.vram = [0; SCRN1_END as usize - VRAM as usize + 1];
        self.mode = Mode::OAMScan;
        self.dot_counter = 0;
        self.lcdc = LCDControl::empty();
        self.bgp = BGPRegister::default();
        self.scy = 0;
        self.scx = 0;
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            BGP => self.bgp.into_bits(),
            LCDC => self.lcdc.bits(),
            SCY => self.scy,
            SCX => self.scx,
            LY => 0x90,
            LYC => self.lyc,
            STAT => self.stat.into_bits(),
            VRAM..=SCRN1_END => self.vram[addr as usize - VRAM as usize],
            _ => panic!("Invalid PPU read ${:04x}", addr),
        }
    }

    pub fn write(&mut self, addr: u16, data: u8) {
        match addr {
            BGP => self.bgp = BGPRegister::from_bits(data),
            LCDC => self.lcdc = LCDControl::from_bits_truncate(data),
            SCY => self.scy = data,
            SCX => self.scx = data,
            LYC => self.lyc = data,
            STAT => self.stat = STATRegister::from_bits(data & 0xf8 | self.stat.into_bits() & 0x7),
            VRAM..=SCRN1_END => self.vram[addr as usize - VRAM as usize] = data,
            _ => panic!("Invalid PPU read ${:04x}", addr),
        }
    }

    pub fn process_tick(&mut self, m_cycles: u8) {
        assert!(m_cycles <= 6, "Invalid m_cycles {}", m_cycles);
        self.dot_counter += m_cycles as usize;
        if self.dot_counter >= DOTS_PER_FRAME {
            self.dot_counter -= DOTS_PER_FRAME;
        }
    }
}

#[bitfield(u8)]
#[derive(PartialEq, Eq, Hash)]
pub struct BGPRegister {
    #[bits(2)]
    pub id3: usize,
    #[bits(2)]
    pub id2: usize,
    #[bits(2)]
    pub id1: usize,
    #[bits(2)]
    pub id0: usize,
}

#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum PPUMode {
    HBlank = 0,
    VBlank = 1,
    OAMScan = 2,
    Draw = 3,
}

impl PPUMode {
    // This has to be a const fn
    const fn into_bits(self) -> u8 {
        self as _
    }
    const fn from_bits(value: u8) -> Self {
        match value {
            1 => Self::VBlank,
            2 => Self::OAMScan,
            3 => Self::Draw,
            _ => Self::HBlank,
        }
    }
}

#[bitfield(u8)]
#[derive(PartialEq, Eq, Hash)]
pub struct STATRegister {
    #[bits(1)]
    pub _unused: bool,
    #[bits(1)]
    pub lyc_select: bool,
    #[bits(1)]
    pub mode_2: bool,
    #[bits(1)]
    pub mode_1: bool,
    #[bits(1)]
    pub mode_0: bool,
    #[bits(1)]
    pub ly_eq_lyc: bool,
    #[bits(2, default = PPUMode::HBlank, from = PPUMode::from_bits)]
    pub ppu_mode: PPUMode,
}
