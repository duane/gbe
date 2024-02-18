#[cfg(feature = "gfx")]
pub mod screen;

pub const DOTS_PER_FRAME: usize = 70224;
pub const DOTS_PER_SECOND: usize = 0x400000;
pub const LCDC: u16 = 0xFF40;

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
    pub vram: [u8; 0x2000],
    pub mode: Mode,
    pub dot_counter: usize,
    pub lcdc: LCDControl,
}

impl PPU {
    pub fn new() -> PPU {
        PPU {
            vram: [0; 0x2000],
            mode: Mode::OAMScan,
            dot_counter: 0,
            lcdc: LCDControl::empty(),
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        assert!(addr >= 0x8000 && addr <= 0x9FFF, "PPU READ - {:#04x}", addr);
        self.vram[addr as usize - 0x8000]
    }

    pub fn write(&mut self, addr: u16, data: u8) {
        assert!(
            addr >= 0x8000 && addr <= 0x9FFF,
            "PPU WRITE - {:#02x} to {:#04x}",
            data,
            addr
        );
        self.vram[addr as usize - 0x8000] = data;
    }

    pub fn lcdc_write(&mut self, data: u8) {
        self.lcdc = LCDControl::from_bits_retain(data);
    }

    pub fn lcdc_read(&self) -> u8 {
        self.lcdc.bits()
    }

    pub fn process_tick(&mut self, m_cycles: u8) {
        assert!(m_cycles <= 6, "Invalid m_cycles {}", m_cycles);
        self.dot_counter += m_cycles as usize;
        if self.dot_counter >= DOTS_PER_FRAME {
            self.dot_counter -= DOTS_PER_FRAME;
        }
    }
}
