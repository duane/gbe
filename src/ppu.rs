#[cfg(feature = "gfx")]
use std::fmt::{Display, Formatter};

use crate::mem_layout::*;
use bitfield_struct::bitfield;

pub const DOTS_PER_FRAME: usize = 70224;
pub const DOTS_PER_SECOND: usize = 0x400000;
pub const FPS: f64 = 59.72750056960583;
pub const UPDATES_PER_SECOND: f64 = 0.016742706298828125;
pub const WIDTH: usize = 160;
pub const HEIGHT: usize = 144;

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
    #[derive(Debug)]
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
    pub frame_count: usize,
    scanline_dot: usize,
    pub lcdc: LCDControl,
    pub bgp: BGPRegister,
    pub scy: u8,
    pub scx: u8,
    pub ly: u8,
    pub lyc: u8,
    lx: u8,
    pub stat: STATRegister,

    pub frame: [u8; WIDTH * HEIGHT * 4],
}

const GRAYSCALE_COLORS: [[u8; 4]; 4] = [
    [255, 255, 255, 255],
    [192, 192, 192, 255],
    [96, 96, 96, 255],
    [0, 0, 0, 255],
];

impl PPU {
    pub fn new() -> PPU {
        PPU {
            vram: [0; SCRN1_END as usize - VRAM as usize + 1],
            mode: Mode::OAMScan,
            dot_counter: 0,
            frame_count: 0,
            scanline_dot: 0,
            lcdc: LCDControl::empty(),
            bgp: BGPRegister::default(),
            scy: 0,
            scx: 0,
            ly: 0,
            lyc: 0,
            lx: 0,
            stat: STATRegister::default(),
            frame: [255; WIDTH * HEIGHT * 4],
        }
    }

    pub fn reset(&mut self) {
        self.vram = [0; SCRN1_END as usize - VRAM as usize + 1];
        self.mode = Mode::OAMScan;
        self.dot_counter = 0;
        self.frame_count = 0;
        self.lcdc = LCDControl::empty();
        self.bgp = BGPRegister::default();
        self.scy = 0;
        self.scx = 0;
        self.lx = 0;
        self.ly = 0;
        self.lyc = 0;
        self.stat = STATRegister::default();
        self.frame = [255; WIDTH * HEIGHT * 4];
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            BGP => self.bgp.into_bits(),
            LCDC => self.lcdc.bits(),
            SCY => self.scy,
            SCX => self.scx,
            LY => self.ly,
            LYC => self.lyc,
            STAT => self.stat.into_bits(),
            VRAM..=SCRN1_END => self.vram[addr as usize - VRAM as usize],
            _ => panic!("Invalid PPU read ${:04x}", addr),
        }
    }

    pub fn write(&mut self, addr: u16, data: u8) {
        if addr > 0xff00 {
            println!("Writing ${:02x} to LCD register {}", data, ioreg_name(addr));
        }
        match addr {
            BGP => self.bgp = BGPRegister::from_bits(data),
            LCDC => {
                self.lcdc = {
                    let lcdc = LCDControl::from_bits_truncate(data);
                    println!("new lcdc: {:?}", lcdc);
                    if !lcdc.contains(LCDControl::ENABLED) {
                        // self.scy = 0;
                        // self.scx = 0;
                        // self.ly = 0;
                        // self.lx = 0;
                        self.dot_counter = 0;
                    }
                    LCDControl::from_bits_truncate(data)
                }
            }
            SCY => {
                println!("wrote ${:02x} to SCY", data);
                self.scy = data;
            }
            SCX => self.scx = data,
            LYC => self.lyc = data,
            STAT => self.stat = STATRegister::from_bits(data & 0xf8 | self.stat.into_bits() & 0x7),
            VRAM..=SCRN1_END => self.vram[addr as usize - VRAM as usize] = data,
            _ => panic!("Invalid PPU read ${:04x}", addr),
        }
    }

    pub fn tick(&mut self, t_cycles: usize) {
        assert!(t_cycles <= 24, "Invalid t_cycles {}", t_cycles);
        let dots = t_cycles / 2;
        if self.lcdc.contains(LCDControl::ENABLED) {
            for _ in 0..dots {
                self.tick_single_dot();
            }
        }
    }

    pub fn tick_single_dot(&mut self) {
        if self.stat.ppu_mode() == PPUMode::OAMScan {
            if self.scanline_dot == 80 {
                // assert!(self.ly < 144);
                self.stat.set_ppu_mode(PPUMode::Draw);
            }
        }

        if self.stat.ppu_mode() == PPUMode::Draw {
            // assert!(self.ly < 144, "Invalid ly {}", self.ly);
            if self.lx as usize == WIDTH {
                self.stat.set_ppu_mode(PPUMode::HBlank);
                self.lx = 0;
            } else {
                self.lx += self.draw();
            }
        }

        if self.stat.ppu_mode() == PPUMode::HBlank || self.stat.ppu_mode() == PPUMode::VBlank {
            if self.scanline_dot == 456 {
                if self.ly >= HEIGHT as u8 {
                    // assert!(self.dot_counter == DOTS_PER_FRAME - 4560);
                    self.stat.set_ppu_mode(PPUMode::VBlank);
                    self.frame_count += 1;
                } else {
                    self.stat.set_ppu_mode(PPUMode::OAMScan);
                }
                self.ly += 1;
                self.scanline_dot = 0;
            } else {
                self.scanline_dot += 1;
            }
        } else if self.scanline_dot == 456 {
            self.scanline_dot = 0;
            self.stat.set_ppu_mode(PPUMode::OAMScan);
            self.ly += 1;
        } else {
            self.scanline_dot += 1;
        }

        if self.stat.ppu_mode() == PPUMode::VBlank && self.dot_counter == DOTS_PER_FRAME {
            println!("blanking");
            self.dot_counter = 0;
            self.ly = 0;
            self.lx = 0;
            self.stat.set_ppu_mode(PPUMode::OAMScan);
        } else {
            self.dot_counter += 1;
        }
    }

    // return number of dots actually drawn
    fn draw(&mut self) -> u8 {
        // assert!(self.ly < 144);
        let x = self.scy.wrapping_add(self.lx);
        let tile_x = x as usize % 8;
        let y = self.scy.wrapping_add(self.ly);
        let tile_y = y as usize % 8;
        let tile_map = if self.lcdc.contains(LCDControl::WTMAP_IDX) {
            0x9c00
        } else {
            0x9800
        } - VRAM;
        let tile_data = if self.lcdc.contains(LCDControl::TILE_DATA_IDX) {
            0x8000
        } else {
            0x8800
        } - VRAM;
        let tile_map_row = (y / 8) as u16;
        let tile_map_col = (x / 8) as u16;
        let tile_map_idx = tile_map + tile_map_row * 32 + tile_map_col;
        let tile_idx = self.vram[tile_map_idx as usize];
        let tile_addr = tile_data + tile_idx as u16 * 16;
        let msb = self.vram[tile_addr as usize + tile_y * 2];
        let lsb = self.vram[tile_addr as usize + tile_y * 2 + 1];
        let bit = 1 << (7 - tile_x);
        let color_num = ((msb & bit) >> (7 - tile_x)) << 1 | ((lsb & bit) >> (7 - tile_x));
        let color = match color_num {
            0b00 => &GRAYSCALE_COLORS[self.bgp.id3()],
            0b01 => &GRAYSCALE_COLORS[self.bgp.id2()],
            0b10 => &GRAYSCALE_COLORS[self.bgp.id1()],
            0b11 => &GRAYSCALE_COLORS[self.bgp.id0()],
            _ => panic!("Invalid color number {}", color_num),
        };
        let start_idx = (self.ly as usize * WIDTH + self.lx as usize) * 4;
        let end_idx = start_idx + 4;
        self.frame[start_idx..end_idx].copy_from_slice(color);
        1
    }

    #[cfg(feature = "headless-render")]
    pub fn frame(&self) -> Vec<u8> {
        use std::io::Cursor;

        use image::{Rgb, RgbImage};

        let mut img = RgbImage::new(WIDTH as u32, HEIGHT as u32);
        for x in 0..WIDTH {
            for y in 0..HEIGHT {
                img.put_pixel(
                    x as u32,
                    y as u32,
                    Rgb([
                        self.frame[(y * WIDTH + x) * 4],
                        self.frame[(y * WIDTH + x) * 4 + 1],
                        self.frame[(y * WIDTH + x) * 4 + 2],
                    ]),
                );
            }
        }
        let mut buf: Vec<u8> = Vec::new();
        img.write_to(&mut Cursor::new(&mut buf), image::ImageOutputFormat::Png)
            .unwrap();
        buf
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
    #[bits(2, default = PPUMode::OAMScan, from = PPUMode::from_bits)]
    pub ppu_mode: PPUMode,
}

impl Display for PPUMode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}
