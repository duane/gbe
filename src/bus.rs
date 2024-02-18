use crate::apu::APU;
use crate::ppu::{self, PPU};
use crate::rom::ROM;
use crate::serial::SerialOut;

use color_eyre::Result;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum BusError {
    #[error("Invalid Read - {0} - {1:#04x}")]
    InvalidRead(String, u16),
    #[error("Invalid Write - {0} - {2:#02x} to {1:#04x}")]
    InvalidWrite(String, u16, u8),
}

pub struct Bus {
    pub work_ram: [u8; 0x8_000],
    pub rom: ROM,
    pub serial_out: SerialOut,
    pub apu: APU,
    pub external_ram: [u8; 0x2_000],
    pub ppu: PPU,
}

impl Bus {
    pub fn new(rom: ROM) -> Self {
        Self {
            work_ram: [0; 0x8_000],
            rom,
            serial_out: SerialOut::new(),
            apu: APU::new(),
            external_ram: [0; 0x2_000],
            ppu: PPU::new(),
        }
    }

    pub fn reset(&mut self) {
        self.work_ram = [0; 0x8_000];
    }

    pub fn read_u16(&self, addr: u16) -> Result<u16> {
        let low = self.read_u8(addr)?;
        let high = self.read_u8(addr + 1)?;
        Ok(((high as u16) << 8) | low as u16)
    }

    pub fn read_u8(&self, addr: u16) -> Result<u8> {
        match addr {
            0x0000..=0x3FFF => Ok(self.rom.buf[addr as usize]),
            0x4000..=0x7FFF => Err(BusError::InvalidRead("SWITCH ROM".into(), addr).into()),
            0x8000..=0x97FF => Ok(self.ppu.read(addr)),
            0x9800..=0x9FFF => Ok(self.ppu.read(addr)),
            0xA000..=0xBFFF => Ok(self.external_ram[addr as usize - 0xA000]),
            0xC000..=0xCFFF => Err(BusError::InvalidRead("WORK BANK".into(), addr).into()),
            0xD000..=0xDFFF => Err(BusError::InvalidRead("WORK BANK".into(), addr).into()),
            0xE000..=0xFDFF => Err(BusError::InvalidRead("ECHO BANK".into(), addr).into()),
            0xFE00..=0xFE9F => Err(BusError::InvalidRead("OAM".into(), addr).into()),
            0xFEA0..=0xFEFF => Err(BusError::InvalidRead("UNUSABLE".into(), addr).into()),
            0xFF00..=0xFF7F => match addr {
                0xFF01 => Ok(self.serial_out.get_buffer()),
                0xFF02 => Ok(self.serial_out.get_control()),
                0xFF10..=0xFF14 | 0xFF16..=0xFF1E | 0xFF20..=0xFF26 | 0xFF30..=0xFF3F => {
                    Ok(self.apu.read(addr))
                }
                ppu::LCDC => Ok(self.ppu.lcdc_read()),
                _ => Err(BusError::InvalidRead("IO REGISTER".into(), addr).into()),
            },
            0xFF80..=0xFFFE => Err(BusError::InvalidRead("HIGH RAM".into(), addr).into()),
            0xFFFF => Err(BusError::InvalidRead("INTERRUPT REGISTER".into(), addr).into()),
        }
    }

    pub fn write_u16(&mut self, addr: u16, data: u16) -> Result<()> {
        self.write_u8(addr, (data & 0x00FF) as u8)?;
        self.write_u8(addr + 1, (data >> 8) as u8)
    }

    pub fn write_u8(&mut self, addr: u16, data: u8) -> Result<()> {
        match addr {
            0x0000..=0x3FFF => Err(BusError::InvalidWrite("ROM".into(), addr, data).into()),
            0x4000..=0x7FFF => Err(BusError::InvalidWrite("SWITCH ROM".into(), addr, data).into()),
            0x8000..=0x97FF => Ok(self.ppu.write(addr, data)),
            0x9800..=0x9FFF => Ok(self.ppu.write(addr, data)),
            0xA000..=0xBFFF => Ok(self.external_ram[addr as usize - 0xA000] = data),
            0xC000..=0xCFFF => Err(BusError::InvalidWrite("WORK BANK".into(), addr, data).into()),
            0xD000..=0xDFFF => Err(BusError::InvalidWrite("WORK BANK".into(), addr, data).into()),
            0xE000..=0xFDFF => Err(BusError::InvalidWrite("ECHO BANK".into(), addr, data).into()),
            0xFE00..=0xFE9F => Err(BusError::InvalidWrite("OAM".into(), addr, data).into()),
            0xFEA0..=0xFEFF => Err(BusError::InvalidWrite("UNUSABLE".into(), addr, data).into()),
            0xFF00..=0xFF7F => match addr {
                0xFF01 => Ok(self.serial_out.set_buffer(data)),
                0xFF02 => Ok(self.serial_out.set_control(data)),
                0xFF10..=0xFF14 | 0xFF16..=0xFF1E | 0xFF20..=0xFF26 | 0xFF30..=0xFF3F => {
                    Ok(self.apu.write(addr, data))
                }
                0xFF40 => Ok(self.ppu.lcdc_write(data)),
                _ => Err(BusError::InvalidWrite("IO REGISTER".into(), addr, data).into()),
            },
            0xFF80..=0xFFFE => Err(BusError::InvalidWrite("HIGH RAM".into(), addr, data).into()),
            0xFFFF => Err(BusError::InvalidWrite("INTERRUPT REGISTER".into(), addr, data).into()),
        }
    }
}
