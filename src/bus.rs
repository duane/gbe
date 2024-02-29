use crate::apu::APU;
use crate::mem_layout::*;
use crate::ppu::PPU;
use crate::rom::ROM;
use crate::serial::SerialOut;

use color_eyre::Result;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum BusError {
    #[error("Invalid Read - {0} - ${1:04x}")]
    InvalidRead(String, u16),
    #[error("Invalid Write - {0} - ${2:02x} to ${1:04x}")]
    InvalidWrite(String, u16, u8),
}

pub struct Bus {
    pub work_ram: [u8; ROM_SIZE + ROMX_SIZE],
    pub rom: ROM,
    pub serial_out: SerialOut,
    pub apu: APU,
    pub external_ram: [u8; SRAM_SIZE],
    pub high_ram: [u8; HRAM_SIZE],
    pub ppu: PPU,
    pub boot_rom_enabled: bool,
}

const BOOT_ROM: &'static [u8; BOOT_ROM_SIZE] = include_bytes!("../assets/boot/dmg.bin");

impl Bus {
    pub fn new(rom: ROM) -> Self {
        Self {
            work_ram: [0; ROM_SIZE + ROMX_SIZE],
            rom,
            serial_out: SerialOut::new(),
            apu: APU::new(),
            external_ram: [0; SRAM_SIZE],
            high_ram: [0; HRAM_SIZE],
            ppu: PPU::new(),
            boot_rom_enabled: true,
        }
    }

    pub fn tick(&mut self, t_cycles: usize) {
        self.ppu.tick(t_cycles);
    }

    pub fn reset(&mut self) {
        self.work_ram = [0; ROM_SIZE + ROMX_SIZE];
        self.serial_out.reset();
        self.apu.reset();
        self.external_ram = [0; SRAM_SIZE];
        self.high_ram = [0; HRAM_SIZE];
        self.ppu.reset();
        self.boot_rom_enabled = true;
    }

    pub fn read_u16(&self, addr: u16) -> Result<u16> {
        let low = self.read_u8(addr)?;
        let high = self.read_u8(addr + 1)?;
        Ok(((high as u16) << 8) | low as u16)
    }

    pub fn read_u8(&self, addr: u16) -> Result<u8> {
        match addr {
            ROM..=BOOT_ROM_END if self.boot_rom_enabled => Ok(BOOT_ROM[addr as usize]),
            ROM..=ROM_END => Ok(self.rom.buf[addr as usize]),
            ROMX..=ROMX_END => Err(BusError::InvalidRead("SWITCH ROM".into(), addr).into()),
            VRAM..=SCRN1_END => Ok(self.ppu.read(addr)),
            SRAM..=SRAM_END => Ok(self.external_ram[addr as usize - 0xA000]),
            RAM..=RAMBANK_END => Ok(self.work_ram[addr as usize - RAM as usize]),
            ECHORAM..=ECHORAM_END => Ok(self.work_ram[addr as usize - ECHORAM as usize]),
            OAMRAM..=OAMRAM_END => Err(BusError::InvalidRead("OAM".into(), addr).into()),
            0xFEA0..=0xFEFF => Err(BusError::InvalidRead("UNUSABLE".into(), addr).into()),
            IO..=IO_END | IE => match addr {
                SB | SC => Ok(self.serial_out.read(addr)),
                NR10..=NR14 | NR21..=NR34 | NR41..=NR52 | WAVE_RAM..=WAVE_RAM_END => {
                    Ok(self.apu.read(addr))
                }
                LCDC | SCY | SCX | LY | LYC | STAT => Ok(self.ppu.read(addr)),
                IE => Err(BusError::InvalidRead("INTERRUPT REGISTER".into(), addr).into()),
                _ => Err(
                    BusError::InvalidRead(format!("IO REGISTER {}", ioreg_name(addr)), addr).into(),
                ),
            },
            HRAM..=HRAM_END => Ok(self.high_ram[addr as usize - HRAM as usize]),
        }
    }

    pub fn write_u16(&mut self, addr: u16, data: u16) -> Result<()> {
        self.write_u8(addr, (data & 0x00FF) as u8)?;
        self.write_u8(addr + 1, (data >> 8) as u8)
    }

    pub fn write_u8(&mut self, addr: u16, data: u8) -> Result<()> {
        match addr {
            ROM..=ROM_END => Err(BusError::InvalidWrite("ROM".into(), addr, data).into()),
            ROMX..=ROMX_END => Err(BusError::InvalidWrite("SWITCH ROM".into(), addr, data).into()),
            VRAM..=SCRN1_END => Ok(self.ppu.write(addr, data)),
            SRAM..=SRAM_END => Ok(self.external_ram[addr as usize - SRAM as usize] = data),
            RAM..=RAMBANK_END => Ok(self.work_ram[addr as usize - RAM as usize] = data),
            ECHORAM..=ECHORAM_END => Ok(self.work_ram[addr as usize - ECHORAM as usize] = data),
            OAMRAM..=OAMRAM_END => Err(BusError::InvalidWrite("OAM".into(), addr, data).into()),
            0xFEA0..=0xFEFF => Err(BusError::InvalidWrite("UNUSABLE".into(), addr, data).into()),
            IO..=IO_END | IE => match addr {
                JOYP => Err(BusError::InvalidWrite("JOYP REGISTER".into(), addr, data).into()),
                SB | SC => Ok(self.serial_out.write(addr, data)),
                DIV => Err(BusError::InvalidWrite("DIVIDER REGISTER".into(), addr, data).into()), // Divider Register
                TIMA => {
                    Err(BusError::InvalidWrite("TIMER COUNTER REGISTER".into(), addr, data).into())
                } // Divider Register
                TIM => {
                    Err(BusError::InvalidWrite("TIMER MODULO REGISTER".into(), addr, data).into())
                } // Divider Register
                TAC => {
                    Err(BusError::InvalidWrite("TIMER CONTROL REGISTER".into(), addr, data).into())
                } // Divider Register
                IF => {
                    Err(BusError::InvalidWrite("INTERRUPT FLAG REGISTER".into(), addr, data).into())
                } // Divider Register

                NR10..=NR14 | NR21..=NR34 | NR41..=NR52 | WAVE_RAM..=WAVE_RAM_END => {
                    Ok(self.apu.write(addr, data))
                }
                LCDC | BGP | SCY | SCX | LYC | STAT => Ok(self.ppu.write(addr, data)),
                IE => Err(BusError::InvalidWrite("INTERRUPT REGISTER".into(), addr, data).into()),

                // undocumented
                BOOT_ROM_ENABLE => Ok(if self.boot_rom_enabled {
                    self.boot_rom_enabled = data != 0;
                }),
                _ => Err(BusError::InvalidWrite(
                    format!("IO REGISTER {}", ioreg_name(addr)),
                    addr,
                    data,
                )
                .into()),
            },
            HRAM..=HRAM_END => Ok(self.high_ram[addr as usize - HRAM as usize] = data),
        }
    }
}
