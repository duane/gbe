use std::rc::Rc;

use crate::rom::ROM;

pub struct Bus {
    pub work_ram: [u8; 0x8_000],
    pub rom: ROM,
}

impl Bus {
    pub fn new(rom: ROM) -> Self {
        Self {
            work_ram: [0; 0x8_000],
            rom,
        }
    }

    pub fn reset(&mut self) {
        self.work_ram = [0; 0x8_000];
    }

    pub fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x3FFF => self.rom.buf[addr as usize],
            0x4000..=0x7FFF => {
                unimplemented!("SWITCH ROM READ - {:#04x}", addr);
            }
            0x8000..=0x9FFF => {
                unimplemented!("VRAM READ - {:#04x}", addr);
            }
            0xA000..=0xBFFF => {
                unimplemented!("EXTERNAL RAM READ - {:#04x}", addr);
            }
            0xC000..=0xCFFF => {
                unimplemented!("WORK BANK READ - {:#04x}", addr);
            }
            0xD000..=0xDFFF => {
                unimplemented!("WORK BANK READ - {:#04x}", addr);
            }
            0xE000..=0xFDFF => {
                unimplemented!("ECHO BANK READ - {:#04x}", addr);
            }
            0xFE00..=0xFE9F => {
                unimplemented!("OBJECT ATTRIBUTE MEMORY READ - {:#04x}", addr);
            }
            0xFEA0..=0xFEFF => {
                unimplemented!("UNUSABLE READ - {:#04x}", addr);
            }
            0xFF00..=0xFF7F => {
                unimplemented!("IO REGISTER READ - {:#04x}", addr);
            }
            0xFF80..=0xFFFE => {
                unimplemented!("HIGH RAM READ - {:#04x}", addr);
            }
            0xFFFF => unimplemented!("INTERRUPT REGISTER READ - {:#04x}", addr),
        }
    }

    pub fn write_u8(&mut self, addr: u16, data: u8) {
        match addr {
            0x0000..=0x3FFF => {
                unimplemented!("ROM WRITE - {:#04x}", addr);
            }
            0x4000..=0x7FFF => {
                unimplemented!("SWITCH ROM WRITE - {:#04x}", addr);
            }
            0x8000..=0x9FFF => {
                unimplemented!("VRAM WRITE - {:#04x}", addr);
            }
            0xA000..=0xBFFF => {
                unimplemented!("EXTERNAL RAM WRITE - {:#04x}", addr);
            }
            0xC000..=0xCFFF => {
                unimplemented!("WORK BANK WRITE - {:#04x}", addr);
            }
            0xD000..=0xDFFF => {
                unimplemented!("WORK BANK WRITE - {:#04x}", addr);
            }
            0xE000..=0xFDFF => {
                unimplemented!("ECHO BANK WRITE - {:#04x}", addr);
            }
            0xFE00..=0xFE9F => {
                unimplemented!("OBJECT ATTRIBUTE MEMORY WRITE - {:#04x}", addr);
            }
            0xFEA0..=0xFEFF => {
                unimplemented!("UNUSABLE WRITE - {:#04x}", addr);
            }
            0xFF00..=0xFF7F => {
                unimplemented!("IO REGISTER WRITE - {:#04x}", addr);
            }
            0xFF80..=0xFFFE => {
                unimplemented!("HIGH RAM WRITE - {:#04x}", addr);
            }
            0xFFFF => unimplemented!("INTERRUPT REGISTER WRITE - {:#04x}", addr),
        }
    }
}
