use crate::mem_layout::*;
use bitfield_struct::bitfield;

pub struct SerialOut {
    sb: u8,
    sc: SCRegister,
}

#[bitfield(u8)]
#[derive(PartialEq, Eq, Hash)]
pub struct SCRegister {
    pub transfer_enable: bool,
    #[bits(5)]
    _unused: u8,
    pub high_clock_speed: bool,
    pub is_master_clock: bool,
}

impl SerialOut {
    pub fn new() -> SerialOut {
        SerialOut {
            sb: 0,
            sc: SCRegister::from_bits(0x0),
        }
    }

    pub fn reset(&mut self) {
        self.sb = 0;
        self.sc = SCRegister::from_bits(0x0);
    }

    pub fn write(&mut self, addr: u16, data: u8) {
        println!("Writing ${:02x} to {}", data, ioreg_name(addr));
        match addr {
            SB => self.sb = data,
            SC => {
                if data == 0x81 {
                    print!("{}", self.sb as char);
                    self.sc = SCRegister::from_bits(0x0);
                } else {
                    self.sc = SCRegister::from_bits(data);
                }
            }
            _ => panic!("Invalid serial write ${:04x}", addr),
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            SB => self.sb,
            SC => self.sc.into_bits(),
            _ => panic!("Invalid serial write ${:04x}", addr),
        }
    }
}
