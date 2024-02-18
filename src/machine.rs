use std::rc::Rc;
use std::sync::Mutex;

use crate::bus::{self, Bus};
use crate::cpu::CPU;
use crate::rom::ROM;

pub struct Machine {
    pub cpu: CPU,
}

impl Machine {
    pub fn new(rom: ROM) -> Self {
        let bus = Bus::new(rom);
        Machine { cpu: CPU::new(bus) }
    }

    pub fn step(&mut self) {
        self.cpu.execute_single_instruction().unwrap();
    }
}
