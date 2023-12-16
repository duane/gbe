use std::rc::Rc;
use std::sync::Mutex;

use crate::bus::{self, Bus};
use crate::cpu::CPU;
use crate::rom::ROM;

pub struct Machine {
    pub cpu: CPU,
    bus: Rc<Mutex<Bus>>,
}

impl Machine {
    pub fn new(rom: ROM) -> Self {
        let bus = Rc::new(Mutex::new(bus::Bus::new(rom)));
        Machine {
            cpu: CPU::new(bus.clone()),
            bus,
        }
    }

    pub fn step(&mut self) {
        self.cpu.execute_single_instruction();
    }
}
