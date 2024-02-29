use crate::bus::Bus;
use crate::cpu::CPU;
use crate::rom::ROM;
use color_eyre::Result;

pub struct Machine {
    pub cpu: CPU,
}

impl Machine {
    pub fn new(rom: ROM) -> Self {
        let bus = Bus::new(rom);
        Machine { cpu: CPU::new(bus) }
    }

    pub fn step(&mut self) -> Result<()> {
        self.cpu.execute_single_instruction()
    }
}
