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

    pub fn step_frame(&mut self) -> Result<()> {
        let cur_frame = self.cpu.bus.ppu.frame_count;
        while self.cpu.bus.ppu.frame_count == cur_frame {
            self.step()?;
        }
        Ok(())
    }

    pub fn reset(&mut self) -> Result<()> {
        self.cpu.reset();
        Ok(())
    }
}
