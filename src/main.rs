use rom::ROM;

mod bus;
mod cartridge;
mod cpu;
mod instruction;
mod machine;
mod rom;

fn main() {
    let rom: ROM = ROM::from_buf(vec![0x0; 0x8000]);
    let mut machine = machine::Machine::new(rom);
}
