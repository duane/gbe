use gbc::rom::ROM;

fn main() {
    let rom: ROM = ROM::from_buf(vec![0x0; 0x8000]);
    let machine = gbc::machine::Machine::new(rom);
}
