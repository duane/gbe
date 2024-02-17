use gbc::rom::ROM;
use std::io::{BufReader, Read};
use std::{env::args, fs::File};

const BUF: [u8; 0x8000] = [0x0; 0x8000];

fn main() {
    let args = args().collect::<Vec<String>>();
    if args.len() > 2 {
        println!("Usage: gbc <rom file>");
        return;
    }
    let rom = if args.len() > 1 {
        let file = File::open(&args[1]);
        if file.is_err() {
            println!("Error opening file: {}", args[1]);
            return;
        }
        let file = file.unwrap();
        let mut reader = BufReader::new(file);
        let mut rom_file = vec![0x0; 0x8000];
        reader.read(&mut rom_file).unwrap();
        ROM::from_buf(rom_file)
    } else {
        ROM::from_buf(BUF.to_vec())
    };
    let mut machine = gbc::machine::Machine::new(rom);
    while !machine.cpu.halted {
        machine.step();
    }
}
