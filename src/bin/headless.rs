use color_eyre::Result;
use gbc::ioreg_addr;
use gbc::rom::ROM;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::collections::HashSet;
use std::io::{BufReader, Read};
use std::{env::args, fs::File};

const BUF: [u8; 0x8000] = [0x0; 0x8000];

fn parse_addr(addr: &str) -> u16 {
    ioreg_addr(addr).unwrap_or_else(|| {
        u16::from_str_radix(addr, 16).expect(format!("Not a valid address: {}", addr).as_str())
    })
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let mut rl = DefaultEditor::new()?;

    let args = args().collect::<Vec<String>>();
    if args.len() > 2 {
        println!("Usage: gbc [rom file]");
        return Ok(());
    }
    let rom = if args.len() > 1 {
        let file = File::open(&args[1]);
        if file.is_err() {
            println!("Error opening file: {}", args[1]);
            return Err(file.err().unwrap().into());
        }
        let file = file.unwrap();
        let mut reader = BufReader::new(file);
        let mut rom_file = vec![0x0; 0x8000];
        reader.read(&mut rom_file).unwrap();
        println!("Loaded ROM: {}", args[1]);
        ROM::from_buf(rom_file)
    } else {
        ROM::from_buf(BUF.to_vec())
    };
    let mut machine = gbc::machine::Machine::new(rom);
    let mut breakpoints = HashSet::<u16>::new();

    let mut running = false;
    loop {
        if !running {
            let readline = rl.readline(">> ");
            match readline {
                Ok(owned_str) => {
                    let trimmed = owned_str.trim();
                    if trimmed.is_empty() {
                        continue;
                    }
                    let tokens = trimmed.split_whitespace().collect::<Vec<&str>>();
                    let cmd = tokens[0];
                    let mut args = tokens.iter().skip(1);
                    match cmd {
                        "run" => {
                            running = true;
                            continue;
                        }
                        "list" => {
                            let count = 12;
                            let mut addr = match args.next() {
                                Some(addr) => parse_addr(addr),
                                None => machine.cpu.pc,
                            };
                            for _ in 0..count {
                                let insn = machine.cpu.bus.read_u8(addr)?;
                                let size = gbc::instruction::Instruction::size_header(insn).unwrap()
                                    as u16;
                                let buf: Vec<u8> = (0..size)
                                    .map(|i| machine.cpu.bus.read_u8(addr + i).unwrap())
                                    .collect();
                                let (structured, _) = gbc::instruction::Instruction::from_u8_slice(
                                    &buf,
                                    0,
                                    size as usize,
                                )
                                .unwrap();
                                println!("{:04x}: {}", addr, structured);
                                addr += size;
                            }
                        }
                        "continue" => {
                            running = true;
                            // manually execute the instruction at the breakpoint
                            machine.step();
                            continue;
                        }
                        "step" => {
                            let steps = args.next().unwrap_or(&"1").parse::<u32>().unwrap();
                            for _ in 0..steps {
                                machine.step();
                            }
                        }
                        "break" => {
                            let addr_arg = args.next().expect(&"USAGE: break ADDR");
                            let addr = u16::from_str_radix(&addr_arg, 16).unwrap();
                            breakpoints.insert(addr);
                        }
                        "unbreak" => {
                            let addr_arg = args.next().expect(&"USAGE: unbreak ADDR");
                            let addr = u16::from_str_radix(&addr_arg, 16).unwrap();
                            breakpoints.remove(&addr);
                        }
                        "breaks" => {
                            println!("{:04x?}", breakpoints);
                        }
                        "pc" => {
                            println!("${:04x}", machine.cpu.pc);
                        }
                        "regs" => {
                            // println!("{:?}", machine.cpu.registers);
                        }
                        "xb" => {
                            let addr = parse_addr(args.next().expect(&"USAGE: mem ADDR"));
                            println!("${:02x}", machine.cpu.bus.read_u8(addr)?);
                        }
                        "xw" => {
                            let addr = parse_addr(args.next().expect(&"USAGE: mem ADDR"));
                            println!("${:04x}", machine.cpu.bus.read_u16(addr)?);
                        }
                        "exit" => {
                            break;
                        }
                        _ => {
                            println!("Unknown command: {}", cmd);
                        }
                    }
                }
                Err(ReadlineError::Interrupted) => {
                    break;
                }
                Err(ReadlineError::Eof) => {
                    break;
                }
                Err(err) => {
                    println!("Error: {:?}", err);
                    break;
                }
            }
            continue;
        }

        if machine.cpu.halted {
            println!("Halted.");
            break;
        }

        while !breakpoints.contains(&machine.cpu.pc) {
            machine.step();
        }
        println!("Encountered breakpoint at ${:04x}", machine.cpu.pc);
        running = false;
    }
    Ok(())
}
