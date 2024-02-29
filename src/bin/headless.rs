use color_eyre::Result;
use gbc::cpu::RegRef;
use gbc::ioreg_addr;
use gbc::rom::ROM;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use signal_hook::consts::SIGINT;
use signal_hook::iterator::Signals;
use std::collections::HashSet;
use std::io::{BufReader, Read, Write};
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use std::thread;
use std::{env::args, fs::File};

const BUF: [u8; 0x8000] = [0x0; 0x8000];

fn parse_addr(addr: &str) -> u16 {
    ioreg_addr(addr).unwrap_or_else(|| {
        u16::from_str_radix(addr, 16).expect(format!("Not a valid address: {}", addr).as_str())
    })
}

macro_rules! on_error {
    ( $on:stmt, $machine:expr, $running:expr, $result:expr ) => {{
        match $result {
            Ok(val) => val,
            Err(err) => {
                println!("{:04X}: {}", $machine.cpu.pc, err);
                $running = false;
                $on
            }
        }
    }};
}

macro_rules! read_u8 {
    ( $on:stmt, $machine:expr, $running:expr, $addr:expr ) => {{
        on_error!($on, $machine, $running, $machine.cpu.bus.read_u8($addr))
    }};
}

macro_rules! read_u16 {
    ( $on:stmt, $machine:expr, $running:expr, $addr:expr ) => {{
        on_error!($on, $machine, $running, $machine.cpu.bus.read_u16($addr))
    }};
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

    let should_stop = Arc::new(Mutex::new(false));
    let should_stop2 = should_stop.clone();

    let mut signals = Signals::new(&[SIGINT])?;
    thread::spawn(move || {
        for sig in signals.forever() {
            // acquire lock
            let mut should_stop_inner = should_stop2.lock().unwrap();

            println!("Received signal {:?}", sig);
            *should_stop_inner = true;
        }
    });

    let mut step_count = 0;
    let check_for_signal_every = 1 << 16;

    'step: loop {
        if !running {
            if *should_stop.lock().unwrap() {
                break 'step;
            }
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
                            continue 'step;
                        }
                        "list" => {
                            let count = 12;
                            let mut addr = match args.next() {
                                Some(addr) => parse_addr(addr),
                                None => machine.cpu.pc,
                            };
                            'listing: for _ in 0..count {
                                let insn = read_u8!(continue 'step, machine, running, addr);
                                let size = match gbc::instruction::Instruction::size_header(insn) {
                                    Ok(size) => size as u16,
                                    Err(_) => {
                                        println!("{:04x}: ???", addr);
                                        addr += 1;
                                        continue 'listing;
                                    }
                                } as u16;
                                let mut buf = Vec::with_capacity(size as usize);
                                for i in 0..size {
                                    let byte = read_u8!(continue 'step, machine, running, addr + i);
                                    buf.push(byte);
                                }
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
                            on_error!(continue 'step, machine, running, machine.step());
                            continue 'step;
                        }
                        "step" => {
                            let steps = args.next().unwrap_or(&"1").parse::<u32>().unwrap();
                            for _ in 0..steps {
                                on_error!(continue 'step, machine, running, machine.step());
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
                        "reset" => machine.reset().unwrap(),
                        "xr" => match args.next() {
                            Some(arg) => {
                                let reg = RegRef::from_str(arg).expect("USAGE: xr REG");
                                println!("{}", machine.cpu.render_reg_val(reg));
                            }
                            None => println!("{}", machine.cpu),
                        },
                        "xb" => {
                            let addr = parse_addr(args.next().expect(&"USAGE: mem ADDR"));
                            let byte = read_u8!(continue 'step, machine, running, addr);
                            println!("${:02x}", byte);
                        }
                        "xw" => {
                            let addr = parse_addr(args.next().expect(&"USAGE: mem ADDR"));
                            let word = read_u16!(continue 'step, machine, running, addr);
                            println!("${:04x}", word);
                        }
                        "dump" => {
                            let start_addr = u16::from_str_radix(
                                args.next().expect("USAGE: dump START END FILE"),
                                16,
                            )
                            .expect("USAGE: dump START END FILE");
                            let end_addr = u16::from_str_radix(
                                args.next().expect("USAGE: dump START END FILE"),
                                16,
                            )
                            .expect("USAGE: dump START END FILE");
                            let filename = args.next().expect("USAGE: dump START END FILE");
                            let mut file = File::create(filename).unwrap();
                            for addr in start_addr..end_addr {
                                let byte = read_u8!(continue 'step, machine, running, addr);
                                file.write_all(&[byte]).unwrap();
                            }
                            file.flush().unwrap();
                        }
                        #[cfg(feature = "headless-render")]
                        "render" => {
                            let filename = args.next().expect("USAGE: render FILE");
                            let mut file = File::create(filename).unwrap();
                            let frame = machine.cpu.bus.ppu.frame();
                            file.write_all(&frame).unwrap();
                            file.flush().unwrap();
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
            step_count += 1;
            if (step_count % check_for_signal_every) == 0 {
                if *should_stop.lock().unwrap() {
                    println!("Pausing execution.");
                    running = false;
                    *should_stop.lock().unwrap() = false;
                    continue 'step;
                }
            }
            on_error!(continue 'step, machine, running, machine.step());
        }
        println!("Encountered breakpoint at ${:04x}", machine.cpu.pc);
        running = false;
    }
    Ok(())
}
