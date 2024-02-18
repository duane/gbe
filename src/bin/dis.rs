use std::env::args;

use bevy::ecs::system::In;
use gbc::instruction::{Instruction, InstructionError};

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() < 2 {
        println!("USAGE: dis <hex bytes in multiples of 2>");
        return;
    }
    let mut aggregated_hex_buf: Vec<u8> = vec![];
    for arg in args.iter().skip(1) {
        let hex_chars_only = arg.bytes().filter(|c| {
            c >= &b'0' && c <= &b'9' || c >= &b'A' && c <= &b'F' || c >= &b'a' && c <= &b'f'
        });
        aggregated_hex_buf.extend(hex_chars_only);
    }
    assert!(aggregated_hex_buf.len() % 2 == 0, "Invalid hex string");
    let mut bytes: Vec<u8> = vec![];
    for i in 0..(aggregated_hex_buf.len() / 2) {
        let byte = u8::from_str_radix(
            std::str::from_utf8(&aggregated_hex_buf[i * 2..=((i * 2) + 1)]).unwrap(),
            16,
        )
        .unwrap();
        bytes.push(byte);
    }
    println!("hex: {:?}", bytes);

    let mut addr = 0;
    while addr < bytes.len() as u16 {
        let size = match Instruction::size_header(bytes[addr as usize]) {
            Ok(size) => size as usize,
            Err(InstructionError::Unknown(_)) => {
                println!("{:#04x}:\tUnknown({:#02x})", addr, bytes[addr as usize]);
                addr += 1;
                continue;
            }
            Err(InstructionError::Illegal(_)) => {
                println!("{:#04x}:\tIllegal({:#02x})", addr, bytes[addr as usize]);
                addr += 1;
                continue;
            }
            Err(InstructionError::Incomplete(_)) => unreachable!(),
        };
        match Instruction::from_u8_slice(&bytes, addr, size) {
            Ok((insn, size)) => {
                assert!(size == insn.size(), "Size mismatch");
                println!("{:#04x}:\t{}", addr, insn);
                addr += size as u16;
            }
            Err(InstructionError::Unknown(insn)) => {
                println!("{:#04x}:\tUnknown({:#02x})", addr, insn);
                addr += 1;
            }
            Err(InstructionError::Illegal(insn)) => {
                println!("{:#04x}:\tIllegal({:#02x})", addr, insn);
                addr += 1;
            }
            Err(InstructionError::Incomplete(buf)) => {
                println!("{:#04x}:\tIncomplete({:#02x?})", addr, buf);
                addr += buf.len() as u16;
            }
        }
    }
}
