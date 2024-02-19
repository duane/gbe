use gbc::instruction::{Instruction, InstructionError, ILLEGAL_INSTRUCTIONS};

fn main() {
    let mut unknown_size: Vec<u8> = vec![];
    let mut unknown_structured: Vec<u8> = vec![];
    let mut unknown_prefixed: Vec<u8> = vec![];
    let total: usize = 256 - 1 - 11 + 256;
    let total_prefixed: usize = 256;

    for instruction in 0..=u8::MAX {
        if gbc::instruction::ILLEGAL_INSTRUCTIONS.contains(&instruction) {
            continue;
        }
        // prefix
        if instruction == 0xCB {
            assert!(
                Instruction::size_header(0xCB).unwrap() == 2,
                "All prefixed instructions are 2 bytes long"
            );
            for prefixed_instruction in 0..=u8::MAX {
                let bytes = vec![0xcb, prefixed_instruction];
                if let Err(InstructionError::Unknown(_)) =
                    Instruction::from_u8_slice(bytes.as_slice(), 0, 2)
                {
                    unknown_prefixed.push(prefixed_instruction);
                }
            }
            continue;
        }
        if let Err(InstructionError::Unknown(_)) = Instruction::size_header(instruction) {
            unknown_size.push(instruction);
            continue;
        }
        let bytes = vec![instruction, 0x0, 0x0];
        if let Err(InstructionError::Unknown(_)) =
            Instruction::from_u8_slice(bytes.as_slice(), 0, 3)
        {
            unknown_structured.push(instruction);
        }
    }
    let total_unknown = unknown_size.len() + unknown_structured.len() + unknown_prefixed.len();

    println!(
        "{}/{} or {}% of all instructions are recognized",
        total - total_unknown,
        total,
        ((total - total_unknown) as f64 / total as f64) * 100.0
    );

    let total_size: usize = 256 - 1 - ILLEGAL_INSTRUCTIONS.len();
    let total_structured: usize = 256 - 1 - 11 - unknown_size.len();
    println!(
        "{}/{} or {}% of all first bytes are recognized",
        total_size - unknown_size.len(),
        total_size,
        ((total_size - unknown_size.len()) as f64 / total_size as f64) * 100.0
    );
    println!(
        "{}/{} or {}% of all first bytes are parsed",
        total_structured - unknown_structured.len(),
        total_structured,
        ((total_structured - unknown_structured.len()) as f64 / total_structured as f64) * 100.0
    );
    println!(
        "{}/{} or {}% of all prefixed instructions are recognized",
        total_prefixed - unknown_prefixed.len(),
        total_prefixed,
        ((total_prefixed - unknown_prefixed.len()) as f64 / total_prefixed as f64) * 100.0
    );
    println!(
        "The following instruction sizes are unknown: {:#02x?}",
        unknown_size
    );
    println!(
        "The following instruction structures are unknown: {:#02x?}",
        unknown_structured
    );
    println!(
        "The following prefixed instructions are unknown: {:#02x?}",
        unknown_prefixed
    );
}
