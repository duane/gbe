use gbc::instruction::{Instruction, ILLEGAL_INSTRUCTIONS};
use regex::Regex;

fn sanitize(s: &str) -> String {
    let tgt3re: _ = Regex::new(r"(T\d)").unwrap();
    let bit_refre: _ = Regex::new(r"(B\d)").unwrap();
    let r8re: _ = Regex::new(r"R8(A|B|C|D|E|H|L|HLRef)").unwrap();
    let r16re: _ = Regex::new(r"R16(BC|DE|HL|Sp)").unwrap();
    let r16_stackre: _ = Regex::new(r"R16Stack(DE|HL|BC|AF)").unwrap();
    let r16_memre: _ = Regex::new(r"(R16MemBC|R16MemDE|R16MemHLInc|R16MemHLDec)").unwrap();
    let condre: _ = Regex::new(r"Cond(NZ|Z|NC|C)").unwrap();

    let output_str = r16re.replace_all(s, "R16::R16$1");
    let output_str = r16_memre.replace_all(&output_str, "R16Mem::$1");
    let output_str = r16_stackre.replace_all(&output_str, "R16Stack::R16Stack$1");

    let output_str = r8re.replace_all(&output_str, "R8::R8$1");
    let output_str = bit_refre.replace_all(&output_str, "BitRef::$1");
    let output_str = tgt3re.replace_all(&output_str, "Tgt3::$1");
    let output_str = condre.replace_all(&output_str, "Cond::Cond$1");
    let output_str = output_str.replace("4660", "_");
    let output_str = output_str.replace("52", "_");
    output_str
}

fn main() {
    for insn in 0..=u8::MAX {
        let mut buf: [u8; 3] = [0, 0x34, 0x12];

        if ILLEGAL_INSTRUCTIONS.contains(&insn) {
            //println!("(0,0),");
            continue;
        }
        if insn == 0xCB {
            continue;
        }

        let size = Instruction::size_header(insn).unwrap();
        buf[0] = insn;
        let (structured, _) = Instruction::from_u8_slice(&buf, 0).unwrap();
        let output_str = sanitize(format!("Instruction::{:?}", structured).as_str());
        println!("{} => {:#02x},", output_str, insn);
    }
    println!("");
    println!("");
    for insn in 0..=u8::MAX {
        let buf = [0xCB, insn];
        let (structured, _) = Instruction::from_u8_slice(&buf, 0).unwrap();
        let output_str = sanitize(format!("Instruction::{:?}", structured).as_str());
        println!("{} => {:#02x},", output_str, insn);
        // println!("({},{}),", structured.t_cycles().0, structured.t_cycles().1);
    }
}
