use std::env;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn extract_bits(val: u32, begin: i32, len: i32, sext: bool) -> u32 {
    let mask = if len >= 32 { 0 } else { !0 << len };
    let result = (val >> begin) & !mask;
    if sext && ((result & (1 << (len - 1))) != 0) {
        return result | mask;
    } else {
        return result;
    }
}

#[test]
fn extract_bits_test() {
    assert_eq!(extract_bits(1, 0, 1, true), 0xffffffff);
    assert_eq!(extract_bits(1, 0, 1, false), 1);
    assert_eq!(extract_bits(8, 2, 3, false), 2);
    assert_eq!(extract_bits(8, 2, 3, true), 2);
    assert_eq!(extract_bits(15, 0, 4, true), 0xffffffff);
    assert_eq!(extract_bits(15, 0, 4, false), 15);
}

struct Memory {
    array: Vec<u32>,
    size: usize,
}

impl Memory {
    fn new(size: usize, stream: &mut BufRead) -> Memory {
        let mut mem = Memory {
            array: Vec::with_capacity(size),
            size: size,
        };

        let mut buffer = String::new();
        loop {
            match stream.read_line(&mut buffer) {
                Ok(0) => break, // EOF
                Ok(_) => {
                    let x = u32::from_str_radix(&buffer.trim_right(), 16);
                    match x {
                        Ok(x) => {
                            mem.array.push(x);
                        }
                        Err(e) => {
                            eprintln!("{} is {}", buffer.trim_right(), e);
                            panic!("read_memh error: failed to parse line");
                        }
                    }
                    buffer.clear();
                }
                Err(e) => {
                    eprintln!("{}", e);
                    panic!("read_memh error: failed to read file");
                }
            }
        }
        return mem;
    }

    fn write(&mut self, addr: u32, value: u32, size: i32) {
        match size {
            1 | 2 => unimplemented!("MemWrite"),
            4 => self.array[(addr >> 2) as usize] = value,
            _ => panic!("invalid size"),
        }
    }

    fn read(&self, addr: u32, size: i32) -> u32 {
        match size {
            1 | 2 => unimplemented!("MemRead"),
            4 => self.array[(addr >> 2) as usize],
            _ => panic!("invalid size Memory::read"),
        }
    }

    fn read_memh(&mut self, stream: &mut BufRead) {}
}

struct ArchitectureState {
    pc: u32,
    regs: [u32; 32],
    memory: Memory,
}

struct OpInfo {
    operation: Operation,
    dst_regs: Vec<i32>,
    src_regs: Vec<i32>,
    imm: i32,
}

enum Operation {
    LUI,
    AUIPC,
    JAL,
    JALR,
    BEQ,
    BNE,
    BLT,
    BGE,
    BLTU,
    BGEU,
    LB,
    LH,
    LW,
    LBU,
    LHU,
    SB,
    SH,
    SW,
    ADDI,
    SLTI,
    SLTIU,
    XORI,
    ORI,
    ANDI,
    SLLI,
    SRLI,
    SRAI,
    ADD,
    SUB,
    SLL,
    SLT,
    SLTU,
    XOR,
    SRL,
    SRA,
    OR,
    AND,
    FENCE,
    FENCEI,
    ECALL,
    EBREAK,
    CSRRW,
    CSRRS,
    CSRRC,
    CSRRWI,
    CSRRSI,
    CSRRCI,
    NOP,
    UNDEFINED, // for debug
}

enum InstType {
    R,
    I,
    S,
    B,
    U,
    J,
}

const OPCODE_LUI: u32 = 0b0110111;
const OPCODE_AUIPC: u32 = 0b0010111;
const OPCODE_JAL: u32 = 0b1101111;
const OPCODE_JALR: u32 = 0b1100111;
const OPCODE_LOADS: u32 = 0b0000011;
const OPCODE_IMM: u32 = 0b0010011;
const OPCODE_FENCE: u32 = 0b0001111;
const OPCODE_CTRL: u32 = 0b1110011;
const OPCODE_BRANCH: u32 = 0b1100011;
const OPCODE_STORES: u32 = 0b0100011;
const OPCODE_REGS: u32 = 0b0110011;

fn opcode_to_insttype(opcode: u32) -> Option<InstType> {
    match opcode {
        OPCODE_LUI | OPCODE_AUIPC => Some(InstType::U), // LUI AUIPC
        OPCODE_JAL => Some(InstType::J),                // JAL
        OPCODE_JALR | OPCODE_LOADS | OPCODE_IMM | OPCODE_FENCE | OPCODE_CTRL => Some(InstType::I), // JALR (LB LH LW LBU LHU) (ADDI SLTI SLTIU XORI ORI ANDI) (FENCE FENCE.I) (ECALL EBREAK CSRRW CSRRS CSRRC CSRRWI CSRRSI CSRRCI)
        OPCODE_BRANCH => Some(InstType::B), // (BEQ, BNE, BLT, BGE, BLTU, BGEU)
        OPCODE_STORES => Some(InstType::S), // (SB SH SW)
        OPCODE_REGS => Some(InstType::R),   // (ADD SUB SLL SLT SLTU XOR SRL SRA OR AND)
        _ => None,
    }
}

fn decode_u_type(code_word: u32) -> OpInfo {
    let opcode = code_word & 0x7f;
    let rd = (code_word >> 7) & 0x1f;
    let imm = code_word >> 12;
    let operation = match opcode {
        OPCODE_LUI => Operation::LUI,
        OPCODE_AUIPC => Operation::AUIPC,
        _ => panic!("can't reach here..."),
    };
    return OpInfo {
        operation: operation,
        dst_regs: vec![rd as i32],
        src_regs: vec![],
        imm: imm as i32,
    };
}

fn decode_j_type(code_word: u32) -> OpInfo {
    let opcode = code_word & 0x7f;
    assert!(
        opcode == OPCODE_JAL,
        "decode_j_type tries to decode not J codeword (opcode: {})",
        opcode
    );
    let rd = (code_word >> 7) & 0x1f;
    let imm = (extract_bits(code_word, 21, 10, false) << 0)
        | (extract_bits(code_word, 20, 1, false) << 10)
        | (extract_bits(code_word, 12, 8, false) << 11)
        | (extract_bits(code_word, 31, 1, false) << 19);
    return OpInfo {
        operation: Operation::JAL,
        dst_regs: vec![rd as i32],
        src_regs: vec![],
        imm: imm as i32,
    };
}

fn decode(code_word: u32) -> OpInfo {
    let opcode = code_word & 0x7f;
    match opcode_to_insttype(opcode) {
        Some(InstType::U) => decode_u_type(code_word),
        Some(InstType::J) => decode_j_type(code_word),
        None => panic!("invalid code_word"),
        _ => unimplemented!("decode"),
    }
}

fn execute(state: &mut ArchitectureState, opinfo: OpInfo) {}

fn main() {
    let argc = env::args().count();
    if argc <= 2 {
        eprintln!("too few arguments");
        eprintln!("Usage: cargo run '.../code.hex' memsize");
        std::process::exit(1);
    }
    let program: String = env::args().next().unwrap();
    let args: Vec<String> = env::args().skip(1).collect();
    let code = &args[0];
    let size: usize = args[1].parse().unwrap();

    let mut arch_state = ArchitectureState {
        pc: 0x8000,
        regs: [0; 32],
        memory: match File::open(code) {
            Ok(file) => {
                let mut buf_file = BufReader::new(file);
                Memory::new(size, &mut buf_file)
            }
            Err(e) => {
                eprintln!("{}: {}", code, e);
                panic!("initialize error")
            }
        },
    };
    println!("{}", program);
    println!("{:?}", args);
    println!("Hello, world! {}", arch_state.pc);
}
