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
}

struct ArchitectureState {
    pc: u32,
    regs: [u32; 32],
    memory: Memory,
}

impl ArchitectureState {
    fn regwrite(&mut self, addr: u32, val: u32) {
        self.regs[addr as usize] = val;
    }
    fn regread(&self, addr: u32) -> u32 {
        if addr == 0 {
            0
        } else {
            self.regs[addr as usize]
        }
    }
}

#[derive(Debug)]
struct OpInfo {
    operation: Operation,
    dst_regs: Vec<u32>,
    src_regs: Vec<u32>,
    imm: i32,
}

#[derive(Debug)]
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

const FUNCT_LB: u32 = 0b000;
const FUNCT_LH: u32 = 0b001;
const FUNCT_LW: u32 = 0b010;
const FUNCT_LBU: u32 = 0b100;
const FUNCT_LHU: u32 = 0b101;

const FUNCT_ADDI: u32 = 0b000;
const FUNCT_SLTI: u32 = 0b010;
const FUNCT_SLTIU: u32 = 0b011;
const FUNCT_XORI: u32 = 0b100;
const FUNCT_ORI: u32 = 0b110;
const FUNCT_ANDI: u32 = 0b111;

const FUNCT_FENCE: u32 = 0b000;
const FUNCT_FENCEI: u32 = 0b001;

const FUNCT_ECALL: u32 = 0b000;
const FUNCT_CSRRW: u32 = 0b001;
const FUNCT_CSRRS: u32 = 0b010;
const FUNCT_CSRRC: u32 = 0b011;
const FUNCT_CSRRWI: u32 = 0b101;
const FUNCT_CSRRSI: u32 = 0b110;
const FUNCT_CSRRCI: u32 = 0b111;

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
    let opcode = extract_bits(code_word, 0, 7, false);
    let rd = extract_bits(code_word, 7, 5, false);
    let imm = extract_bits(code_word, 12, 20, true);
    let operation = match opcode {
        OPCODE_LUI => Operation::LUI,
        OPCODE_AUIPC => Operation::AUIPC,
        _ => panic!("can't reach here..."),
    };
    return OpInfo {
        operation: operation,
        dst_regs: vec![rd as u32],
        src_regs: vec![],
        imm: imm as i32,
    };
}

fn decode_j_type(code_word: u32) -> OpInfo {
    let opcode = extract_bits(code_word, 0, 7, false);
    assert!(
        opcode == OPCODE_JAL,
        "decode_j_type tries to decode not J codeword (opcode: {})",
        opcode
    );
    let rd = extract_bits(code_word, 7, 5, false);
    let imm = (extract_bits(code_word, 21, 10, false) << 0)
        | (extract_bits(code_word, 20, 1, false) << 10)
        | (extract_bits(code_word, 12, 8, false) << 11)
        | (extract_bits(code_word, 31, 1, false) << 19);
    let imm = extract_bits(imm, 0, 20, true);
    return OpInfo {
        operation: Operation::JAL,
        dst_regs: vec![rd as u32],
        src_regs: vec![],
        imm: imm as i32,
    };
}

fn decode_i_type(code_word: u32) -> OpInfo {
    let opcode = extract_bits(code_word, 0, 7, false);
    let rd = extract_bits(code_word, 7, 5, false);
    let funct = extract_bits(code_word, 12, 3, false);
    let rs1 = extract_bits(code_word, 15, 5, false);
    let imm = extract_bits(code_word, 20, 12, true);

    // (ECALL EBREAK CSRRW CSRRS CSRRC CSRRWI CSRRSI CSRRCI)
    let operation = match (opcode, funct) {
        (OPCODE_JALR, _) => Operation::JALR,
        (OPCODE_LOADS, FUNCT_LB) => Operation::LB,
        (OPCODE_LOADS, FUNCT_LH) => Operation::LH,
        (OPCODE_LOADS, FUNCT_LW) => Operation::LW,
        (OPCODE_LOADS, FUNCT_LBU) => Operation::LBU,
        (OPCODE_LOADS, FUNCT_LHU) => Operation::LHU,
        (OPCODE_IMM, FUNCT_ADDI) => Operation::ADDI,
        (OPCODE_IMM, FUNCT_SLTI) => Operation::SLTI,
        (OPCODE_IMM, FUNCT_SLTIU) => Operation::SLTIU,
        (OPCODE_IMM, FUNCT_XORI) => Operation::XORI,
        (OPCODE_IMM, FUNCT_ORI) => Operation::ORI,
        (OPCODE_IMM, FUNCT_ANDI) => Operation::ANDI,
        (OPCODE_FENCE, FUNCT_FENCE) => Operation::FENCE,
        (OPCODE_FENCE, FUNCT_FENCEI) => Operation::FENCEI,
        (OPCODE_CTRL, FUNCT_ECALL) => match imm {
            0 => Operation::ECALL,
            1 => Operation::EBREAK,
            _ => panic!("invalid ECALL/EBREAK imm!"),
        },
        (OPCODE_CTRL, FUNCT_CSRRW) => Operation::CSRRW,
        (OPCODE_CTRL, FUNCT_CSRRS) => Operation::CSRRS,
        (OPCODE_CTRL, FUNCT_CSRRC) => Operation::CSRRC,
        (OPCODE_CTRL, FUNCT_CSRRWI) => Operation::CSRRWI,
        (OPCODE_CTRL, FUNCT_CSRRSI) => Operation::CSRRSI,
        (OPCODE_CTRL, FUNCT_CSRRCI) => Operation::CSRRCI,
        _ => unimplemented!("hoge"),
    };

    let imm = match operation {
        Operation::SLTIU | Operation::XORI | Operation::ORI | Operation::ANDI => {
            extract_bits(imm, 0, 12, false)
        }
        _ => extract_bits(imm, 0, 12, true),
    };

    return OpInfo {
        operation: operation,
        dst_regs: vec![rd as u32],
        src_regs: vec![rs1 as u32],
        imm: imm as i32,
    };
}

fn decode(code_word: u32) -> OpInfo {
    let opcode = code_word & 0x7f;
    match opcode_to_insttype(opcode) {
        Some(InstType::U) => decode_u_type(code_word),
        Some(InstType::J) => decode_j_type(code_word),
        Some(InstType::I) => decode_i_type(code_word),
        None => panic!("invalid code_word"),
        _ => unimplemented!("decode"),
    }
}

fn fetch(state: &ArchitectureState) -> u32 {
    state.memory.read(state.pc, 4)
}

fn execute(state: &mut ArchitectureState, opinfo: OpInfo) {
    let mut npc = state.pc + 4;
    println!("PC: {}", state.pc);
    println!("{:?}", opinfo);

    match opinfo.operation {
        Operation::JAL => {
            state.regwrite(opinfo.dst_regs[0], npc);
            npc += opinfo.imm as u32;
        }
        Operation::LUI => {
            state.regwrite(opinfo.dst_regs[0], (opinfo.imm << 12) as u32);
        }
        _ => unimplemented!("unimplemented operation"),
    };

    state.pc = npc;
}

fn main() {
    let argc = env::args().count();
    if argc <= 2 {
        eprintln!("too few arguments");
        eprintln!("Usage: cargo run '.../code.hex' memsize");
        std::process::exit(1);
    }
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
    loop {
        let code_word = fetch(&arch_state);
        let opinfo = decode(code_word);
        execute(&mut arch_state, opinfo);
    }
}
