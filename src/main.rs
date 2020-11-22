use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

static mut DEBUG_PRINT: bool = false;
static mut TIME: u32 = 0;

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
}

impl Memory {
    fn new(size: usize, stream: &mut dyn BufRead) -> Memory {
        let mut mem = Memory {
            array: Vec::with_capacity(size),
        };

        let mut buffer = String::new();
        let mut count = 0;
        loop {
            match stream.read_line(&mut buffer) {
                Ok(0) => break, // EOF
                Ok(_) => {
                    let x = u32::from_str_radix(&buffer.trim_end(), 16);
                    match x {
                        Ok(x) => {
                            mem.array.push(x);
                            count += 4;
                        }
                        Err(e) => {
                            eprintln!("{} is {}", buffer.trim_end(), e);
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
        while count <= size {
            mem.array.push(0);
            count += 4;
        }
        return mem;
    }

    fn write(&mut self, addr: u32, value: u32, size: i32) {
        if addr == 0xf6ff_f070 {
            print!("{}", (value & 0xff) as u8 as char);
            return;
        }

        let index = (addr >> 2) as usize;
        let pos = (addr % 4) * 8;
        let mask = match size {
            1 => !(0xff << pos),
            2 => !(0xffff << pos),
            4 => 0,
            _ => panic!("invalid size"),
        };
        let memv = self.array[index] & mask;
        self.array[index] = memv | (value << pos);
    }

    fn read(&self, addr: u32, size: i32) -> u32 {
        if addr == 0xffffff00 {
            unsafe {
                return TIME;
            }
        }

        let index = (addr >> 2) as usize;
        let pos = (addr % 4) * 8;
        let mask = match size {
            1 => 0xff << pos,
            2 => 0xffff << pos,
            4 => 0xffffffff,
            _ => panic!("invalid size"),
        };
        let memv = (self.array[index] & mask) >> pos;
        return memv;
    }
}

struct ArchitectureState {
    pc: u32,
    regs: [u32; 32],
    memory: Memory,
}

impl ArchitectureState {
    fn regwrite(&mut self, addr: u32, val: u32) {
        unsafe {
            if DEBUG_PRINT && addr != 0 {
                //eprintln!("reg[{}]: {} -> {}", addr, self.regs[addr as usize], val);
            }
        }
        self.regs[addr as usize] = val;
    }
    fn regread(&self, addr: u32) -> u32 {
        if addr == 0 {
            0
        } else {
            unsafe {
                if DEBUG_PRINT {
                    //eprintln!("reg[{}]: {}", addr, self.regs[addr as usize]);
                }
            }
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
    code_word: u32,
}

#[derive(Debug, PartialEq)]
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

const FUNCT_SLLI: u32 = 0b001;
const FUNCT_SRXI: u32 = 0b101;

const FUNCT_FENCE: u32 = 0b000;
const FUNCT_FENCEI: u32 = 0b001;

const FUNCT_ECALL: u32 = 0b000;
const FUNCT_CSRRW: u32 = 0b001;
const FUNCT_CSRRS: u32 = 0b010;
const FUNCT_CSRRC: u32 = 0b011;
const FUNCT_CSRRWI: u32 = 0b101;
const FUNCT_CSRRSI: u32 = 0b110;
const FUNCT_CSRRCI: u32 = 0b111;

const FUNCT_SB: u32 = 0b000;
const FUNCT_SH: u32 = 0b001;
const FUNCT_SW: u32 = 0b010;

const FUNCT_ADD: u32 = 0b0000000_000;
const FUNCT_SUB: u32 = 0b0100000_000;
const FUNCT_SLL: u32 = 0b0000000_001;
const FUNCT_SRL: u32 = 0b0000000_101;
const FUNCT_SRA: u32 = 0b0100000_101;
const FUNCT_SLT: u32 = 0b0000000_010;
const FUNCT_SLTU: u32 = 0b0000000_011;
const FUNCT_XOR: u32 = 0b0000000_100;
const FUNCT_OR: u32 = 0b0000000_110;
const FUNCT_AND: u32 = 0b0000000_111;

const FUNCT_BEQ: u32 = 0b000;
const FUNCT_BNE: u32 = 0b001;
const FUNCT_BLT: u32 = 0b100;
const FUNCT_BGE: u32 = 0b101;
const FUNCT_BLTU: u32 = 0b110;
const FUNCT_BGEU: u32 = 0b111;

fn opcode_to_insttype(opcode: u32) -> Option<InstType> {
    match opcode {
        OPCODE_LUI | OPCODE_AUIPC => Some(InstType::U), // LUI AUIPC
        OPCODE_JAL => Some(InstType::J),                // JAL
        OPCODE_JALR | OPCODE_LOADS | OPCODE_IMM | OPCODE_FENCE | OPCODE_CTRL => Some(InstType::I), // JALR (LB LH LW LBU LHU) (ADDI SLTI SLTIU XORI ORI ANDI SLLI SRLI SRAI) (FENCE FENCE.I) (ECALL EBREAK CSRRW CSRRS CSRRC CSRRWI CSRRSI CSRRCI)
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
        code_word: code_word,
    };
}

fn decode_j_type(code_word: u32) -> OpInfo {
    let opcode = extract_bits(code_word, 0, 7, false);
    assert!(
        opcode == OPCODE_JAL,
        "decode_j_type tries to decode not J code_word (opcode: {})",
        opcode
    );
    let rd = extract_bits(code_word, 7, 5, false);
    let imm = (extract_bits(code_word, 21, 10, false) << 0)
        | (extract_bits(code_word, 20, 1, false) << 10)
        | (extract_bits(code_word, 12, 8, false) << 11)
        | (extract_bits(code_word, 31, 1, false) << 19);
    let imm = extract_bits(imm, 0, 20, true) << 1;
    return OpInfo {
        operation: Operation::JAL,
        dst_regs: vec![rd as u32],
        src_regs: vec![],
        imm: imm as i32,
        code_word: code_word,
    };
}

fn decode_i_type(code_word: u32) -> OpInfo {
    let opcode = extract_bits(code_word, 0, 7, false);
    let rd = extract_bits(code_word, 7, 5, false);
    let funct = extract_bits(code_word, 12, 3, false);
    let rs1 = extract_bits(code_word, 15, 5, false);
    let imm = extract_bits(code_word, 20, 12, true);

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
        (OPCODE_IMM, FUNCT_SLLI) => Operation::SLLI,
        (OPCODE_IMM, FUNCT_SRXI) => match imm >> 5 {
            0 => Operation::SRLI,
            _ => Operation::SRAI,
        },
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

    let imm = if operation == Operation::SRAI {
        extract_bits(imm, 0, 12, true) & 0x1f
    } else {
        extract_bits(imm, 0, 12, true)
    };

    return OpInfo {
        operation: operation,
        dst_regs: vec![rd as u32],
        src_regs: vec![rs1 as u32],
        imm: imm as i32,
        code_word: code_word,
    };
}

fn decode_s_type(code_word: u32) -> OpInfo {
    let opcode = extract_bits(code_word, 0, 7, false);
    assert_eq!(opcode, OPCODE_STORES);
    let funct = extract_bits(code_word, 12, 3, false);
    let rs1 = extract_bits(code_word, 15, 5, false);
    let rs2 = extract_bits(code_word, 20, 5, false);
    let imm = extract_bits(code_word, 25, 7, true) << 5 | extract_bits(code_word, 7, 5, false);
    let operation = match funct {
        FUNCT_SB => Operation::SB,
        FUNCT_SH => Operation::SH,
        FUNCT_SW => Operation::SW,
        _ => panic!("invalid S-Type funct!"),
    };

    return OpInfo {
        operation: operation,
        dst_regs: vec![],
        src_regs: vec![rs1 as u32, rs2 as u32],
        imm: imm as i32,
        code_word: code_word,
    };
}

fn decode_b_type(code_word: u32) -> OpInfo {
    let opcode = extract_bits(code_word, 0, 7, false);
    assert_eq!(opcode, OPCODE_BRANCH);
    let funct = extract_bits(code_word, 12, 3, false);
    let rs1 = extract_bits(code_word, 15, 5, false);
    let rs2 = extract_bits(code_word, 20, 5, false);

    let imm = (extract_bits(code_word, 8, 4, false) << 0)
        | (extract_bits(code_word, 25, 6, false) << 4)
        | (extract_bits(code_word, 7, 1, false) << 10)
        | (extract_bits(code_word, 31, 1, false) << 11);
    let imm = extract_bits(imm, 0, 12, true) << 1;

    let operation = match funct {
        FUNCT_BEQ => Operation::BEQ,
        FUNCT_BNE => Operation::BNE,
        FUNCT_BLT => Operation::BLT,
        FUNCT_BGE => Operation::BGE,
        FUNCT_BLTU => Operation::BLTU,
        FUNCT_BGEU => Operation::BGEU,
        _ => panic!("invalid B-Type funct!"),
    };

    return OpInfo {
        operation: operation,
        dst_regs: vec![],
        src_regs: vec![rs1 as u32, rs2 as u32],
        imm: imm as i32,
        code_word: code_word,
    };
}

fn decode_r_type(code_word: u32) -> OpInfo {
    let opcode = extract_bits(code_word, 0, 7, false);
    assert_eq!(opcode, OPCODE_REGS);
    let rd = extract_bits(code_word, 7, 5, false);
    let funct3 = extract_bits(code_word, 12, 3, false);
    let rs1 = extract_bits(code_word, 15, 5, false);
    let rs2 = extract_bits(code_word, 20, 5, false);
    let funct7 = extract_bits(code_word, 25, 7, false);
    let funct = (funct7 << 3) | funct3;
    let operation = match funct {
        FUNCT_ADD => Operation::ADD,
        FUNCT_SUB => Operation::SUB,
        FUNCT_SLL => Operation::SLL,
        FUNCT_SRL => Operation::SRL,
        FUNCT_SRA => Operation::SRA,
        FUNCT_SLT => Operation::SLT,
        FUNCT_SLTU => Operation::SLTU,
        FUNCT_XOR => Operation::XOR,
        FUNCT_OR => Operation::OR,
        FUNCT_AND => Operation::AND,
        _ => panic!("invalid R-Type funct!"),
    };

    return OpInfo {
        operation: operation,
        dst_regs: vec![rd as u32],
        src_regs: vec![rs1 as u32, rs2 as u32],
        imm: 0,
        code_word: code_word,
    };
}

fn decode(code_word: u32) -> Option<OpInfo> {
    let opcode = code_word & 0x7f;
    if opcode == 0 {
        return Some(OpInfo {
            operation: Operation::NOP,
            dst_regs: vec![],
            src_regs: vec![],
            imm: 0,
            code_word: code_word,
        });
    }
    match opcode_to_insttype(opcode) {
        Some(InstType::U) => Some(decode_u_type(code_word)),
        Some(InstType::J) => Some(decode_j_type(code_word)),
        Some(InstType::I) => Some(decode_i_type(code_word)),
        Some(InstType::S) => Some(decode_s_type(code_word)),
        Some(InstType::R) => Some(decode_r_type(code_word)),
        Some(InstType::B) => Some(decode_b_type(code_word)),
        None => None,
    }
}

fn fetch(state: &ArchitectureState) -> u32 {
    state.memory.read(state.pc, 4)
}

fn execute(state: &mut ArchitectureState, opinfo: OpInfo) {
    let mut npc = state.pc + 4;

    match opinfo.operation {
        Operation::NOP => {}
        Operation::JAL => {
            state.regwrite(opinfo.dst_regs[0], npc);
            npc = state.pc.wrapping_add(opinfo.imm as u32);
        }
        Operation::JALR => {
            state.regwrite(opinfo.dst_regs[0], npc);
            npc = state.regread(opinfo.src_regs[0].wrapping_add(opinfo.imm as u32));
        }
        Operation::LUI => {
            state.regwrite(opinfo.dst_regs[0], (opinfo.imm << 12) as u32);
        }
        Operation::ADDI => {
            let val = state
                .regread(opinfo.src_regs[0])
                .wrapping_add(opinfo.imm as u32);
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::ADD => {
            let src0 = state.regread(opinfo.src_regs[0]);
            let src1 = state.regread(opinfo.src_regs[1]);
            let val = src0.wrapping_add(src1);
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::SUB => {
            let src0 = state.regread(opinfo.src_regs[0]);
            let src1 = state.regread(opinfo.src_regs[1]);
            let val = src0.wrapping_sub(src1);
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::ANDI => {
            let val = state.regread(opinfo.src_regs[0]) & opinfo.imm as u32;
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::ORI => {
            let val = state.regread(opinfo.src_regs[0]) | opinfo.imm as u32;
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::XORI => {
            let val = state.regread(opinfo.src_regs[0]) ^ opinfo.imm as u32;
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::AND => {
            let src0 = state.regread(opinfo.src_regs[0]);
            let src1 = state.regread(opinfo.src_regs[1]);
            let val = src0 & src1;
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::OR => {
            let src0 = state.regread(opinfo.src_regs[0]);
            let src1 = state.regread(opinfo.src_regs[1]);
            let val = src0 | src1;
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::XOR => {
            let src0 = state.regread(opinfo.src_regs[0]);
            let src1 = state.regread(opinfo.src_regs[1]);
            let val = src0 ^ src1;
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::SLTI => {
            let val = if (state.regread(opinfo.src_regs[0]) as i32) < opinfo.imm {
                1
            } else {
                0
            };
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::SLTIU => {
            let val = if state.regread(opinfo.src_regs[0]) < opinfo.imm as u32 {
                1
            } else {
                0
            };
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::SLT => {
            let src0 = state.regread(opinfo.src_regs[0]) as i32;
            let src1 = state.regread(opinfo.src_regs[1]) as i32;
            let val = if src0 < src1 { 1 } else { 0 };
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::SLTU => {
            let src0 = state.regread(opinfo.src_regs[0]);
            let src1 = state.regread(opinfo.src_regs[1]);
            let val = if src0 < src1 { 1 } else { 0 };
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::SW | Operation::SH | Operation::SB => {
            let addr = state
                .regread(opinfo.src_regs[0])
                .wrapping_add(opinfo.imm as u32);
            let (mask, size) = match opinfo.operation {
                Operation::SW => (0xffffffff, 4),
                Operation::SH => (0xffff, 2),
                Operation::SB => (0xff, 1),
                _ => panic!("can't reach here"),
            };
            let val = state.regread(opinfo.src_regs[1]) & mask;
            state.memory.write(addr, val, size);
        }
        Operation::LW | Operation::LH | Operation::LB | Operation::LHU | Operation::LBU => {
            let addr = state
                .regread(opinfo.src_regs[0])
                .wrapping_add(opinfo.imm as u32);
            let (size, sext) = match opinfo.operation {
                Operation::LW => (4, false),
                Operation::LH => (2, true),
                Operation::LHU => (2, false),
                Operation::LB => (1, true),
                Operation::LBU => (1, false),
                _ => panic!("can't reach here"),
            };
            let val = state.memory.read(addr, size);
            let val = extract_bits(val, 0, 8 * size, sext);
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::BEQ => {
            let src0 = state.regread(opinfo.src_regs[0]);
            let src1 = state.regread(opinfo.src_regs[1]);
            if src0 == src1 {
                npc = state.pc.wrapping_add(opinfo.imm as u32);
            }
        }
        Operation::BNE => {
            let src0 = state.regread(opinfo.src_regs[0]);
            let src1 = state.regread(opinfo.src_regs[1]);
            if src0 != src1 {
                npc = state.pc.wrapping_add(opinfo.imm as u32);
            }
        }
        Operation::BLT => {
            let src0 = state.regread(opinfo.src_regs[0]) as i32;
            let src1 = state.regread(opinfo.src_regs[1]) as i32;
            if src0 < src1 {
                npc = state.pc.wrapping_add(opinfo.imm as u32);
            }
        }
        Operation::BLTU => {
            let src0 = state.regread(opinfo.src_regs[0]);
            let src1 = state.regread(opinfo.src_regs[1]);
            if src0 < src1 {
                npc = state.pc.wrapping_add(opinfo.imm as u32);
            }
        }
        Operation::BGE => {
            let src0 = state.regread(opinfo.src_regs[0]) as i32;
            let src1 = state.regread(opinfo.src_regs[1]) as i32;
            if src0 >= src1 {
                npc = state.pc.wrapping_add(opinfo.imm as u32);
            }
        }
        Operation::BGEU => {
            let src0 = state.regread(opinfo.src_regs[0]);
            let src1 = state.regread(opinfo.src_regs[1]);
            if src0 >= src1 {
                npc = state.pc.wrapping_add(opinfo.imm as u32);
            }
        }
        Operation::SLLI => {
            let val = state.regread(opinfo.src_regs[0]) << opinfo.imm;
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::SRLI => {
            let val = state.regread(opinfo.src_regs[0]) >> opinfo.imm;
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::SRAI => {
            let val = (state.regread(opinfo.src_regs[0]) as i32) >> opinfo.imm;
            state.regwrite(opinfo.dst_regs[0], val as u32);
        }
        Operation::SLL => {
            let src0 = state.regread(opinfo.src_regs[0]);
            let src1 = state.regread(opinfo.src_regs[1]);
            let val = src0 << src1;
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::SRL => {
            let src0 = state.regread(opinfo.src_regs[0]);
            let src1 = state.regread(opinfo.src_regs[1]);
            let val = src0 >> src1;
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::SRA => {
            let src0 = state.regread(opinfo.src_regs[0]);
            let src1 = state.regread(opinfo.src_regs[1]);
            let val = ((src0 as i32) >> src1) as u32;
            state.regwrite(opinfo.dst_regs[0], val);
        }
        Operation::EBREAK => {} // treat as NOP
        Operation::AUIPC => {
            let val = state.pc + (opinfo.imm << 12) as u32;
            state.regwrite(opinfo.dst_regs[0], val);
        }
        _ => unimplemented!("unimplemented operation: {:?}", opinfo),
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
    let maxinsns: u32 = args[2].parse().unwrap();

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
    let mut i = 0;
    let mut pc_stats: HashMap<u32, u32> = HashMap::new();
    while i <= maxinsns {
        let old_pc = arch_state.pc;
        let c = 1 + match pc_stats.get(&arch_state.pc) {
            Some(x) => *x,
            None => 0,
        };
        pc_stats.insert(arch_state.pc, c);
        i += 1;
        let code_word = fetch(&arch_state);
        let opinfo = decode(code_word);
        unsafe {
            if DEBUG_PRINT {
                //eprintln!("\n{}:", i);
                //eprintln!("PC: 0x{:x} {:?}", arch_state.pc, opinfo);
            }
        }
        match opinfo {
            Some(opinfo) => {
                unsafe {
                    if DEBUG_PRINT {
                        eprintln!(
                            "{:08x} 32'h{:08x} {:?} {:?} {:?} {}",
                            arch_state.pc,
                            opinfo.code_word,
                            opinfo.operation,
                            opinfo.src_regs,
                            opinfo.dst_regs,
                            opinfo.imm
                        );
                    }
                }
                execute(&mut arch_state, opinfo)
            }
            None => {
                eprintln!(
                    "invalid code_word (PC: {:x}): {:b}",
                    arch_state.pc, code_word
                );
                panic!("invalid code_word");
            }
        };
        unsafe {
            TIME += 1000;
        }
        if true {
            //i >= maxinsns * 9 / 10 {
            unsafe {
                DEBUG_PRINT = true;
            }
        }

        if arch_state.pc == old_pc {
            break;
        }
    }

    unsafe {
        if DEBUG_PRINT {
            for (pc, counter) in &pc_stats {
                eprintln!("0x{:x}\t{}", pc, counter);
            }
        }
    }

    for i in 0..32 {
        eprintln!("r{}:\t{:x}", i, arch_state.regs[i]);
    }
}
