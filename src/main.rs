struct ArchitectureState {
    pc: u32,
    regs: [u32; 32],
}

struct OpInfo {
    operation: Operation,
    dstRegs: Vec<i32>,
    srcRegs: Vec<i32>,
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
        dstRegs: vec![rd as i32],
        srcRegs: vec![],
        imm: imm as i32,
    };
}

fn decode(code_word: u32) -> OpInfo {
    let opcode = code_word & 0x7f;
    match opcode_to_insttype(opcode) {
        Some(InstType::U) => decode_u_type(code_word),

        None => panic!("invalid code_word"),
        _ => unimplemented!("decode"),
    }
}
fn main() {
    let arch_state = ArchitectureState {
        pc: 0x8000,
        regs: [0; 32],
    };
    println!("Hello, world! {}", arch_state.pc);
    println!("{}", decode(0).imm);
}
