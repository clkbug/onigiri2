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

fn opcode_to_insttype(opcode: u32) -> InstType {
    match opcode {
        0b0110111 | 0b0010111 => InstType::U, // LUI AUIPC
        0b1101111 => InstType::J,             // JAL
        0b1100111 | 0b0000011 | 0b0010011 | 0b0001111 | 0b1110011 => InstType::I, // JALR (LB LH LW LBU LHU) (ADDI SLTI SLTIU XORI ORI ANDI) (FENCE FENCE.I) (ECALL EBREAK CSRRW CSRRS CSRRC CSRRWI CSRRSI CSRRCI)
        0b1100011 => InstType::B, // (BEQ, BNE, BLT, BGE, BLTU, BGEU)
        0b0100011 => InstType::S, // (SB SH SW)
        0b0110011 => InstType::R, // (ADD SUB SLL SLT SLTU XOR SRL SRA OR AND)
        _ => panic!("invalid opcode!"),
    }
}

fn decode_u_type(code_word: u32) -> OpInfo {
    let opcode = code_word & 0x7f;
    return OpInfo {
        operation: Operation::LUI,
        dstRegs: vec![1, 2],
        srcRegs: vec![1],
        imm: 0,
    };
}

fn decode(code_word: u32) -> OpInfo {
    let opcode = code_word & 0x7f;
    return match opcode {
        0x38 => decode_u_type(code_word),
        _ => OpInfo {
            operation: Operation::UNDEFINED,
            dstRegs: vec![0],
            srcRegs: vec![0],
            imm: 1,
        },
    };
}
fn main() {
    let arch_state = ArchitectureState {
        pc: 0x8000,
        regs: [0; 32],
    };
    println!("Hello, world! {}", arch_state.pc);
    println!("{}", decode(0).imm);
}
