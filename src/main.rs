struct ArchitectureState {
    pc: u32,
    regs: [u32; 32],
}

struct DecodedInstruction {
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

fn decode_u_type(code_word: u32) -> DecodedInstruction {
    let opcode = code_word & 0x7f;
    return DecodedInstruction {
        operation: Operation::LUI,
        dstRegs: vec![1, 2],
        srcRegs: vec![1],
        imm: 0,
    };
}

fn decode(code_word: u32) -> DecodedInstruction {
    let opcode = code_word & 0x7f;
    return match opcode {
        0x38 => decode_u_type(code_word),
        _ => DecodedInstruction {
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
