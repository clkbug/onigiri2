struct ArchitectureState {
    pc: u32,
    regs: [u32; 32],
}

struct DecodedInstruction {
    opcode: u32,
    dstRegs: Vec<i32>,
    srcRegs: Vec<i32>,
    imm: i32,
}

fn decode(codeWord: u32) -> DecodedInstruction {
    let opcode = codeWord & 0x32;
    return DecodedInstruction {
        opcode: opcode,
        dstRegs: vec![1, 2],
        srcRegs: vec![1],
        imm: 0,
    };
}
fn main() {
    let arch_state = ArchitectureState {
        pc: 0x8000,
        regs: [0; 32],
    };
    println!("Hello, world! {}", arch_state.pc);
}
