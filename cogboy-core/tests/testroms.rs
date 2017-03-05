extern crate cogboy_core;

use cogboy_core::System;

fn run_test_rom(path: &str, cycles: u64) {
    let mut system = System::new(path);
    system.run_to_cycle(cycles);
    assert!(system.drain_serial_out().contains("Passed"));
}

#[test]
fn cpu_instr_01_special() {
    run_test_rom("tests/gb-test-roms/cpu_instrs/individual/01-special.gb", 10000000);
}

#[test]
fn cpu_instr_02_interrupts() {
    run_test_rom("tests/gb-test-roms/cpu_instrs/individual/02-interrupts.gb", 20000000);
}

#[test]
fn cpu_instr_03_op_sp_hl() {
    run_test_rom("tests/gb-test-roms/cpu_instrs/individual/03-op sp,hl.gb", 20000000);
}

#[test]
fn cpu_instr_04_op_r_imm() {
    run_test_rom("tests/gb-test-roms/cpu_instrs/individual/04-op r,imm.gb", 20000000);
}