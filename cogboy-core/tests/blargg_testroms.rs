extern crate cogboy_core;

use cogboy_core::{RunContext, System};

struct TestContext;
impl RunContext for TestContext {
    fn check_watchpoint(&mut self, _addr: u16, _value: u8) {}
}

fn run_test_rom(path: &str) {
    let mut system = System::new(path);
    let mut log = String::new();
    let mut ctx = TestContext;
    while system.cpu.cycles < 100_000_000 {
        let target_cycles = system.cpu.cycles + 100_000;
        system.run_to_cycle(target_cycles, &mut ctx);
        let serial_out = system.drain_serial_out();
        log.push_str(serial_out.as_str());
        if log.contains("Passed") || log.contains("Failed") {
            // Run a little more just to make sure to get the serial bytes after "Failed" out
            let target_cycles = system.cpu.cycles + 100_000;
            system.run_to_cycle(target_cycles, &mut ctx);
            break;
        }
    }
    println!("{}", log);
    assert!(log.contains("Passed"));
}

#[test]
fn cpu_instr_01_special() {
    run_test_rom("tests/gb-test-roms/cpu_instrs/individual/01-special.gb");
}

#[test]
fn cpu_instr_02_interrupts() {
    run_test_rom("tests/gb-test-roms/cpu_instrs/individual/02-interrupts.gb");
}

#[test]
fn cpu_instr_03_op_sp_hl() {
    run_test_rom("tests/gb-test-roms/cpu_instrs/individual/03-op sp,hl.gb");
}

#[test]
fn cpu_instr_04_op_r_imm() {
    run_test_rom("tests/gb-test-roms/cpu_instrs/individual/04-op r,imm.gb");
}

#[test]
fn cpu_instr_05_op_rp() {
    run_test_rom("tests/gb-test-roms/cpu_instrs/individual/05-op rp.gb");
}

#[test]
fn cpu_instr_06_ld_r_r() {
    run_test_rom("tests/gb-test-roms/cpu_instrs/individual/06-ld r,r.gb");
}

#[test]
fn cpu_instr_07_jr_jp_call_ret_rst() {
    run_test_rom("tests/gb-test-roms/cpu_instrs/individual/07-jr,jp,call,ret,rst.gb");
}

#[test]
fn cpu_instr_08_misc_instr() {
    run_test_rom("tests/gb-test-roms/cpu_instrs/individual/08-misc instrs.gb");
}

#[test]
fn cpu_instr_09_op_r_r() {
    run_test_rom("tests/gb-test-roms/cpu_instrs/individual/09-op r,r.gb");
}

#[test]
fn cpu_instr_10_bit_ops() {
    run_test_rom("tests/gb-test-roms/cpu_instrs/individual/10-bit ops.gb");
}

#[test]
fn cpu_instr_11_op_a_hl() {
    run_test_rom("tests/gb-test-roms/cpu_instrs/individual/11-op a,(hl).gb");
}

#[test]
fn instr_timing() {
    run_test_rom("tests/gb-test-roms/instr_timing/instr_timing.gb");
}
