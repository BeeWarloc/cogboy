use nom::{IResult, eof, space, digit, hex_digit, alphanumeric};

use std::str::{self, FromStr};
use std::borrow::Cow;

#[derive(Debug,Clone,Copy)]
pub enum Breakpoint
{
    Code(u16),
    Watch(u16),
    Cycle(u64)
}

#[derive(Debug, Clone)]
pub enum Command {
    ShowRegs,
    Step,
    RevStep,
    Continue,
    Goto(u32),
    ShowMem(Option<u32>),
    Disassemble(Option<usize>),
    Label,
    AddLabel(String, u32),
    RemoveLabel(String),
    ListBreakpoints,
    ListWatchpoints,
    AddBreakpoint(Breakpoint),
    RemoveBreakpoint(Breakpoint),
    Save,
    Load,

    Exit,
    Repeat,
}

impl FromStr for Command {
    type Err = Cow<'static, str>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match command(s.as_bytes()) {
            IResult::Done(_, c) => Ok(c),
            err => Err(format!("Unable to parse command: {:?}", err).into()),
        }
    }
}

named!(
    command<Command>,
    complete!(
        terminated!(
        alt_complete!(
            save_state |
            load_state |
            step |
            revstep |
            continue_ |
            goto |
            show_mem |
            disassemble |
            label |
            add_label |
            remove_label |
            breakpoint |
            add_breakpoint |
            remove_breakpoint |
            watchpoint |
            add_watchpoint |
            remove_watchpoint |
            exit |
            show_regs |
            repeat),
        eof)));

named!(
    step<Command>,
    map!(
        alt_complete!(tag!("step") | tag!("s")),
    |_| Command::Step));

named!(
    revstep<Command>,
    map!(
        alt_complete!(tag!("rstep") | tag!("rs")),
    |_| Command::RevStep));

named!(
    continue_<Command>,
    map!(
        alt_complete!(tag!("continue") | tag!("c")),
    |_| Command::Continue));

named!(
    goto<Command>,
    chain!(
        alt_complete!(tag!("goto") | tag!("g")) ~
        addr: preceded!(space, hex_u32_parser),
    || Command::Goto(addr)));

named!(
    show_mem<Command>,
    chain!(
        alt_complete!(tag!("showmem") | tag!("mem") | tag!("m")) ~
        addr: opt!(preceded!(space, hex_u32_parser)),
    || Command::ShowMem(addr)));

named!(
    hex_u32_parser<u32>,
    map_res!(
        map_res!(
            preceded!(opt!(alt_complete!(tag!("0x") | tag!("$"))), hex_digit),
            str::from_utf8),
    |s| u32::from_str_radix(s, 16)));

named!(
    hex_u16_parser<u16>,
    map_res!(
        map_res!(
            preceded!(opt!(alt_complete!(tag!("0x") | tag!("$"))), hex_digit),
            str::from_utf8),
    |s| u16::from_str_radix(s, 16)));

named!(
    disassemble<Command>,
    chain!(
        alt_complete!(tag!("disassemble") | tag!("d")) ~
        count: opt!(preceded!(space, usize_parser)),
    || Command::Disassemble(count)));

named!(
    usize_parser<usize>,
    map_res!(
        map_res!(
            digit,
            str::from_utf8),
    FromStr::from_str));

named!(
    u64_dec_parser<u64>,
    map_res!(
        map_res!(
            digit,
            str::from_utf8),
    FromStr::from_str));

named!(
    label<Command>,
    map!(
        alt_complete!(tag!("label") | tag!("l")),
    |_| Command::Label));

named!(
    add_label<Command>,
    chain!(
        alt_complete!(tag!("addlabel") | tag!("al")) ~
        space ~
        name: label_name ~
        space ~
        addr: hex_u32_parser,
    || Command::AddLabel(name, addr)));

named!(
    label_name<String>,
    preceded!(
        char!('.'),
        map_res!(
            map_res!(
                alphanumeric,
                str::from_utf8),
        FromStr::from_str)));

named!(
    remove_label<Command>,
    chain!(
        alt_complete!(tag!("removelabel") | tag!("rl")) ~
        space ~
        name: label_name,
    || Command::RemoveLabel(name)));

named!(
    breakpoint_code_type_parser<Breakpoint>,
    chain!(
        addr: hex_u16_parser,
    || Breakpoint::Code(addr)));

named!(
    breakpoint_cycle_type_parser<Breakpoint>,
    chain!(
        cycle: preceded!(tag!("@"), u64_dec_parser),
    || Breakpoint::Cycle(cycle)));

named!(
    breakpoint_watchpoint_parser<Breakpoint>,
    chain!(
        addr: preceded!(tag!("w"), hex_u16_parser),
    || Breakpoint::Watch(addr)));

named!(
   breakpoint_parser<Breakpoint>,
   alt_complete!(breakpoint_code_type_parser | breakpoint_cycle_type_parser | breakpoint_watchpoint_parser));

named!(
    breakpoint<Command>,
    map!(
        alt_complete!(tag!("breakpoint") | tag!("b")),
    |_| Command::ListBreakpoints));

named!(
    add_breakpoint<Command>,
    chain!(
        alt_complete!(tag!("addbreakpoint") | tag!("ab")) ~
        space ~
        addr: breakpoint_parser,
    || Command::AddBreakpoint(addr)));

named!(
    remove_breakpoint<Command>,
    chain!(
        alt_complete!(tag!("removebreakpoint") | tag!("rb")) ~
        space ~
        addr: breakpoint_parser,
    || Command::RemoveBreakpoint(addr)));

named!(
    watchpoint<Command>,
    map!(
        alt_complete!(tag!("watchpoint") | tag!("w")),
    |_| Command::ListWatchpoints));

named!(
    add_watchpoint<Command>,
    chain!(
        alt_complete!(tag!("addwatchpoint") | tag!("aw")) ~
        space ~
        addr: hex_u32_parser,
    || Command::AddBreakpoint(Breakpoint::Watch(addr as u16))));

named!(
    remove_watchpoint<Command>,
    chain!(
        alt_complete!(tag!("removewatchpoint") | tag!("rw")) ~
        space ~
        addr: hex_u32_parser,
    || Command::RemoveBreakpoint(Breakpoint::Watch(addr as u16))));

named!(
    exit<Command>,
    map!(
        alt_complete!(tag!("exit") | tag!("quit") | tag!("e") | tag!("x") | tag!("q")),
        |_| Command::Exit));

named!(
    show_regs<Command>,
    map!(
        alt_complete!(tag!("showregs") | tag!("r")),
    |_| Command::ShowRegs));

named!(
    save_state<Command>,
    map!(
        alt_complete!(tag!("save")),
    |_| Command::Save));

named!(
    load_state<Command>,
    map!(
        alt_complete!(tag!("load")),
    |_| Command::Load));
named!(
    repeat<Command>,
    value!(Command::Repeat));
