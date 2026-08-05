use libc::MAP_ANONYMOUS;
use libc::MAP_PRIVATE;
use libc::PROT_EXEC;
use libc::PROT_READ;
use libc::PROT_WRITE;
use std::env;
use std::fs;
use std::io::{Read, Write};
use std::process;
use std::ptr;
enum Op {
    Inc(usize),
    Dec(usize),
    Left(usize),
    Right(usize),
    Input(usize),
    Output(usize),
    Jz(usize),
    Jnz(usize),
}
impl Op {
    fn new(op: u8, cnt: usize) -> Self {
        match op {
            b'+' => Op::Inc(cnt),
            b'-' => Op::Dec(cnt),
            b'<' => Op::Left(cnt),
            b'>' => Op::Right(cnt),
            b',' => Op::Input(cnt),
            b'.' => Op::Output(cnt),
            b'[' => Op::Jz(cnt),
            b']' => Op::Jnz(cnt),
            _ => panic!("Invalid operation: {}", op),
        }
    }
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Inc(cnt) => write!(f, "Inc({})", cnt),
            Op::Dec(cnt) => write!(f, "Dec({})", cnt),
            Op::Left(cnt) => write!(f, "Left({})", cnt),
            Op::Right(cnt) => write!(f, "Right({})", cnt),
            Op::Input(cnt) => write!(f, "Input({})", cnt),
            Op::Output(cnt) => write!(f, "Output({})", cnt),
            Op::Jz(cnt) => write!(f, "Jz({})", cnt),
            Op::Jnz(cnt) => write!(f, "Jnz({})", cnt),
        }
    }
}
struct Lexer<'a> {
    code: &'a [u8],
    pos: usize,
}
impl<'a> Lexer<'a> {
    fn new(code: &'a str) -> Self {
        Self {
            code: code.as_bytes(),
            pos: 0,
        }
    }
    fn next(&mut self) -> Option<u8> {
        while self.pos < self.code.len() && !b"+-<>.,[]".contains(&self.code[self.pos]) {
            self.pos += 1;
        }
        if self.pos >= self.code.len() {
            None
        } else {
            let ch = self.code[self.pos];
            self.pos += 1;
            Some(ch)
        }
    }

    fn peek(&self) -> Option<u8> {
        let mut pos = self.pos;
        while pos < self.code.len() && !b"+-<>.,[]".contains(&self.code[pos]) {
            pos += 1;
        }
        self.code.get(pos).copied()
    }
}
struct Backpatch {
    addr: usize,
    src_addr: usize,
    dst_index: usize,
}
impl Backpatch {
    fn new(addr: usize, src_addr: usize, dst_index: usize) -> Self {
        Self {
            addr,
            src_addr,
            dst_index,
        }
    }
}
fn parse(code: &str) -> Result<Vec<Op>, ()> {
    let mut ops = Vec::new();
    let mut addr_stack = Vec::new();
    let mut lexer = Lexer::new(code);
    while let Some(ch) = lexer.next() {
        match ch {
            b'+' | b'-' | b'<' | b'>' | b',' | b'.' => {
                let mut cnt = 1;
                while let Some(s) = lexer.peek() {
                    if s == ch {
                        lexer.next();
                        cnt += 1;
                    } else {
                        break;
                    }
                }
                ops.push(Op::new(ch, cnt));
            }
            b'[' => {
                let addr = ops.len();
                ops.push(Op::new(ch, 0));
                addr_stack.push(addr);
            }
            b']' => {
                let addr = addr_stack.pop().ok_or_else(|| {
                    eprintln!("Unmatched closing bracket at position {}.\n", lexer.pos);
                })?;
                ops.push(Op::new(ch, addr + 1));
                ops[addr] = Op::new(b'[', ops.len());
            }
            _ => {
                eprintln!("Invalid character in Brainfuck code: {}", ch);
                return Err(());
            }
        }
    }
    if !addr_stack.is_empty() {
        eprintln!(
            "Unmatched opening bracket(s) at positions: {:?}\n",
            addr_stack
        );
        return Err(());
    }
    Ok(ops)
}
type Code = extern "C" fn(*mut u8);

fn interpret(ops: &[Op]) {
    let mut memory = vec![0u8];
    let mut head = 0;
    let mut ip = 0;
    while ip < ops.len() {
        match ops[ip] {
            Op::Inc(cnt) => {
                memory[head] = memory[head].wrapping_add(cnt as u8);
                ip += 1;
            }
            Op::Dec(cnt) => {
                memory[head] = memory[head].wrapping_sub(cnt as u8);
                ip += 1;
            }
            Op::Left(cnt) => {
                if head < cnt {
                    eprintln!(
                        "ERROR: Memory underflow at instruction {}: moving left by {} from position {}.",
                        ip, cnt, head
                    );
                    process::exit(1);
                }
                head = head.saturating_sub(cnt);
                ip += 1;
            }
            Op::Right(cnt) => {
                head = head.saturating_add(cnt);
                if head >= memory.len() {
                    memory.resize(head + 1, 0);
                }
                ip += 1;
            }
            Op::Input(_) => {
                std::io::stdout().flush().unwrap();
                let mut buffer = [0u8; 1];
                std::io::stdin().read_exact(&mut buffer).unwrap();
                memory[head] = buffer[0];
                ip += 1;
            }
            Op::Output(cnt) => {
                for _ in 0..cnt {
                    print!("{}", memory[head] as char);
                }
                ip += 1;
            }
            Op::Jz(addr) => {
                if memory[head] == 0 {
                    ip = addr;
                } else {
                    ip += 1;
                }
            }
            Op::Jnz(addr) => {
                if memory[head] != 0 {
                    ip = addr;
                } else {
                    ip += 1;
                }
            }
        }
    }
    std::io::stdout().flush().unwrap();
}
fn jit_compile(ops: &[Op]) -> Code {
    let mut code: Vec<u8> = Vec::new();
    let mut backpatches: Vec<Backpatch> = Vec::new();
    let mut addrs: Vec<usize> = Vec::new();
    for op in ops {
        addrs.push(code.len());
        match op {
            Op::Inc(value) => {
                code.push(0x80); // add byte ptr [rdi], imm8
                code.push(0x07);
                code.push((*value % 256) as u8);
            }
            Op::Dec(value) => {
                code.push(0x80); // sub byte ptr [rdi], imm8
                code.push(0x2F);
                code.push((*value % 256) as u8);
            }
            Op::Left(value) => {
                code.push(0x48);
                code.push(0x81);
                code.push(0xEF); // sub rdi, imm32
                let value = *value as u32;
                code.extend_from_slice(&value.to_le_bytes());
            }
            Op::Right(value) => {
                code.push(0x48);
                code.push(0x81);
                code.push(0xC7); // add rdi, imm32
                let value = *value as u32;
                code.extend_from_slice(&value.to_le_bytes());
            }
            Op::Input(cnt) => {
                for _ in 0..*cnt {
                    code.push(0x57); // push rdi
                    code.extend_from_slice(&[0x48, 0xC7, 0xC0, 0x00, 0x00, 0x00, 0x00]); // mov rax, 0
                    code.extend_from_slice(&[0x48, 0x89, 0xFE]); // mov rsi, rdi
                    code.extend_from_slice(&[0x48, 0xC7, 0xC7, 0x00, 0x00, 0x00, 0x00]); // mov rdi, 0
                    code.extend_from_slice(&[0x48, 0xC7, 0xC2, 0x01, 0x00, 0x00, 0x00]); // mov rdx, 1
                    code.extend_from_slice(&[0x0F, 0x05]); // syscall
                    code.push(0x5F); // pop rdi
                }
            }
            Op::Output(cnt) => {
                for _ in 0..*cnt {
                    code.push(0x57); // push rdi
                    code.extend_from_slice(&[0x48, 0xC7, 0xC0, 0x01, 0x00, 0x00, 0x00]); // mov rax, 1
                    code.extend_from_slice(&[0x48, 0x89, 0xFE]); // mov rsi, rdi
                    code.extend_from_slice(&[0x48, 0xC7, 0xC7, 0x01, 0x00, 0x00, 0x00]); // mov rdi, 1
                    code.extend_from_slice(&[0x48, 0xC7, 0xC2, 0x01, 0x00, 0x00, 0x00]); // mov rdx, 1
                    code.extend_from_slice(&[0x0F, 0x05]); // syscall
                    code.push(0x5F); // pop rdi
                }
            }
            Op::Jz(value) => {
                code.extend_from_slice(&[0x8A, 0x07]); // mov al, byte ptr [rdi]
                code.extend_from_slice(&[0x84, 0xC0]); // test al, al
                code.extend_from_slice(&[0x0F, 0x84]); // jz rel32
                let op_addr = code.len();
                code.extend_from_slice(&[0x00; 4]); // placeholder for jump address
                let next_ip = code.len();
                backpatches.push(Backpatch::new(op_addr, next_ip, *value));
            }
            Op::Jnz(value) => {
                code.extend_from_slice(&[0x8A, 0x07]); // mov al, byte ptr [rdi]
                code.extend_from_slice(&[0x84, 0xC0]); // test al, al
                code.extend_from_slice(&[0x0F, 0x85]); // jnz rel32
                let op_addr = code.len();
                code.extend_from_slice(&[0x00; 4]); // placeholder for jump address
                let next_ip = code.len();
                backpatches.push(Backpatch::new(op_addr, next_ip, *value));
            }
        }
    }
    addrs.push(code.len());
    for bp in backpatches {
        let dst_addr = addrs[bp.dst_index];
        let rel32 = (dst_addr as isize - bp.src_addr as isize) as i32;
        let rel32_bytes = rel32.to_le_bytes();
        code[bp.addr..bp.addr + 4].copy_from_slice(&rel32_bytes);
    }
    code.push(0xC3);
    let len = code.len();
    unsafe {
        let ptr = libc::mmap(
            ptr::null_mut(),
            len,
            PROT_READ | PROT_WRITE | PROT_EXEC,
            MAP_PRIVATE | MAP_ANONYMOUS,
            -1,
            0,
        );
        if ptr == libc::MAP_FAILED {
            eprintln!("mmap failed");
            process::exit(1);
        }
        ptr::copy_nonoverlapping(code.as_ptr(), ptr as *mut u8, len);
        std::mem::transmute::<*mut u8, Code>(ptr as *mut u8)
    }
}
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} [--interpret] <brainfuck_file>", args[0]);
        process::exit(1);
    }
    let mut interpret_mode = false;
    let mut filename = None;
    for arg in &args[1..] {
        if arg == "--interpret" {
            interpret_mode = true;
        } else if filename.is_none() {
            filename = Some(arg);
        } else {
            eprintln!("Invalid argument: {}", arg);
            process::exit(1);
        }
    }
    let filename = filename.unwrap_or_else(|| {
        eprintln!("No input file specified.");
        process::exit(1);
    });
    let code = fs::read_to_string(filename).unwrap_or_else(|e| {
        eprintln!("Failed to read file {}: {}", filename, e);
        process::exit(1);
    });
    let ops = parse(&code).unwrap_or_else(|_| {
        eprintln!("Failed to parse Brainfuck code.");
        process::exit(1);
    });
    // for (i, op) in ops.iter().enumerate() {
    //     println!("{}: {}", i, op);
    // }
    if interpret_mode {
        interpret(&ops);
    } else {
        let code = jit_compile(&ops);
        let mut memory = vec![0u8; 1_000_000];
        code(memory.as_mut_ptr());
    }
}
