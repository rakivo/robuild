#[derive(Debug)]
pub enum Trap {
    StackOverflow,
    StackUnderflow,
    DivisionByZero,
    InvalidOperand,
    IllegalInstruction,
    IllegalInstructionAccess
}

#[derive(Debug, PartialEq)]
enum Flag {JE, JL, JNGE, JG, JNLE, JZ}

#[derive(Debug)]
pub enum Inst {
    NOP,
    PUSH(Word),
    POP,
    ADD,
    SUB,
    MUL,
    DIV,
    CMP,
    DUP(usize),
    JE(usize),
    JL(usize),
    JNGE(usize),
    JG(usize),
    JNLE(usize),
    JZ(usize),
    JMP(usize),
    HALT
}

macro_rules! extend_from_byte {
    ($a: expr, $ret: expr) => {{
        let mut bytes = vec![$a];
        bytes.extend(&$ret.to_le_bytes());
        bytes
    }};
}

macro_rules! inst_from_bytes {
    ($b: ident, $ret: tt, $ty: ty) => {
        if $b.len() >= 9 {
            let mut array = [0; 8];
            array.copy_from_slice(&$b[1..9]);
            let a = usize::from_le_bytes(array);
            Ok((Inst::$ret(a as $ty), 9))
        } else { Err(Trap::InvalidOperand) }
    }
}

impl Inst {
    fn to_bytes(&self) -> Vec::<u8> {
        match self {
            Inst::NOP        => vec![0],
            Inst::PUSH(val)  => extend_from_byte!(1, *val),
            Inst::POP        => vec![2],
            Inst::ADD        => vec![3],
            Inst::SUB        => vec![4],
            Inst::MUL        => vec![5],
            Inst::DIV        => vec![6],
            Inst::CMP        => vec![7],
            Inst::DUP(val)   => extend_from_byte!(8, *val),
            Inst::JE(addr)   => extend_from_byte!(9, *addr),
            Inst::JL(addr)   => extend_from_byte!(10, *addr),
            Inst::JNGE(addr) => extend_from_byte!(11, *addr),
            Inst::JG(addr)   => extend_from_byte!(12, *addr),
            Inst::JNLE(addr) => extend_from_byte!(13, *addr),
            Inst::JZ(addr)   => extend_from_byte!(14, *addr),
            Inst::JMP(addr)  => extend_from_byte!(15, *addr),
            Inst::HALT       => vec![16],
        }
    }

    fn from_bytes(bytes: &[u8]) -> Result<(Inst, usize), Trap> {
        match bytes.get(0) {
            Some(0)  => Ok((Inst::NOP, 1)),
            Some(1)  => inst_from_bytes!(bytes, PUSH, Word),
            Some(2)  => Ok((Inst::POP, 1)),
            Some(3)  => Ok((Inst::ADD, 1)),
            Some(4)  => Ok((Inst::SUB, 1)),
            Some(5)  => Ok((Inst::MUL, 1)),
            Some(6)  => Ok((Inst::DIV, 1)),
            Some(7)  => Ok((Inst::CMP, 1)),
            Some(8)  => inst_from_bytes!(bytes, DUP, usize),
            Some(9)  => inst_from_bytes!(bytes, JE, usize),
            Some(10) => inst_from_bytes!(bytes, JL, usize),
            Some(11) => inst_from_bytes!(bytes, JNGE, usize),
            Some(12) => inst_from_bytes!(bytes, JG, usize),
            Some(13) => inst_from_bytes!(bytes, JNLE, usize),
            Some(14) => inst_from_bytes!(bytes, JZ, usize),
            Some(15) => inst_from_bytes!(bytes, JMP, usize),
            Some(16) => Ok((Inst::HALT, 1)),
            _        => Err(Trap::IllegalInstruction),
        }
    }
}

impl std::convert::TryFrom<&str> for Inst {
    type Error = Trap;

    fn try_from(s: &str) -> Result<Inst, Self::Error> {
        let mut splitted = s.split_whitespace();
        let inst = splitted.next().ok_or(Trap::IllegalInstruction)?;
        let oper = if let Some(oper) = splitted.next() {
            Some(oper.parse::<Word>().map_err(|_| Trap::InvalidOperand)?)
        } else { None };
        match inst {
            "nop"  => Ok(Inst::NOP),
            "pop"  => Ok(Inst::POP),
            "add"  => Ok(Inst::ADD),
            "sub"  => Ok(Inst::SUB),
            "mul"  => Ok(Inst::MUL),
            "div"  => Ok(Inst::DIV),
            "cmp"  => Ok(Inst::CMP),
            "halt" => Ok(Inst::HALT),
            "push" => Ok(Inst::PUSH(oper.ok_or(Trap::InvalidOperand)?)),
            "dup"  => Ok(Inst::DUP(oper.ok_or(Trap::InvalidOperand)? as usize)),
            "je"   => Ok(Inst::JE(oper.ok_or(Trap::InvalidOperand)? as usize)),
            "jl"   => Ok(Inst::JL(oper.ok_or(Trap::InvalidOperand)? as usize)),
            "jnge" => Ok(Inst::JNGE(oper.ok_or(Trap::InvalidOperand)? as usize)),
            "jg"   => Ok(Inst::JG(oper.ok_or(Trap::InvalidOperand)? as usize)),
            "jnle" => Ok(Inst::JNLE(oper.ok_or(Trap::InvalidOperand)? as usize)),
            "jz"   => Ok(Inst::JZ(oper.ok_or(Trap::InvalidOperand)? as usize)),
            "jmp"  => Ok(Inst::JMP(oper.ok_or(Trap::InvalidOperand)? as usize)),
            _      => Err(Trap::IllegalInstruction)
        }
    }
}

pub type Word = i64;

pub struct Mm<'a> {
    stack: Vec::<Word>,
    flags: Vec::<Flag>,
    program: &'a [Inst],
    ip: usize,
    halt: bool
}

impl std::fmt::Debug for Mm<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "stack size: {size}\n", size = self.stack.len())?;
        write!(f, "stack:")?;
        let mut i = 0;
        while i < self.stack.len() {
            write!(f, "\n\t{oper}", oper = self.stack[i])?;
            i += 1;
        }
        Ok(())
    }
}

impl std::fmt::Display for Mm<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "stack: ")?;
        if let Some(first) = self.stack.first() {
            write!(f, "{first}, ")?;
            let (mut i, n) = (1, self.stack.len());
            while i < n {
                write!(f, "{oper}", oper = self.stack[i])?;
                if i < n - 1 { write!(f, ", ")?; }
                i += 1;
            }
        }
        Ok(())
    }
}

impl<'a> Mm<'a> {
    const FLAGS_CAP: usize = 6;
    const STACK_CAP: usize = 1024;

    pub fn new(program: &'a [Inst]) -> Mm<'a> {
        Mm {
            stack: Vec::with_capacity(Mm::STACK_CAP),
            flags: Vec::with_capacity(Mm::FLAGS_CAP),
            program,
            ip: 0,
            halt: false
        }
    }

    pub fn halt(&self) -> &bool {
        &self.halt
    }

    fn two_opers_inst(&mut self, inst: &Inst) -> Result<(), Trap> {
        let stack_len = self.stack.len();
        if stack_len < 2 {
            eprintln!("ERROR: Not enough operands on the stack, needed: 2, have: {stack_len}");
            return Err(Trap::StackUnderflow)
        }

        use Inst::*;
        let last = self.stack.pop().unwrap();
        let prelast = &mut self.stack[stack_len - 2];
        match inst {
            ADD => *prelast += last,
            SUB => *prelast -= last,
            MUL => *prelast *= last,
            DIV => if last != 0 {
                *prelast /= last
            } else { return Err(Trap::DivisionByZero) }
            CMP => if stack_len < Mm::STACK_CAP {
                *prelast -= last;
                if *prelast == 0 { self.flags.push(Flag::JZ); }
                if *prelast == last {
                    self.flags.push(Flag::JE);
                } else if *prelast < last {
                    self.flags.push(Flag::JL);
                    if *prelast != last { self.flags.push(Flag::JNGE); }
                } else if *prelast > last {
                    self.flags.push(Flag::JG);
                    if *prelast != last { self.flags.push(Flag::JNLE); }
                }
            } else { return Err(Trap::StackOverflow) }
            _ => return Err(Trap::IllegalInstruction)
        };

        self.ip += 1;
        Ok(())
    }

    fn jump_if_flag(&mut self, oper: &usize, flag: Flag) -> Result<(), Trap> {
        let program_len = self.program.len();
        if *oper >= program_len {
            eprintln!("ERROR: operand {oper} is outside of program bounds, program len: {program_len}");
            return Err(Trap::InvalidOperand);
        }

        if self.flags.contains(&flag) {
            self.ip = *oper;
        } else {
            self.ip += 1;
        }
        self.flags.clear();
        Ok(())
    }

    pub fn execute(&mut self) -> Result<(), Trap> {
        if self.ip >= self.program.len() {
            self.halt = true;
            return Ok(())
        }

        use Inst::*;
        let inst = &self.program[self.ip];
        match inst {
            NOP => Ok(()),
            PUSH(oper) => if self.stack.len() < Mm::STACK_CAP {
                self.stack.push(*oper);
                self.ip += 1;
                Ok(())
            } else { Err(Trap::StackOverflow) }
            POP => if self.stack.len() >= 1 {
                self.stack.pop();
                self.ip += 1;
                Ok(())
            } else { Err(Trap::StackUnderflow) }

            ADD => self.two_opers_inst(&ADD),
            SUB => self.two_opers_inst(&SUB),
            MUL => self.two_opers_inst(&MUL),
            DIV => self.two_opers_inst(&DIV),
            CMP => self.two_opers_inst(&CMP),

            DUP(oper) => if self.stack.len() > *oper {
                if self.stack.len() < Mm::STACK_CAP {
                    let val = self.stack[self.stack.len() - 1 - oper];
                    self.stack.push(val);
                    self.ip += 1;
                    Ok(())
                } else { Err(Trap::StackOverflow) }
            } else { Err(Trap::StackUnderflow) }

            Inst::JE(oper)   => self.jump_if_flag(oper, Flag::JE),
            Inst::JL(oper)   => self.jump_if_flag(oper, Flag::JL),
            Inst::JNGE(oper) => self.jump_if_flag(oper, Flag::JNGE),
            Inst::JG(oper)   => self.jump_if_flag(oper, Flag::JG),
            Inst::JNLE(oper) => self.jump_if_flag(oper, Flag::JNLE),
            Inst::JZ(oper)   => self.jump_if_flag(oper, Flag::JZ),

            JMP(oper) => if *oper < self.program.len() {
                self.ip = *oper;
                Ok(())
            } else {
                eprintln!("ERROR: operand {oper} is outside of program bounds, program len: {len}",
                          len = self.program.len());
                Err(Trap::IllegalInstructionAccess)
            }

            HALT => {
                self.halt = true;
                Ok(())
            }
        }
    }

    pub fn save_program_to_file(program: &Vec::<Inst>, file_path: &str) -> std::io::Result::<()> {
        use std::{fs::File, io::Write};

        let mut f = File::create(file_path)?;
        for inst in program {
            f.write_all(&inst.to_bytes())?;
        }
        Ok(())
    }

    pub fn load_program_from_file(file_path: &str) -> std::io::Result<Vec::<Inst>> {
        use std::{fs::read, io::*};

        let buf = read(file_path)?;
        let (mut i, mut program) = (0, Vec::new());
        while i < buf.len() {
            match Inst::from_bytes(&buf[i..]) {
                Ok((inst, size)) => {
                    program.push(inst);
                    i += size;
                }
                Err(e) => return Err(Error::new(ErrorKind::InvalidData, format!("{:?}", e)))
            }
        }
        Ok(program)
    }

    pub fn translate_masm(file_path: &str) -> std::io::Result<Vec::<Inst>> {
        use std::{fs::read_to_string, convert::TryFrom};

        let ret = read_to_string(file_path)?
            .lines()
            .filter(|l| !l.starts_with(";") || !l.is_empty())
            .map(Inst::try_from)
            .filter_map(Result::ok)
            .collect::<Vec::<Inst>>();

        Ok(ret)
    }
}
