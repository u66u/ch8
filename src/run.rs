use crate::cpu::{ C8Kernel, Reg, V0, addr, byte };
use crate::{ utils::*, match_input };
use crate::memory::Memory;
use std::fs::File;
use std::io::{ Read, Write, stdout };
use std::sync::mpsc::TryRecvError;
use std::sync::{ Arc, atomic::Ordering, mpsc::{ self, Receiver, Sender } };
use std::thread;
use std::time::{ Duration, Instant };
use argh::FromArgs;
use rand::prelude::*;
use crossterm::{
    cursor::{ self },
    event::{ self, KeyCode, KeyModifiers },
    style::ResetColor,
    terminal::{ self, EnterAlternateScreen, LeaveAlternateScreen },
    QueueableCommand,
};

struct Instruction {
    op: C8Kernel,
    x: usize,
    y: usize,
    n: usize,
    kk: u8,
    nnn: u16,
}

impl Instruction {} // TODO

// instructions
impl C8Kernel {
    /// Set register `Vx` to `b`.
    pub fn ld_vx_bb(&mut self, x: Reg, b: u8) {
        self.reg[x] = b;
    }

    /// Add the value `b` to register `Vx`.
    pub fn add_vx_bb(&mut self, x: Reg, b: u8) {
        self.reg[x] += b;
    }

    /// Set register `Vx` to the value of register `Vy`.
    pub fn ld_vx_vy(&mut self, x: Reg, y: Reg) {
        self.reg[x] = self.reg[y];
    }

    /// Set register `Vx` to `Vx` OR `Vy`.
    pub fn or_vx_vy(&mut self, x: Reg, y: Reg) {
        self.reg[x] |= self.reg[y];
    }

    /// Set register `Vx` to `Vx` AND `Vy`.
    pub fn and_vx_vy(&mut self, x: Reg, y: Reg) {
        self.reg[x] &= self.reg[y];
    }

    /// Set register `Vx` to `Vx` XOR `Vy`.
    pub fn xor_vx_vy(&mut self, x: Reg, y: Reg) {
        self.reg[x] ^= self.reg[y];
    }

    /// Add the value of register `Vy` to register `Vx`. Set the flag register to 01 if a carry occurs.
    pub fn add_vx_vy(&mut self, x: Reg, y: Reg) {
        let ret = self.reg[x] + self.reg[y];
        self.reg.set_flag(ret < self.reg[x]); // Check for overflow/carry
        self.reg[x] = ret;
    }

    /// Subtract the value of register `Vy` from register `Vx`. Set the flag register to 01 if `Vx` is greater than `Vy`.
    pub fn sub_vx_vy(&mut self, x: Reg, y: Reg) {
        let ret = self.reg[x] - self.reg[y];
        self.reg.set_flag(self.reg[x] > self.reg[y]); // Check for underflow/not borrow
        self.reg[x] = ret;
    }

    /// Set register `Vx` to the result of a bitwise AND on a random number and `b`.
    pub fn rnd_vx_bb(&mut self, x: Reg, b: u8) {
        self.reg[x] = self.rng.gen::<u8>() & b;
    }

    /// Draw a sprite at coordinate `(Vx, Vy)` with `n` bytes of sprite data starting at the memory pointer `I`.
    /// Set the flag register to 01 if any set pixels are changed to unset, and to 00 otherwise.
    pub fn drw_vx_vy_n(&mut self, x: Reg, y: Reg, n: u8) {
        let flag = self.buf.draw_sprite(
            self.reg[x] & ((SCREEN_WIDTH as u8) - 1),
            self.reg[y] & ((SCREEN_HEIGHT as u8) - 1),
            self.mem.get_slice(n)
        );
        self.reg.set_flag(flag); // Set collision flag
    }

    /// Skip the next instruction if the key corresponding to the value of register `Vx` is pressed.
    pub fn skp_vx(&mut self, x: Reg) {
        if self.input.is_pressed(x) {
            self.pc += 2;
        }
    }

    /// Skip the next instruction if the key corresponding to the value of register `Vx` is not pressed.
    pub fn sknp_vx(&mut self, x: Reg) {
        if !self.input.is_pressed(x) {
            self.pc += 2;
        }
    }

    /// Set register `Vx` to the value of the delay timer.
    pub fn ld_vx_dt(&mut self, x: Reg) {
        self.reg[x] = self.timers.dt.load(Ordering::SeqCst);
    }

    /// Wait for a key press and then store the result in register `Vx`. If no key is pressed, repeat the instruction.
    pub fn ld_vx_k(&mut self, x: Reg) {
        if let Some(key) = self.input.try_get_key() {
            self.reg[x] = key;
        } else {
            self.pc -= 2; // Repeat the instruction if no key is pressed
        }
    }

    /// Set the delay timer to the value of register `Vx`.
    pub fn ld_dt_vx(&mut self, x: Reg) {
        self.timers.dt.store(self.reg[x], Ordering::SeqCst);
    }

    /// Set the sound timer to the value of register `Vx`.
    pub fn ld_st_vx(&mut self, x: Reg) {
        self.timers.st.store(self.reg[x], Ordering::SeqCst);
    }

    /// Add the value of register `Vx` to the memory pointer `I`.
    pub fn add_i_vx(&mut self, x: Reg) {
        self.mem.ptr += self.reg[x] as u16;
    }

    /// Set `I` to the location of the sprite for the character in `Vx`. Characters 0-F are represented by a 4x5 font.
    pub fn ld_f_vx(&mut self, x: Reg) {
        self.mem.ptr = (FONT_START as u16) + (self.reg[x] as u16) * (FONT_SIZE as u16);
    }

    /// Store the BCD representation of register `Vx` in memory locations `I`, `I+1`, and `I+2`.
    pub fn ld_b_vx(&mut self, x: Reg) {
        let val = self.reg[x];
        *self.mem.get_offset_mut(0u8) = val / 100;
        *self.mem.get_offset_mut(1u8) = (val / 10) % 10;
        *self.mem.get_offset_mut(2u8) = val % 10;
    }

    /// Store registers `V0` through `Vx` in memory starting at location `I`.
    pub fn ld_i_vx(&mut self, x: Reg) {
        for i in 0..=x {
            *self.mem.get_offset_mut(i) = self.reg[i];
        }
    }

    /// Clear the display buffer.
    pub fn cls(&mut self) {
        self.buf.clear();
    }

    /// Return from a subroutine. Set the program counter to the address at the top of the stack.
    pub fn ret(&mut self) {
        self.pc = self.stack.pop();
    }

    /// This is a legacy Chip-8 instruction not used in most modern programs.
    pub fn sys_aaa(&mut self, _a: u16) {}

    /// Jump to address `a`.
    pub fn jp_aaa(&mut self, a: u16) {
        self.pc = a;
    }

    /// Call subroutine at address `a`. The current program counter is pushed onto the stack.
    pub fn call_aaa(&mut self, a: u16) {
        self.stack.push(self.pc);
        self.pc = a;
    }

    /// Skip the next instruction if register `Vx` equals `b`.
    pub fn se_vx_bb(&mut self, x: Reg, b: u8) {
        if self.reg[x] == b {
            self.pc += 2;
        }
    }

    /// Skip the next instruction if register `Vx` does not equal `b`.
    pub fn sne_vx_bb(&mut self, x: Reg, b: u8) {
        if self.reg[x] != b {
            self.pc += 2;
        }
    }

    /// Skip the next instruction if register `Vx` equals register `Vy`.
    pub fn se_vx_vy(&mut self, x: Reg, y: Reg) {
        if self.reg[x] == self.reg[y] {
            self.pc += 2;
        }
    }

    /// Read registers `V0` through `Vx` from memory starting at location `I`.
    pub fn ld_vx_i(&mut self, x: Reg) {
        for i in 0..=x {
            self.reg[i] = *self.mem.get_offset(i);
        }
    }

    /// Store the least significant bit of `Vx` in the flag register and then shift `Vx` to the right by 1.
    pub fn shr_vx_vy(&mut self, x: Reg, _y: Reg) {
        self.reg.set_flag((self.reg[x] & 1) == 1); // Check the least significant bit
        self.reg[x] >>= 1;
    }

    /// Set register `Vx` to the value of `Vy` minus `Vx`. Set flag register to 01 if `Vy` is greater than `Vx`.
    pub fn subn_vx_vy(&mut self, x: Reg, y: Reg) {
        let ret = self.reg[y] - self.reg[x];
        self.reg.set_flag(self.reg[y] > self.reg[x]); // Check for not borrow
        self.reg[x] = ret;
    }

    /// Store the most significant bit of `Vx` in the flag register and then shift `Vx` to the left by 1.
    pub fn shl_vx_vy(&mut self, x: Reg, _y: Reg) {
        self.reg.set_flag((self.reg[x] & 0b10000000) == 0b10000000); // Check the most significant bit
        self.reg[x] <<= 1;
    }

    /// Skip the next instruction if register `Vx` does not equal register `Vy`.
    pub fn sne_vx_vy(&mut self, x: Reg, y: Reg) {
        if self.reg[x] != self.reg[y] {
            self.pc += 2;
        }
    }

    /// Set the memory pointer `I` to the address `a`.
    pub fn ld_i_aaa(&mut self, a: u16) {
        self.mem.ptr = a;
    }

    /// Jump to the address `a` plus the value of register `V0`.
    pub fn jp_v0_aaa(&mut self, a: u16) {
        self.pc = (self.reg[V0] as u16) + a;
    }
}

pub enum HaltSignal {
    Halt,
}
pub enum InputSignal {
    Exit,
}
pub enum ExitKind {
    Forced,
    Natural,
}

pub struct ChannelHandler {
    timer_tx: Sender<HaltSignal>,
    input_tx: Sender<HaltSignal>,
    input_rx: Receiver<InputSignal>,
}

impl ChannelHandler {
    pub fn halt(&self) -> CustomResult<()> {
        for tx in &[&self.input_tx, &self.timer_tx] {
            tx.send(HaltSignal::Halt).c8_err("Could not send message to thread")?;
        }
        Ok(())
    }
}

impl C8Kernel {
    pub fn new(rom: &[u8]) -> CustomResult<Self> {
        Ok(Self {
            mem: Memory::from_rom(rom)?,
            rng: thread_rng(),
            pc: PROGRAM_START as u16,
            ..Default::default()
        })
    }
    pub fn run_from_args(args: Args) -> CustomResult<ExitKind> {
        let executor = if args.hex { Self::run_hex } else { Self::run_binary };
        let mut buf = vec![];
        File::open(args.file)
            .c8_err("Error occurred trying to open the file")?
            .read_to_end(&mut buf)
            .c8_err("Error occurred trying to read the file")?;
        executor(&buf)
    }
    pub fn run_hex(rom: &[u8]) -> CustomResult<ExitKind> {
        let mut bin = vec![];
        let hex = |c: u8| {
            match c {
                n @ b'0'..=b'9' => Ok(n - b'0'),
                x @ b'a'..=b'f' => Ok(x - b'a' + 10),
                x @ b'A'..=b'F' => Ok(x - b'a' + 10),
                n => Err(format!("Byte {} in input is not a hex character", n)),
            }
        };
        if (rom.len() & 1) == 1 {
            return Err(String::from("The program has an odd number of hex digits."));
        }
        for chunk in rom.chunks(2) {
            let h = chunk[0];
            let l = chunk[1];
            bin.push((hex(h)? << 4) | hex(l)?);
        }
        let mut machine = Self::new(rom)?;
        machine.run()
    }
    pub fn run_binary(rom: &[u8]) -> CustomResult<ExitKind> {
        let mut machine = Self::new(rom)?;
        machine.run()
    }
    pub fn setup_terminal(&self) -> CustomResult<()> {
        let mut stdout = stdout();
        stdout
            .queue(cursor::Hide)
            .c8_err("Could not hide cursor")?
            .queue(EnterAlternateScreen)
            .c8_err("Could not enter alternate screen")?
            .flush()
            .c8_err("Could not flush stdout")?;

        terminal::enable_raw_mode().c8_err("Could not enable terminal raw mode")?;

        Ok(())
    }
    pub fn teardown_terminal(&self) -> CustomResult<()> {
        terminal::disable_raw_mode().c8_err("Could not disable terminal raw mode")?;
        let mut stdout = stdout();
        stdout
            .queue(LeaveAlternateScreen)
            .c8_err("Could not leave alternate screen")?
            .queue(cursor::Show)
            .c8_err("Could not show cursor")?
            .queue(ResetColor)
            .c8_err("Could not reset colors")?
            .flush()
            .c8_err("Could not flush stdout")?;

        Ok(())
    }
    pub fn spawn_extra_threads(&self) -> CustomResult<ChannelHandler> {
        let timer_tx = self.spawn_timer()?;
        let (display_rx, input_tx) = self.spawn_display()?;
        Ok(ChannelHandler {
            timer_tx,
            input_tx,
            input_rx: display_rx,
        })
    }
    pub fn spawn_timer(&self) -> CustomResult<Sender<HaltSignal>> {
        let dt = Arc::clone(&self.timers.dt);
        let st = Arc::clone(&self.timers.st);
        let (timer_tx, timer_rx) = mpsc::channel();
        thread::spawn(move || {
            loop {
                match timer_rx.try_recv() {
                    Ok(HaltSignal::Halt) | Err(TryRecvError::Disconnected) => {
                        break;
                    }
                    Err(TryRecvError::Empty) => (),
                }
                // Result is ignored, since we don't care about the
                // registers when they're zero
                let _ = dt.fetch_update(Ordering::SeqCst, Ordering::SeqCst, |n| {
                    if n != 0 { Some(n - 1) } else { None }
                });
                let _ = st.fetch_update(Ordering::SeqCst, Ordering::SeqCst, |n| {
                    if n != 0 { Some(n - 1) } else { None }
                });
                thread::sleep(Duration::from_secs_f32(1.0 / TIMER_TICK_RATE_HZ));
            }
        });
        Ok(timer_tx)
    }
    pub fn spawn_display(&self) -> CustomResult<(Receiver<InputSignal>, Sender<HaltSignal>)> {
        let input = Arc::clone(&self.input.bits);
        let (display_tx, display_rx) = mpsc::channel();
        let (input_tx, input_rx) = mpsc::channel();
        thread::spawn(move || {
            loop {
                let mut starts: [Option<Instant>; 16] = [None; 16];
                match input_rx.try_recv() {
                    Ok(HaltSignal::Halt) | Err(TryRecvError::Disconnected) => {
                        break;
                    }
                    Err(TryRecvError::Empty) => (),
                }
                let next_timeout = starts
                    .iter()
                    .flatten()
                    .min()
                    .map_or(INPUT_TIMEOUT, |inst| inst.duration_since(Instant::now()));
                let result = event::poll(next_timeout);
                starts = {
                    let mut new = [None; 16];
                    for (i, &start) in starts.iter().enumerate() {
                        new[i] = if let Some(inst) = start {
                            if inst <= Instant::now() { None } else { Some(inst) }
                        } else {
                            None
                        };
                    }
                    new
                };
                if let Ok(true) = result {
                    if let Ok(event::Event::Key(k)) = event::read() {
                        if let KeyModifiers::NONE = k.modifiers {
                            match_input! {
                                k => |i| {starts[i] = Some(Instant::now() + INPUT_TIMEOUT)}
                            }
                        } else {
                            match k.code {
                                KeyCode::Char('c') => {
                                    display_tx.send(InputSignal::Exit).unwrap();
                                    // prevents race conditions as the thread can be killed at any point after this line
                                    thread::sleep(Duration::from_secs(300));
                                }
                                _ => (),
                            }
                        }
                    }
                }
                let mut key_state = 0u16;
                for i in 0..16 {
                    key_state |= (starts[i].is_some() as u16) << i;
                }
                input.store(key_state, Ordering::SeqCst);
            }
        });
        Ok((display_rx, input_tx))
    }
    pub fn run(&mut self) -> CustomResult<ExitKind> {
        self.setup_terminal()?;
        let channels = self.spawn_extra_threads()?;
        self.buf.clear();
        loop {
            match channels.input_rx.try_recv() {
                Ok(InputSignal::Exit) => {
                    self.teardown_terminal()?;
                    channels.halt()?;
                    return Ok(ExitKind::Forced);
                }
                _ => (),
            }
            if self.pc >= (MEM_SIZE as u16) {
                self.teardown_terminal()?;
                channels.halt()?;
                return Ok(ExitKind::Natural);
            }
            let high = *self.mem.get(self.pc);
            let low = *self.mem.get(self.pc + 1);
            self.pc += 2;
            self.execute(high, low);
        }
    }
    pub fn execute(&mut self, high: u8, low: u8) {
        let h_h = high >> 4;
        let h_l = high & 0b00001111;
        let l_h = low >> 4;
        let l_l = low & 0b00001111;
        // uh oh
        match (h_h, h_l, l_h, l_l) {
            (0, 0, 0xe, 0) => self.cls(),
            (0, 0, 0xe, 0xe) => self.ret(),
            (0, a_0, a_1, a_2) => self.sys_aaa(addr(a_0, a_1, a_2)),
            (1, a_0, a_1, a_2) => self.jp_aaa(addr(a_0, a_1, a_2)),
            (2, a_0, a_1, a_2) => self.call_aaa(addr(a_0, a_1, a_2)),
            (3, x, b_0, b_1) => self.se_vx_bb(x, byte(b_0, b_1)),
            (4, x, b_0, b_1) => self.sne_vx_bb(x, byte(b_0, b_1)),
            (5, x, y, 0) => self.se_vx_vy(x, y),
            (6, x, b_0, b_1) => self.ld_vx_bb(x, byte(b_0, b_1)),
            (7, x, b_0, b_1) => self.add_vx_bb(x, byte(b_0, b_1)),
            (8, x, y, 0) => self.ld_vx_vy(x, y),
            (8, x, y, 1) => self.or_vx_vy(x, y),
            (8, x, y, 2) => self.and_vx_vy(x, y),
            (8, x, y, 3) => self.xor_vx_vy(x, y),
            (8, x, y, 4) => self.add_vx_vy(x, y),
            (8, x, y, 5) => self.sub_vx_vy(x, y),
            (8, x, y, 6) => self.shr_vx_vy(x, y),
            (8, x, y, 7) => self.subn_vx_vy(x, y),
            (8, x, y, 0xe) => self.shl_vx_vy(x, y),
            (9, x, y, 0) => self.sne_vx_vy(x, y),
            (0xa, a_0, a_1, a_2) => self.ld_i_aaa(addr(a_0, a_1, a_2)),
            (0xb, a_0, a_1, a_2) => self.jp_v0_aaa(addr(a_0, a_1, a_2)),
            (0xc, x, b_0, b_1) => self.rnd_vx_bb(x, byte(b_0, b_1)),
            (0xd, x, y, n) => self.drw_vx_vy_n(x, y, n),
            (0xe, x, 8, 0xe) => self.skp_vx(x),
            (0xe, x, 0xa, 1) => self.sknp_vx(x),
            (0xf, x, 0, 7) => self.ld_vx_dt(x),
            (0xf, x, 0, 0xa) => self.ld_vx_k(x),
            (0xf, x, 1, 5) => self.ld_dt_vx(x),
            (0xf, x, 1, 8) => self.ld_st_vx(x),
            (0xf, x, 1, 0xe) => self.add_i_vx(x),
            (0xf, x, 2, 9) => self.ld_f_vx(x),
            (0xf, x, 3, 3) => self.ld_b_vx(x),
            (0xf, x, 5, 5) => self.ld_i_vx(x),
            (0xf, x, 6, 5) => self.ld_vx_i(x),
            _ => {}
        }
    }
}

#[derive(FromArgs)]
#[argh(description = "chip8 emulator arguments")]
pub struct Args {
    /// parse hex file into binary
    #[argh(switch, short = 'x')]
    pub hex: bool,
    /// the file to execute
    #[argh(positional)]
    pub file: String,
}
