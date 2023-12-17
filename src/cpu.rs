use crate::{ utils::*, memory::Memory };
use crossterm::QueueableCommand;
use rand::rngs::ThreadRng;
use std::{
    ops::{ Index, IndexMut },
    sync::{ Arc, atomic::{ AtomicU8, AtomicU16, Ordering } },
    io::{ Stdout, stdout, Write },
};
use crossterm::{ cursor::MoveTo, style::{ Colors, Print, SetBackgroundColor, SetColors } };
#[derive(Debug, Default)]
pub struct Registers([u8; 16]);

pub type Reg = u8;
pub const V0: Reg = 0x0;
pub const VF: Reg = 0xf;

impl Index<Reg> for Registers {
    type Output = u8;

    fn index(&self, index: Reg) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl IndexMut<Reg> for Registers {
    fn index_mut(&mut self, index: Reg) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

impl Registers {
    pub fn new() -> Self {
        Registers([0; 16])
    }

    pub fn set_flag(&mut self, flag: bool) {
        self[VF] = flag as u8;
    }
}
#[derive(Debug, Default)]
pub struct Stack {
    data: [u16; STACK_DEPTH],
    ptr: usize,
}

impl Stack {
    pub fn new() -> Self {
        Stack { data: [0; STACK_DEPTH], ptr: 0 }
    }

    pub fn push(&mut self, addr: u16) -> bool {
        if self.ptr < STACK_DEPTH {
            self.data[self.ptr] = addr;
            self.ptr += 1;
            true
        } else {
            false
        }
    }

    pub fn pop(&mut self) -> u16 {
        let val = self.data[self.ptr as usize];
        self.ptr -= 1;
        val
    }
}

#[derive(Debug, Default)]
pub struct Timers {
    pub dt: Arc<AtomicU8>,
    pub st: Arc<AtomicU8>,
}

impl Timers {
    pub fn new() -> Self {
        Timers {
            dt: Arc::new(AtomicU8::new(0)),
            st: Arc::new(AtomicU8::new(0)),
        }
    }
}
#[derive(Debug)]
pub struct Framebuffer {
    pub data: [u64; SCREEN_HEIGHT],
    pub stream: Stdout,
}

impl Default for Framebuffer {
    fn default() -> Self {
        Framebuffer {
            data: [0; SCREEN_HEIGHT],
            stream: stdout(),
        }
    }
}

impl Framebuffer {
    pub fn draw_slice(&mut self, top: u8, bottom: u8, x: u8) {
        for i in 0..((SCREEN_WIDTH as u8) - x).min(SPRITE_WIDTH) {
            let top_px = (top & (1 << i)) != 0;
            let bottom_px = (bottom & (1 << i)) != 0;
            if top_px ^ bottom_px {
                self.stream
                    .queue(
                        SetColors(
                            Colors::new(
                                if bottom_px {
                                    SCREEN_ON_COLOR
                                } else {
                                    SCREEN_OFF_COLOR
                                },
                                if top_px {
                                    SCREEN_ON_COLOR
                                } else {
                                    SCREEN_OFF_COLOR
                                }
                            )
                        )
                    )
                    .unwrap();
                self.stream.queue(Print(PIXEL_HALF)).unwrap();
            } else {
                self.stream
                    .queue(
                        SetBackgroundColor(if top_px { SCREEN_ON_COLOR } else { SCREEN_OFF_COLOR })
                    )
                    .unwrap();
                self.stream.queue(Print(PIXEL_EMPTY)).unwrap();
            }
        }
    }
    pub fn move_to(&mut self, x: u8, y: u8) {
        self.stream
            .queue(MoveTo(SCREEN_OFFSET_X + (x as u16), SCREEN_OFFSET_Y + (y as u16) / 2))
            .unwrap();
    }
    pub fn udpate(&mut self, slice: u8, x: u8, y: u8) -> (bool, u8) {
        let orig = self.data[y as usize];
        let shifted = (slice as u64) << (x as u64);
        let result = orig ^ shifted;
        self.data[y as usize] = result;
        ((orig & !result) != 0, (result >> (x as u64)) as u8)
    }
    pub fn draw_sprite(&mut self, x: u8, y: u8, sprite: &[u8]) -> bool {
        let x_pos = x % (SCREEN_WIDTH as u8);
        let y_pos = y % (SCREEN_HEIGHT as u8);
        let mut flag = false;
        let align_offset = y_pos & 1;
        if align_offset != 0 {
            let top = (self.data[(y_pos as usize) - 1] >> (x_pos as u64)) as u8;
            let (f, bottom) = self.udpate(sprite[0].reverse_bits(), x_pos, y_pos);
            flag |= f;
            self.move_to(x_pos, y_pos - 1);
            self.draw_slice(top, bottom, x_pos);
        }
        for (i, pair) in sprite[align_offset as usize..].chunks(2).enumerate() {
            if let &[top, bottom] = pair {
                let y_offset = y_pos + align_offset + 2 * (i as u8);
                let (f_t, top) = self.udpate(top.reverse_bits(), x_pos, y_offset);
                let (f_b, bottom) = self.udpate(bottom.reverse_bits(), x_pos, y_offset + 1);
                flag |= f_t | f_b;
                self.move_to(x_pos, y_offset);
                self.draw_slice(top, bottom, x_pos);
            } else if let &[top] = pair {
                let y_offset = y_pos + align_offset + 2 * (i as u8);
                let (f, top) = self.udpate(top.reverse_bits(), x_pos, y_offset);
                flag |= f;
                let bottom = (self.data[(y_offset as usize) + 1] >> (x_pos as u64)) as u8;
                self.move_to(x_pos, y_offset);
                self.draw_slice(top, bottom, x_pos);
            }
        }
        self.stream.flush().unwrap();
        flag
    }
    pub fn clear(&mut self) {
        self.data = [0; SCREEN_HEIGHT];
        for y in 0..SCREEN_HEIGHT_TERMINAL_UNITS {
            self.stream.queue(MoveTo(SCREEN_OFFSET_X, SCREEN_OFFSET_Y + (y as u16))).unwrap();
            self.stream.queue(SetBackgroundColor(SCREEN_OFF_COLOR)).unwrap();
            let empty_row = PIXEL_EMPTY.repeat(SCREEN_WIDTH);
            self.stream.queue(Print(empty_row)).unwrap();
        }
        self.stream.flush().unwrap();
    }
}

#[derive(Debug, Default)]
pub struct Input {
    pub bits: Arc<AtomicU16>,
}

impl Input {
    pub fn is_pressed(&self, x: u8) -> bool {
        (self.bits.load(Ordering::SeqCst) & (1 << x)) != 0
    }
    pub fn try_get_key(&self) -> Option<u8> {
        let bits = self.bits.load(Ordering::SeqCst);
        if bits != 0 {
            Some(bits.trailing_zeros() as u8)
        } else {
            None
        }
    }
}

#[derive(Debug, Default)]
pub struct C8Kernel {
    pub mem: Memory,
    pub reg: Registers,
    pub pc: u16,
    pub stack: Stack,
    pub buf: Framebuffer,
    pub timers: Timers,
    pub rng: ThreadRng,
    pub input: Input,
}

pub fn byte(b_0: u8, b_1: u8) -> u8 {
    (b_0 << 4) | b_1
}
pub fn addr(a_0: u8, a_1: u8, a_2: u8) -> u16 {
    ((a_0 as u16) << 8) | ((a_1 as u16) << 4) | (a_2 as u16)
}
