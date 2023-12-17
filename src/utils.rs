use std::{ time::Duration, fmt::Display, fmt::Debug };
use crossterm::style::Color;

// No magic numbers :)
pub const PIXEL_EMPTY: &str = " ";
pub const PIXEL_HALF: &str = "\u{2584}";
pub const SCREEN_ON_COLOR: Color = Color::White;
pub const SCREEN_OFF_COLOR: Color = Color::DarkGrey;
pub const TIMER_TICK_RATE_HZ: f32 = 60.0;
pub const INPUT_TIMEOUT: Duration = Duration::from_millis(17);
pub const MEM_SIZE: usize = 0x1000;
pub const FONT_START: usize = 0x000;
pub const FONT_SIZE: usize = 5;
pub const PROGRAM_START: usize = 0x200;
pub const STACK_DEPTH: usize = 32;
pub const SPRITE_WIDTH: u8 = 8;
pub const SCREEN_HEIGHT: usize = 32;
pub const SCREEN_HEIGHT_TERMINAL_UNITS: usize = 16;
pub const SCREEN_WIDTH: usize = 64;
pub const SCREEN_OFFSET_X: u16 = 0;
pub const SCREEN_OFFSET_Y: u16 = 0;

// Type aliases
pub type CustomResult<T> = Result<T, String>;

pub trait IntoCustomResult<T, M: Display> {
    // just for convenience
    fn c8_err(self, msg: M) -> CustomResult<T>;
}

impl<T, E: Debug, M: Display> IntoCustomResult<T, M> for Result<T, E> {
    fn c8_err(self, msg: M) -> CustomResult<T> {
        self.map_err(|e| format!("{}\nerror:\n{:?}", msg, e))
    }
}

#[macro_export]
macro_rules! match_input {
    ($target:expr => $result:expr) => {
        match_input!(@inner $target; $result; 'x'0'1'1'2'2'3'3'q'4'w'5'e'6'a'7's'8'd'9'z'10'c'11'4'12'r'13'f'14'v'15);
    };
    (@inner $target:expr; $result:expr; $($chr:literal $idx:literal)*) => {
        match ($target).code {
            $(
                KeyCode::Char($chr) => {($result)($idx)},
            )*
            _ => ()
        }
    };
}
