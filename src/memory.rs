use crate::utils::{ MEM_SIZE, PROGRAM_START, CustomResult };

#[derive(Debug)]
pub struct Memory {
    pub data: [u8; MEM_SIZE],
    pub ptr: u16,
}

impl Default for Memory {
    fn default() -> Self {
        Self {
            data: [0; MEM_SIZE],
            ptr: PROGRAM_START as u16,
        }
    }
}

impl Memory {
    pub fn from_rom(rom: &[u8]) -> CustomResult<Self> {
        if PROGRAM_START + rom.len() > MEM_SIZE {
            Err(format!("ROM size 0x{:x} exceeds available memory space.", rom.len()))
        } else {
            let mut data = [0u8; MEM_SIZE];
            data[PROGRAM_START..PROGRAM_START + rom.len()].copy_from_slice(rom);

            Ok(Self {
                data,
                ptr: PROGRAM_START as u16,
            })
        }
    }

    pub fn get<N: Into<usize>>(&self, addr: N) -> &u8 {
        &self.data[addr.into()]
    }

    pub fn get_offset<N: Into<usize>>(&self, offset: N) -> &u8 {
        &self.data[(self.ptr as usize).wrapping_add(offset.into())]
    }

    pub fn get_offset_mut<N: Into<usize>>(&mut self, offset: N) -> &mut u8 {
        &mut self.data[(self.ptr as usize).wrapping_add(offset.into())]
    }

    pub fn get_slice<N: Into<usize>>(&self, length: N) -> &[u8] {
        let start = self.ptr as usize;
        let end = start.wrapping_add(length.into());
        &self.data[start..end]
    }
}
