pub struct SerialOut {
    buffer: u8,
    control: u8,
}

impl SerialOut {
    pub fn new() -> SerialOut {
        SerialOut {
            buffer: 0,
            control: 1,
        }
    }

    pub fn set_buffer(&mut self, data: u8) {
        self.buffer = data;
    }

    pub fn get_buffer(&self) -> u8 {
        self.buffer
    }

    pub fn set_control(&mut self, data: u8) {
        if data == 0x81 {
            print!("{}", self.buffer as char);
            self.control = 0;
        }
    }

    pub fn get_control(&self) -> u8 {
        self.control
    }
}
