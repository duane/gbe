pub struct SerialOut {
    buf: u8,
}

impl SerialOut {
    pub fn new() -> SerialOut {
        SerialOut { buf: 0 }
    }

    pub fn buffer(&mut self, data: u8) {
        self.buf = data;
    }

    pub fn control(&mut self, _data: u8) {
        print!("{}", self.buf as char);
    }
}
