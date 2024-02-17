pub struct HeadlessAudio {
    pub data: u8,
}

impl HeadlessAudio {
    pub fn new() -> Self {
        Self { data: 0b1000_1111 }
    }

    pub fn set(&mut self, data: u8) {
        self.data = data;
    }

    pub fn read(&self) -> u8 {
        self.data
    }
}
