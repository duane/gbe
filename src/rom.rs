pub struct ROM {
    pub buf: Vec<u8>,
}

impl ROM {
    pub fn from_buf(buf: Vec<u8>) -> Self {
        Self { buf }
    }
}
