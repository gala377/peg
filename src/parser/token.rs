

pub trait Token: Clone {
    const EOF: Self;
}

impl Token for char {
    const EOF: Self = 0x05 as char;
}
