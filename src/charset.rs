use std::fmt::{Debug, Display};

#[derive(PartialEq, Clone)]
pub enum CharSet {
    Char(char),
    Range((u16, u16)),
    Any,
    Alpha(bool),
    AlphaNum(bool),
    Digits(bool),
    Spaces(bool),
}

impl CharSet {
    pub fn matches(&self, c: char) -> bool {
        match self {
            CharSet::Char(c2) => c == *c2,
            CharSet::Range(x) => {
                let c = c as u32;
                c >= x.0 as u32 && c <= x.1 as u32
            }
            CharSet::Any => true,
            CharSet::AlphaNum(v) => c.is_alphanumeric() == *v,
            CharSet::Alpha(v) => c.is_alphabetic() == *v,
            CharSet::Digits(v) => c.is_digit(10) == *v,
            CharSet::Spaces(v) => c.is_whitespace() == *v,
        }
    }
}

impl Display for CharSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CharSet::Char(c) => write!(f, "{}", c),
            CharSet::Range(x) => write!(f, "{}-{}", x.0, x.1),
            CharSet::Any => write!(f, "."),
            CharSet::Alpha(true) => write!(f, "\\a"),
            CharSet::Alpha(false) => write!(f, "\\A"),
            CharSet::AlphaNum(true) => write!(f, "\\w"),
            CharSet::AlphaNum(false) => write!(f, "\\W"),
            CharSet::Digits(true) => write!(f, "\\d"),
            CharSet::Digits(false) => write!(f, "\\D"),
            CharSet::Spaces(true) => write!(f, "\\s"),
            CharSet::Spaces(false) => write!(f, "\\S"),
        }
    }
}

impl Debug for CharSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[test]
fn charset_mem_size() {
    assert_eq!(std::mem::size_of::<CharSet>(), 8);
}
