#[derive(PartialEq)]
pub struct Token<'input, 'rule> {
    pub rule: &'rule str,
    pub input: &'input str,
    pub children: Option<Box<[Token<'input, 'rule>]>>,
}
impl<'input, 'rule> Token<'input, 'rule> {
    pub fn children(&self) -> &[Token<'input, 'rule>] {
        self.children.as_ref().map(AsRef::as_ref).unwrap_or(&[])
    }
}

use std::fmt::{Debug, Display};

impl<'input, 'rule> Display for Token<'input, 'rule> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(children) = &self.children {
            let children = children
                .into_iter()
                .map(|c| c.to_string())
                .collect::<Vec<String>>()
                .join(", ");
            write!(f, "{} [{}] -> \"{}\"", self.rule, children, self.input)
        } else {
            write!(f, "{} -> \"{}\"", self.rule, self.input)
        }
    }
}

impl<'input, 'rule> Debug for Token<'input, 'rule> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
