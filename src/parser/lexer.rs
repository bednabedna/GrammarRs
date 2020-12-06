use std::str::Chars;

pub struct Lexer<'input> {
    input: Chars<'input>,
    stack: Vec<*const u8>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            input: input.chars(),
            stack: Vec::with_capacity(16),
        }
    }

    pub fn push_state(&mut self) {
        self.stack.push(self.input.as_str().as_ptr());
    }

    unsafe fn extend_front_of_slice<'a>(front_ptr: *const u8, rest: &'a str) -> &'a str {
        std::mem::transmute(std::slice::from_raw_parts(
            front_ptr,
            rest.as_ptr().sub(front_ptr as usize) as usize + rest.len(),
        ))
    }

    pub fn rollback_state(&mut self) {
        self.input = unsafe {
            Lexer::extend_front_of_slice(
                self.stack
                    .pop()
                    .expect("attempt to rollback state of lexer with empty stack"),
                self.input.as_str(),
            )
        }
        .chars();
    }

    pub fn previous_state(&self) -> Chars<'input> {
        unsafe {
            Lexer::extend_front_of_slice(
                *self
                    .stack
                    .last()
                    .expect("attempt to get last state of lexer with empty stack"),
                self.input.as_str(),
            )
        }
        .chars()
    }

    pub fn peek_word(&self) -> &'input str {
        if let Some(&prev_ptr) = self.stack.last() {
            unsafe {
                std::mem::transmute(std::slice::from_raw_parts(
                    prev_ptr,
                    self.input.as_str().as_ptr().sub(prev_ptr as usize) as usize,
                ))
            }
        } else {
            ""
        }
    }

    pub fn pop_state(&mut self) {
        self.stack.pop();
    }

    pub fn peek_char(&mut self) -> Option<char> {
        self.input.clone().next()
    }

    pub fn next_char(&mut self) -> Option<char> {
        self.input.next()
    }

    pub fn rewind_char(&mut self) {
        let mut word_chars = self.peek_word().chars();
        word_chars.next_back();
        self.input = self.previous_state().as_str()[word_chars.as_str().len()..].chars();
    }

    pub fn consume_char(&mut self, c: char) -> bool {
        if let Some(next_char) = self.peek_char() {
            if next_char == c {
                self.next_char();
                return true;
            }
        }
        false
    }

    pub fn consume_word(&mut self, word: &str) -> bool {
        let input = self.input();
        let consume = input.starts_with(word);
        if consume {
            self.input = input[word.len()..].chars();
        }
        consume
    }

    pub fn has_input(&self) -> bool {
        !self.input.as_str().is_empty()
    }

    pub fn input(&self) -> &'input str {
        self.input.as_str()
    }

    pub fn consume_one<F>(&mut self, predicate: F) -> bool
    where
        F: FnOnce(char) -> bool,
    {
        if let Some(c) = self.peek_char() {
            if predicate(c) {
                self.next_char();
                return true;
            }
        }
        false
    }

    pub fn consume_all<F>(&mut self, predicate: F)
    where
        F: Fn(char) -> bool,
    {
        while let Some(c) = self.peek_char() {
            if !predicate(c) {
                break;
            }
            self.next_char();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn peek_char() {
        let mut lexer = Lexer::new("abc");
        assert_eq!(lexer.peek_char(), Some('a'));
        assert_eq!(lexer.next_char(), Some('a'));
        assert_eq!(lexer.peek_char(), Some('b'));
        assert_eq!(lexer.next_char(), Some('b'));
        assert_eq!(lexer.peek_char(), Some('c'));
        assert_eq!(lexer.next_char(), Some('c'));
        assert_eq!(lexer.peek_char(), None);
        assert_eq!(lexer.next_char(), None);
    }

    fn test_consume<'input, F, T>(input: &'input str, mut f: F, ex_patt: T, ex_rem: &str)
    where
        T: std::fmt::Debug + PartialEq,
        F: FnMut(&mut Lexer<'input>) -> Option<T>,
    {
        let mut l = Lexer::new(input);
        assert_eq!((f(&mut l), l.input()), (Some(ex_patt), ex_rem));
    }

    #[test]
    fn consume_all_1() {
        test_consume(
            "abc",
            |l| Some(l.consume_all(|c| c.is_alphabetic())),
            (),
            "",
        );
    }

    #[test]
    fn consume_all_2() {
        test_consume(
            "abc)",
            |l| Some(l.consume_all(|c| c.is_alphabetic())),
            (),
            ")",
        );
    }
}
