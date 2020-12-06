use super::super::charset::*;
use super::errors::*;
use super::lexer::*;

pub struct Tokenizer<'input> {
    pub lexer: Lexer<'input>,
}

#[derive(Debug, PartialEq)]
pub struct RuleToken<'input> {
    pub name: &'input str,
    pub pattern: &'input str,
    pub error: &'input str,
}

impl<'input> Tokenizer<'input> {
    pub fn new(input: &'input str) -> Self {
        Tokenizer {
            lexer: Lexer::new(input),
        }
    }

    pub fn try_consume<F, T>(&mut self, func: F) -> Option<T>
    where
        F: FnOnce(&mut Tokenizer<'input>) -> Option<T>,
    {
        self.lexer.push_state();
        let result = func(self);
        if result.is_some() {
            self.lexer.pop_state();
        } else {
            self.lexer.rollback_state();
        }
        result
    }

    pub fn consume_delimited<F, T>(
        &mut self,
        start_del: char,
        consumer: F,
        end_del: char,
    ) -> Option<T>
    where
        F: FnOnce(&mut Tokenizer<'input>) -> Option<T>,
    {
        self.try_consume(|s| {
            if s.lexer.consume_char(start_del) {
                let result = s.try_consume(consumer)?;
                if s.lexer.consume_char(end_del) {
                    return Some(result);
                }
            }
            None
        })
    }

    pub fn consume_spaces(&mut self) {
        self.lexer.consume_all(|c| c.is_ascii_whitespace());
    }

    pub fn consume_digits(&mut self) {
        self.lexer.consume_all(|c| c.is_ascii_digit());
    }

    pub fn consume_identifier(&mut self) -> Option<&'input str> {
        self.try_consume(|s| {
            if s.lexer
                .consume_one(|c| c.is_ascii_alphabetic() || c == '_' || c == '$')
            {
                s.lexer
                    .consume_all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '$');
                Some(s.lexer.peek_word())
            } else {
                None
            }
        })
    }

    pub fn consume_rule(&mut self) -> Option<(&'input str, bool)> {
        self.consume_delimited(
            '{',
            |s| {
                s.consume_spaces();
                let keep = !s.lexer.consume_char('?');
                s.consume_spaces();
                let id = s.consume_identifier()?;
                s.consume_spaces();
                Some((id, keep))
            },
            '}',
        )
    }

    pub fn is_next_char_escaped(&mut self) -> bool {
        let word = self.lexer.peek_word();
        let mut escaped = false;
        let mut iter = word.chars();
        while let Some('\\') = iter.next_back() {
            escaped = !escaped;
        }
        escaped
    }

    pub fn consume_grammar(
        mut self,
    ) -> Result<Box<[RuleToken<'input>]>, GrammarParseError<'input>> {
        let mut rules = Vec::with_capacity(16);
        self.consume_spaces();
        loop {
            match self.consume_grammar_rule() {
                Ok(rule) => rules.push(rule),
                Err(err) => {
                    return if !self.lexer.has_input() {
                        if rules.is_empty() {
                            Err(GrammarParseError::EmptyGrammar)
                        } else {
                            Ok(rules.into_boxed_slice())
                        }
                    } else {
                        if let GrammarParseError::MissingRuleIdentifier(_) = err {
                            if let Some(rule) = rules.last() {
                                Err(GrammarParseError::MissingRuleIdentifier(rule.name))
                            } else {
                                Err(err)
                            }
                        } else {
                            Err(err)
                        }
                    }
                }
            }
        }
    }

    pub fn consume_grammar_rule(&mut self) -> Result<RuleToken<'input>, GrammarParseError<'input>> {
        let mut error = GrammarParseError::MissingRuleIdentifier("");
        self.try_consume(|s| {
            let name = s.consume_identifier()?;
            s.consume_spaces();
            error = GrammarParseError::MissingPatternStartQuote(name);
            s.consume_delimited(
                '"',
                |s| {
                    loop {
                        s.lexer.consume_all(|c| c != '"');
                        if s.is_next_char_escaped() {
                            s.lexer.next_char();
                        } else {
                            break;
                        }
                    }
                    error = GrammarParseError::MissingPatternEndQuote(name);
                    Some(RuleToken {
                        name,
                        pattern: s.lexer.peek_word(),
                        error: "",
                    })
                },
                '"',
            )
        })
        .and_then(|mut rule_token| {
            self.consume_spaces();
            if self.lexer.consume_word("->") {
                self.consume_spaces();
                error = GrammarParseError::MissingErrorStartQuote(rule_token.name);
                self.consume_delimited(
                    '"',
                    |s| {
                        loop {
                            s.lexer.consume_all(|c| c != '"');
                            if s.is_next_char_escaped() {
                                s.lexer.next_char();
                            } else {
                                break;
                            }
                        }
                        error = GrammarParseError::MissingErrorEndQuote(rule_token.name);
                        Some(s.lexer.peek_word())
                    },
                    '"',
                )
                .and_then(|error| {
                    self.consume_spaces();
                    rule_token.error = error;
                    Some(rule_token)
                })
            } else {
                Some(rule_token)
            }
        })
        .ok_or(error)
    }

    pub fn consume_word(&mut self) -> Option<&'input str> {
        self.try_consume(|s| {
            s.lexer.consume_all(|c| match c {
                '\\' | '[' | '(' | ')' | '|' | '.' | '+' | '*' | '?' | '{' => false,
                _ => true,
            });
            if let Some(c) = s.lexer.peek_char() {
                match c {
                    '+' | '*' | '?' => s.lexer.rewind_char(),
                    '{' => {
                        s.lexer.push_state();
                        s.consume_spaces();
                        let is_cardinality = s.lexer.consume_one(|c| c.is_ascii_digit());
                        s.lexer.rollback_state();
                        if is_cardinality {
                            s.lexer.rewind_char()
                        }
                    }
                    _ => {}
                }
            }
            let word = s.lexer.peek_word();

            if word.is_empty() {
                None
            } else {
                Some(word)
            }
        })
    }

    pub fn parse_number(&mut self) -> Option<u32> {
        self.try_consume(|s| {
            s.consume_digits();
            s.lexer.peek_word().parse::<u32>().ok()
        })
    }

    pub fn consume_cardinality(&mut self) -> Option<(u32, u32)> {
        self.try_consume(|s| {
            let c = s.lexer.next_char()?;
            match c {
                '+' => Some((1, std::u32::MAX)),
                '*' => Some((0, std::u32::MAX)),
                '?' => Some((0, 1)),
                '{' => {
                    s.consume_spaces();
                    let min = s.parse_number()?;
                    s.consume_spaces();
                    if s.lexer.consume_char(',') {
                        s.consume_spaces();
                        let max = s.parse_number()?;
                        if min <= max && s.lexer.consume_char('}') {
                            return Some((min, max));
                        } else {
                            None
                        }
                    } else {
                        s.consume_spaces();
                        if s.lexer.consume_char('}') {
                            Some((min, min))
                        } else {
                            None
                        }
                    }
                }
                _ => None,
            }
        })
    }

    /// charset = \\?[^\]]
    pub fn consume_any_charset(&mut self) -> Option<CharSet> {
        self.try_consume(|s| {
            let c = s.lexer.next_char()?;
            match c {
                '\\' => {
                    if let Some(c) = s.lexer.next_char() {
                        let charset = match c {
                            'w' => CharSet::AlphaNum(true),
                            'W' => CharSet::AlphaNum(false),
                            'a' => CharSet::Alpha(true),
                            'A' => CharSet::Alpha(false),
                            'd' => CharSet::Digits(true),
                            'D' => CharSet::Digits(false),
                            's' => CharSet::Spaces(true),
                            'S' => CharSet::Spaces(false),
                            'n' => CharSet::Char('\n'),
                            'r' => CharSet::Char('\r'),
                            't' => CharSet::Char('\t'),
                            '0' => CharSet::Char('\0'),
                            _ => CharSet::Char(c),
                        };
                        Some(charset)
                    } else {
                        // if we are at the EOI we can accept a single \.
                        // if we were trying to parse an expression with delimiters
                        // that expression will fail
                        Some(CharSet::Char('\\'))
                    }
                }
                '.' => Some(CharSet::Any),
                _ => Some(CharSet::Char(c)),
            }
        })
    }
    // charset_ns = [^]-] && charset
    pub fn consume_charset_in_set(&mut self) -> Option<CharSet> {
        let first_char = self.lexer.peek_char()?;
        if first_char == ']' || first_char == '-' {
            None
        } else {
            self.consume_any_charset()
        }
    }

    /// charset_ns = [^+*?()[|{] && charset
    pub fn consume_charset_not_in_set(&mut self) -> Option<CharSet> {
        let first_char = self.lexer.peek_char()?;
        match first_char {
            '[' | '(' | ')' | '|' | '+' | '*' | '?' | '{' => None,
            _ => self.consume_any_charset(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_consume<'input, F, T>(input: &'input str, mut f: F, ex_patt: T, ex_rem: &str)
    where
        T: std::fmt::Debug + PartialEq,
        F: FnMut(&mut Tokenizer<'input>) -> Option<T>,
    {
        let mut l = Tokenizer::new(input);
        assert_eq!((f(&mut l), l.lexer.input()), (Some(ex_patt), ex_rem));
    }

    #[test]
    fn consume_word_1() {
        test_consume("abc", Tokenizer::consume_word, "abc", "");
        test_consume("abc)", Tokenizer::consume_word, "abc", ")");
    }

    #[test]
    fn consume_charset_1() {
        test_consume(
            "abc",
            Tokenizer::consume_any_charset,
            CharSet::Char('a'),
            "bc",
        );
    }

    #[test]
    fn consume_charset_2() {
        test_consume(
            "\\wabc",
            Tokenizer::consume_any_charset,
            CharSet::AlphaNum(true),
            "abc",
        );
    }
}
