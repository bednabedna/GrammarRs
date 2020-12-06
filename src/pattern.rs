use super::charset::*;
use super::token::*;

#[derive(PartialEq, Debug)]
pub struct Rule {
    name: Box<str>,
    pattern: Pattern,
    error: Option<Box<str>>,
}

impl Rule {
    pub fn new(name: &str, pattern: Pattern, error: &str) -> Self {
        Self {
            name: name.into(),
            pattern,
            error: if error.is_empty() {
                None
            } else {
                Some(error.into())
            },
        }
    }
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }
    pub fn error(&self) -> Option<&str> {
        self.error.as_ref().map(|err| err.as_ref())
    }
    pub fn pattern(&self) -> &Pattern {
        &self.pattern
    }
}

#[derive(PartialEq, Clone)]
pub enum Pattern {
    Word(Box<str>),
    NotOneOf(Box<[CharSet]>),
    OneOf(Box<[CharSet]>),
    Char(CharSet),
    NotChar(CharSet),
    Cardinality {
        pattern: Box<Pattern>,
        min: u32,
        max: u32,
    },
    Or(Box<[Pattern]>),
    And(Box<[Pattern]>),
    RuleIndex {
        index: usize,
        keep: bool,
    },
}

#[derive(PartialEq, Debug)]
pub struct ErrorNode<'rule, 'input> {
    message: &'rule str,
    input: &'input str,
    next: Option<Box<ErrorNode<'rule, 'input>>>,
}

#[derive(PartialEq, Debug)]
pub struct ErrorEntry<'rule, 'input> {
    message: &'rule str,
    input: &'input str,
}

impl<'rule, 'input> ErrorEntry<'rule, 'input> {
    pub fn message(&self) -> &'rule str {
        self.message
    }
    pub fn input(&self) -> &'input str {
        self.input
    }
}

impl<'rule, 'input> ErrorNode<'rule, 'input> {
    fn new(
        message: &'rule str,
        input: &'input str,
        next: Option<Box<ErrorNode<'rule, 'input>>>,
    ) -> Self {
        ErrorNode {
            message,
            input,
            next,
        }
    }
    pub fn message(&self) -> &'rule str {
        self.message
    }
    pub fn input(&self) -> &'input str {
        self.input
    }
    pub fn to_boxed_slice(self) -> Box<[ErrorEntry<'rule, 'input>]> {
        let mut vec = Vec::with_capacity(4);
        vec.push(ErrorEntry {
            message: self.message,
            input: self.input,
        });
        let mut maybe_next = self.next;
        while let Some(next) = maybe_next {
            vec.push(ErrorEntry {
                message: next.message,
                input: next.input,
            });
            maybe_next = next.next;
        }
        vec.into()
    }
}

impl Pattern {
    pub fn word(text: &str) -> Pattern {
        Pattern::Word(text.into())
    }

    pub fn char(c: char) -> Pattern {
        Pattern::Char(CharSet::Char(c))
    }

    pub fn rule(index: usize, keep: bool) -> Pattern {
        Pattern::RuleIndex { index, keep }
    }

    pub fn zero_or_one(pattern: Pattern) -> Pattern {
        Pattern::Cardinality {
            min: 0,
            max: 1,
            pattern: pattern.into(),
        }
    }

    pub fn zero_or_more(pattern: Pattern) -> Pattern {
        Pattern::Cardinality {
            min: 0,
            max: std::u32::MAX,
            pattern: pattern.into(),
        }
    }

    pub fn one_or_more(pattern: Pattern) -> Pattern {
        Pattern::Cardinality {
            min: 1,
            max: std::u32::MAX,
            pattern: pattern.into(),
        }
    }

    pub fn consume<'input, 'rule>(
        &self,
        rule_map: &'rule [Rule],
        matches: &'_ mut Vec<Token<'input, 'rule>>,
        input: &'input str,
    ) -> Result<&'input str, Option<Box<ErrorNode<'rule, 'input>>>> {
        match &self {
            Pattern::Word(text) => {
                if input.starts_with(text.as_ref()) {
                    Ok(&input[text.len()..])
                } else {
                    Err(None)
                }
            }
            Pattern::Char(charset) => {
                if let Some(first_char) = input.chars().next() {
                    if charset.matches(first_char) {
                        return Ok(&input[first_char.len_utf8()..]);
                    }
                }
                Err(None)
            }
            Pattern::NotChar(charset) => {
                if let Some(first_char) = input.chars().next() {
                    if !charset.matches(first_char) {
                        return Ok(&input[first_char.len_utf8()..]);
                    }
                }
                Err(None)
            }
            Pattern::OneOf(charsets) => {
                if let Some(first_char) = input.chars().next() {
                    if charsets.iter().any(|charset| charset.matches(first_char)) {
                        return Ok(&input[first_char.len_utf8()..]);
                    }
                }
                Err(None)
            }
            Pattern::NotOneOf(charsets) => {
                if let Some(first_char) = input.chars().next() {
                    if !charsets.iter().any(|charset| charset.matches(first_char)) {
                        return Ok(&input[first_char.len_utf8()..]);
                    }
                }
                Err(None)
            }
            Pattern::RuleIndex { index, keep } => {
                let rule = rule_map.get(*index).expect("using an unknown rule");
                let result = if *keep {
                    let mut rule_matches = Vec::new();
                    rule.pattern
                        .consume(rule_map, &mut rule_matches, input)
                        .and_then(|remainder| {
                            matches.push(Token {
                                rule: rule.name(),
                                input: &input[..input.len() - remainder.len()],
                                children: if rule_matches.is_empty() {
                                    None
                                } else {
                                    Some(rule_matches.into_boxed_slice())
                                },
                            });
                            Ok(remainder)
                        })
                } else {
                    rule.pattern.consume(rule_map, matches, input)
                };
                result.map_err(|maybe_prev_err| {
                    if let Some(message) = rule.error.as_ref() {
                        Some(Box::new(ErrorNode::new(
                            message.as_ref(),
                            input,
                            maybe_prev_err,
                        )))
                    } else {
                        maybe_prev_err
                    }
                })
            }
            Pattern::Or(patterns) => {
                for pattern in patterns.iter() {
                    let matches_len = matches.len();
                    match pattern.consume(rule_map, matches, input) {
                        ok @ Ok(_) => {
                            return ok;
                        }
                        Err(None) => {
                            matches.truncate(matches_len);
                        }
                        err => {
                            return err;
                        }
                    }
                }
                Err(None)
            }
            Pattern::And(patterns) => {
                let mut input = input;
                let matches_len = matches.len();
                for pattern in patterns.into_iter() {
                    match pattern.consume(rule_map, matches, input) {
                        Ok(remainder) => {
                            input = remainder;
                        }
                        err => {
                            matches.truncate(matches_len);
                            return err;
                        }
                    }
                }
                Ok(input)
            }
            Pattern::Cardinality { min, max, pattern } => {
                let mut input = input;
                for times in 0..(*max) {
                    let matches_len = matches.len();
                    match pattern.consume(rule_map, matches, input) {
                        Ok(remainder) => {
                            if remainder != input {
                                input = remainder;
                            } else {
                                break;
                            }
                        }
                        Err(None) if times >= *min => {
                            matches.truncate(matches_len);
                            break;
                        }
                        err => {
                            return err;
                        }
                    }
                }
                Ok(input)
            }
        }
    }

    pub fn matches(&self, input: &str) -> bool {
        let rule_map = Vec::new();
        let mut matches = Vec::new();
        if let Ok(remainder) = self.consume(&rule_map, &mut matches, input) {
            remainder.is_empty()
        } else {
            false
        }
    }
}

use std::fmt::{Debug, Display};

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Word(w) => write!(f, "{}", w),
            Pattern::NotOneOf(charsets) => {
                write!(f, "[^")?;
                for c in charsets.iter() {
                    write!(f, "{}", c)?;
                }
                write!(f, "]")?;
                Ok(())
            }
            Pattern::OneOf(charsets) => {
                write!(f, "[")?;
                for c in charsets.iter() {
                    write!(f, "{}", c)?;
                }
                write!(f, "]")?;
                Ok(())
            }
            Pattern::Char(c) => write!(f, "{{{}}}", c),
            Pattern::NotChar(c) => write!(f, "{{^{}}}", c),
            Pattern::Cardinality {
                pattern,
                ref min,
                max,
            } => {
                if *min == 0 && *max == 1 {
                    write!(f, "{}?", pattern)
                } else if *min == 0 && *max == std::u32::MAX {
                    write!(f, "{}*", pattern)
                } else if *min == 1 && *max == std::u32::MAX {
                    write!(f, "{}+", pattern)
                } else {
                    write!(f, "{}{{{},{}}}", pattern, min, max)
                }
            }
            Pattern::Or(p) => {
                let mut patterns = p.iter();
                if let Some(p1) = patterns.next() {
                    write!(f, "<OR>({}", p1)?;
                    for p in patterns {
                        write!(f, "|{}", p)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Pattern::And(patterns) => {
                write!(f, "<AND>(")?;
                for p in patterns.iter() {
                    write!(f, "{}", p)?;
                }
                write!(f, ")")?;
                Ok(())
            }
            Pattern::RuleIndex { index, keep } => {
                if *keep {
                    write!(f, "{{{}}}", index)
                } else {
                    write!(f, "{{!{}}}", index)
                }
            }
        }
    }
}

impl Debug for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constant_simple_pattern() {
        let pattern = Pattern::word("ciao");
        assert!(pattern.matches("ciao"))
    }

    #[test]
    fn constant_group() {
        let pattern = Pattern::And(vec![Pattern::word("ciao")].into());
        assert!(pattern.matches("ciao"));
        assert!(!pattern.matches("ciaoo"));
    }

    #[test]
    fn simple_or() {
        let pattern = Pattern::Or(
            vec![
                Pattern::word("ciao"),
                Pattern::word("bau"),
                Pattern::word("a"),
                Pattern::word(""),
            ]
            .into(),
        );
        assert!(pattern.matches("ciao"));
        assert!(pattern.matches("bau"));
        assert!(pattern.matches("a"));
        assert!(pattern.matches(""));
        assert!(!pattern.matches("miao"));
        assert!(!pattern.matches("b"));
    }

    #[test]
    fn nested_or() {
        let pattern = Pattern::Or(
            vec![
                Pattern::word("ciao"),
                Pattern::Or(vec![Pattern::word("dsa"), Pattern::word("asd")].into()),
                Pattern::word("a"),
            ]
            .into(),
        );
        assert!(pattern.matches("ciao"));
        assert!(pattern.matches("a"));
        assert!(pattern.matches("dsa"));
        assert!(pattern.matches("asd"));
    }

    #[test]
    fn constant_complex_pattern() {
        let pattern = Pattern::And(
            vec![
                Pattern::word("hello"),
                Pattern::word(" "),
                Pattern::word("world!"),
            ]
            .into(),
        );
        assert!(pattern.matches("hello world!"))
    }

    fn zero_or_one_pattern() -> Pattern {
        Pattern::And(
            vec![
                Pattern::word("ripeto "),
                Pattern::zero_or_one(Pattern::word("tante ")),
                Pattern::word("volte"),
            ]
            .into(),
        )
    }

    fn zero_or_more_constant_pattern() -> Pattern {
        Pattern::zero_or_more(Pattern::word("a"))
    }

    fn zero_or_more_pattern() -> Pattern {
        Pattern::And(
            vec![
                Pattern::word("ripeto "),
                Pattern::zero_or_more(Pattern::word("tante ")),
                Pattern::word("volte"),
            ]
            .into(),
        )
    }

    fn one_or_more_pattern() -> Pattern {
        Pattern::And(
            vec![
                Pattern::word("ripeto "),
                Pattern::one_or_more(Pattern::word("tante ")),
                Pattern::word("volte"),
            ]
            .into(),
        )
    }

    #[test]
    fn zero_or_more_constant() {
        assert!(zero_or_more_constant_pattern().matches(""));
        assert!(zero_or_more_constant_pattern().matches("a"));
        assert!(zero_or_more_constant_pattern().matches("aaa"));
    }

    #[test]
    fn zero_or_one_greedy() {
        assert_eq!(
            zero_or_more_constant_pattern().consume(&Vec::new(), &mut Vec::new(), "ba"),
            Ok("ba")
        );
        assert_eq!(
            zero_or_more_constant_pattern().consume(&Vec::new(), &mut Vec::new(), "aba"),
            Ok("ba")
        );
        assert_eq!(
            zero_or_more_constant_pattern().consume(&Vec::new(), &mut Vec::new(), "aaab"),
            Ok("b")
        );
    }

    #[test]
    fn zero_or_one() {
        assert!(zero_or_one_pattern().matches("ripeto volte"));
        assert!(zero_or_one_pattern().matches("ripeto tante volte"));
        assert!(!zero_or_one_pattern().matches("ripeto tante tante tante tante volte"));
    }

    #[test]
    fn zero_or_more() {
        assert!(zero_or_more_pattern().matches("ripeto volte"));
        assert!(zero_or_more_pattern().matches("ripeto tante volte"));
        assert!(zero_or_more_pattern().matches("ripeto tante tante tante tante volte"));
    }

    #[test]
    fn one_or_more() {
        assert!(!one_or_more_pattern().matches("ripeto volte"));
        assert!(one_or_more_pattern().matches("ripeto tante volte"));
        assert!(one_or_more_pattern().matches("ripeto tante tante tante tante volte"));
    }

    #[test]
    fn one_or_more_infinite_loop() {
        let pattern = Pattern::one_or_more(Pattern::one_or_more(Pattern::one_or_more(
            Pattern::word(""),
        )));
        assert_eq!(
            pattern.consume(&Vec::new(), &mut Vec::new(), "hello world!"),
            Ok("hello world!")
        );
        assert_eq!(pattern.consume(&Vec::new(), &mut Vec::new(), ""), Ok(""));
    }

    #[test]
    fn charsets() {
        let pattern = Pattern::OneOf(
            vec![
                CharSet::Char('a'),
                CharSet::Char('B'),
                CharSet::Digits(true),
                CharSet::Spaces(true),
            ]
            .into(),
        );
        assert!(pattern.matches("a"));
        assert!(pattern.matches("B"));
        assert!(pattern.matches("0"));
        assert!(pattern.matches("3"));
        assert!(pattern.matches(" "));
        assert!(pattern.matches("\t"));
        assert!(pattern.matches("\n"));
        assert!(!pattern.matches("."));
        assert!(!pattern.matches(""));
        assert!(!pattern.matches("b"));
        assert!(!pattern.matches("c"));
        assert!(!pattern.matches("11"));
    }

    #[test]
    fn not_charsets() {
        let pattern = Pattern::NotOneOf(
            vec![
                CharSet::Char('a'),
                CharSet::Char('B'),
                CharSet::Digits(true),
                CharSet::Spaces(true),
            ]
            .into(),
        );
        assert!(!pattern.matches("a"));
        assert!(!pattern.matches("B"));
        assert!(!pattern.matches("0"));
        assert!(!pattern.matches("3"));
        assert!(!pattern.matches(" "));
        assert!(!pattern.matches("\t"));
        assert!(!pattern.matches("\n"));
        assert!(!pattern.matches(""));
        assert!(pattern.matches("."));
        assert!(pattern.matches("b"));
        assert!(pattern.matches("c"));
        assert!(!pattern.matches("11"));
    }

    #[test]
    fn charset() {
        let pattern = Pattern::And(
            vec![
                Pattern::word("ciao"),
                Pattern::Char(CharSet::Digits(true)),
                Pattern::Char(CharSet::Char('!')),
            ]
            .into(),
        );
        assert!(pattern.matches("ciao3!"));
        assert!(pattern.matches("ciao0!"));
        assert!(!pattern.matches("ciao!"));
        assert!(!pattern.matches("ciaoo!"));
    }

    #[test]
    fn pattern_mem_size() {
        assert_eq!(std::mem::size_of::<Pattern>(), 24);
    }
}
