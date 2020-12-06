use super::super::charset::*;
use super::super::grammar::Grammar;
use super::super::pattern::*;
use super::errors::*;
use super::tokenizer::*;

pub type RuleNamesMap<'input> = std::collections::HashMap<&'input str, usize>;

impl Pattern {
    pub fn parse<'input>(input: &'input str) -> ParserResult<'input> {
        Parser::new(input, None).parse_pattern()
    }
}

impl Grammar {
    pub fn parse(input: &str) -> Result<Grammar, GrammarParseError> {
        parse_grammar(input)
    }
}

pub fn parse_grammar(input: &str) -> Result<Grammar, GrammarParseError> {
    let tokens = Tokenizer::new(input).consume_grammar()?;
    let mut rule_names = RuleNamesMap::with_capacity(tokens.len());
    for token in tokens.into_iter() {
        if rule_names.insert(token.name, rule_names.len()).is_some() {
            return Err(GrammarParseError::DuplicatedRuleName(token.name));
        }
    }
    let mut rules = Vec::with_capacity(tokens.len());
    for token in tokens.into_iter() {
        let pattern = Parser::new(token.pattern, Some(&rule_names))
            .parse_pattern()
            .map_err(|err| match err {
                PatternParseError::UnknownRule(name) => GrammarParseError::UnknownRule(name),
                PatternParseError::InvalidPattern => GrammarParseError::InvalidPattern(token.name),
                PatternParseError::InvalidRangeInSet => {
                    GrammarParseError::InvalidRangeInSet(token.name)
                }
            })?;
        rules.push(Rule::new(token.name, pattern, token.error));
    }
    Ok(Grammar::new(rules.into()))
}

use PatternParseError::{InvalidPattern, UnknownRule};

pub struct Parser<'input, 'rules> {
    rules: Option<&'rules RuleNamesMap<'input>>,
    tokenizer: Tokenizer<'input>,
}

type ParserResult<'input> = Result<Pattern, PatternParseError<'input>>;

impl<'input, 'rules> Parser<'input, 'rules> {
    pub fn new(input: &'input str, rules: Option<&'rules RuleNamesMap<'input>>) -> Self {
        Parser {
            tokenizer: Tokenizer::new(input),
            rules: rules,
        }
    }

    /// $ = "" |  or
    pub fn parse_pattern(&mut self) -> Result<Pattern, PatternParseError<'input>> {
        if !self.tokenizer.lexer.has_input() {
            Ok(Pattern::word(""))
        } else {
            let pattern = self.parse_or_no_par()?;
            if !self.tokenizer.lexer.has_input() {
                Ok(pattern)
            } else {
                Err(PatternParseError::InvalidPattern)
            }
        }
    }

    /// p1 = word | p2 card
    fn parse_p1(&mut self) -> ParserResult<'input> {
        or(self.parse_word(), || {
            self.try_parse(|s| s.parse_p2().and_then(|p| s.parse_cardinality(p)))
        })
    }

    /// p2 = or_p | rule | set | charset
    fn parse_p2(&mut self) -> ParserResult<'input> {
        or(self.parse_or_par(), || {
            or(self.parse_rule(), || {
                or(self.parse_set(), || {
                    self.tokenizer
                        .consume_charset_not_in_set()
                        .map(|c| Pattern::Char(c))
                        .ok_or(InvalidPattern)
                })
            })
        })
    }

    /// and =  p1+
    fn parse_and_no_par(&mut self) -> ParserResult<'input> {
        self.try_parse(|s| {
            s.parse_p1().and_then(|p1| {
                let mut patterns = Vec::with_capacity(6);
                patterns.push(p1);
                s.parse_all(&mut patterns, Parser::parse_p1)?;
                Ok(if patterns.len() == 1 {
                    patterns.pop().unwrap()
                } else {
                    Pattern::And(patterns.into())
                })
            })
        })
    }

    /// or =  and ("|" and)*
    fn parse_or_no_par(&mut self) -> ParserResult<'input> {
        self.try_parse(|s| {
            s.parse_and_no_par().and_then(|p1| {
                let mut patterns = Vec::with_capacity(6);
                patterns.push(p1);
                s.parse_all(&mut patterns, |s| {
                    if s.tokenizer.lexer.consume_char('|') {
                        or(s.parse_and_no_par(), || Ok(Pattern::word("")))
                    } else {
                        Err(InvalidPattern)
                    }
                })?;
                Ok(if patterns.len() == 1 {
                    patterns.pop().unwrap()
                } else {
                    Pattern::Or(patterns.into())
                })
            })
        })
    }

    /// or_p = "(" or ")"
    fn parse_or_par(&mut self) -> ParserResult<'input> {
        self.parse_delimited('(', Parser::parse_or_no_par, ')')
    }

    fn parse_all<F, T>(
        &mut self,
        patterns: &mut Vec<T>,
        parser: F,
    ) -> Result<(), PatternParseError<'input>>
    where
        F: Fn(&mut Parser<'input, 'rules>) -> Result<T, PatternParseError<'input>>,
    {
        loop {
            match parser(self) {
                Ok(pattern) => patterns.push(pattern),
                Err(InvalidPattern) => break,
                Err(err) => {
                    return Err(err);
                }
            }
        }
        Ok(())
    }

    fn parse_rule(&mut self) -> ParserResult<'input> {
        self.try_parse(|s| {
            s.tokenizer
                .consume_rule()
                .ok_or(InvalidPattern)
                .and_then(|(rule_name, keep)| {
                    s.rules
                        .and_then(|rules| {
                            rules
                                .get(rule_name)
                                .map(|&index| Pattern::rule(index, keep))
                        })
                        .ok_or(UnknownRule(rule_name))
                })
        })
    }

    /// word = [^+*?()[\\|{]+   non seguito da ?*+
    fn parse_word(&mut self) -> ParserResult<'input> {
        self.tokenizer
            .consume_word()
            .ok_or(InvalidPattern)
            .and_then(|word| {
                let mut chars = word.chars();
                let first_char = chars.next().unwrap();
                Ok(if chars.next().is_none() {
                    Pattern::Char(CharSet::Char(first_char))
                } else {
                    Pattern::word(word.into())
                })
            })
    }

    fn parse_cardinality(&mut self, pattern: Pattern) -> ParserResult<'input> {
        Ok(
            if let Some((min, max)) = self.tokenizer.consume_cardinality() {
                Pattern::Cardinality {
                    min,
                    max,
                    pattern: pattern.into(),
                }
            } else {
                pattern
            },
        )
    }

    /// set = "[^" charset_s+ "]" | "[" charset_s+ "]"
    fn parse_set(&mut self) -> ParserResult<'input> {
        self.parse_delimited(
            '[',
            |s| {
                let is_nset = s.tokenizer.lexer.consume_char('^');
                let charset = s.parse_charset_in_set()?;
                let mut charsets = Vec::with_capacity(8);
                charsets.push(charset);
                s.parse_all(&mut charsets, Parser::parse_charset_in_set)?;
                Ok(if is_nset {
                    if charsets.len() == 1 {
                        Pattern::NotChar(charsets.pop().unwrap())
                    } else {
                        Pattern::NotOneOf(charsets.into_boxed_slice())
                    }
                } else {
                    if charsets.len() == 1 {
                        Pattern::Char(charsets.pop().unwrap())
                    } else {
                        Pattern::OneOf(charsets.into_boxed_slice())
                    }
                })
            },
            ']',
        )
    }

    /// charset_s =  charset "-" charset | charset
    fn parse_charset_in_set(&mut self) -> Result<CharSet, PatternParseError<'input>> {
        self.try_parse(|s| {
            let charset1 = s.tokenizer.consume_charset_in_set().ok_or(InvalidPattern)?;
            if let CharSet::Char(c1) = charset1 {
                return or(
                    s.try_parse(|s| {
                        if let Some('-') = s.tokenizer.lexer.next_char() {
                            if let Some(c2) = s.tokenizer.consume_charset_in_set() {
                                // we expect a valid range otherwise we have an error
                                if let CharSet::Char(c2) = c2 {
                                    let max_u16 = std::u16::MAX as u32;
                                    if c1 as u32 <= max_u16 && c2 as u32 <= max_u16 {
                                        return Ok(CharSet::Range((c1 as u16, c2 as u16)));
                                    }
                                } else {
                                    return Err(PatternParseError::InvalidRangeInSet);
                                }
                            }
                        }
                        Err(InvalidPattern)
                    }),
                    || Ok(charset1),
                );
            }
            Ok(charset1)
        })
    }

    fn parse_delimited<F>(
        &mut self,
        start_del: char,
        parser: F,
        end_del: char,
    ) -> ParserResult<'input>
    where
        F: FnOnce(&mut Parser<'input, 'rules>) -> ParserResult<'input>,
    {
        self.try_parse(|s| {
            if s.tokenizer.lexer.consume_char(start_del) {
                let result = parser(s)?;
                if s.tokenizer.lexer.consume_char(end_del) {
                    return Ok(result);
                }
            }
            Err(InvalidPattern)
        })
    }

    fn try_parse<F, T>(&mut self, func: F) -> Result<T, PatternParseError<'input>>
    where
        F: FnOnce(&mut Parser<'input, 'rules>) -> Result<T, PatternParseError<'input>>,
    {
        self.tokenizer.lexer.push_state();
        let result = func(self);
        if let Ok(_) = result {
            self.tokenizer.lexer.pop_state();
        } else {
            self.tokenizer.lexer.rollback_state();
        }
        result
    }
}

fn or<'input, F, T>(
    result: Result<T, PatternParseError<'input>>,
    func: F,
) -> Result<T, PatternParseError<'input>>
where
    F: FnOnce() -> Result<T, PatternParseError<'input>>,
{
    if let Err(InvalidPattern) = result {
        func()
    } else {
        result
    }
}

#[cfg(test)]
mod pattern_parser_tests {
    use super::*;

    fn parser(input: &str) -> Parser {
        Parser::new(input, None)
    }

    fn remainder<'input>(p: &Parser<'input, '_>) -> &'input str {
        p.tokenizer.lexer.input()
    }

    fn test_parse<'input, F, T>(parser_input: &'input str, mut pf: F, ex_patt: T, ex_rem: &str)
    where
        F: FnMut(&mut Parser<'input, 'input>) -> Result<T, PatternParseError<'input>>,
        T: std::fmt::Debug + PartialEq,
    {
        let mut p = parser(parser_input);
        assert_eq!((pf(&mut p), remainder(&p)), (Ok(ex_patt), ex_rem));
    }

    #[test]
    fn parse_word_1() {
        test_parse("ciao", Parser::parse_word, Pattern::word("ciao"), "");
    }

    #[test]
    fn parse_word_2() {
        test_parse("ciao)", Parser::parse_word, Pattern::word("ciao"), ")");
    }

    #[test]
    // p1 = w | p2 card
    fn parse_p1() {
        test_parse("ciao?", Parser::parse_p1, Pattern::word("cia"), "o?");
        test_parse(
            "a?a",
            Parser::parse_p1,
            Pattern::zero_or_one(Pattern::char('a')),
            "a",
        );
        test_parse(
            "[ab\\w]",
            Parser::parse_p1,
            Pattern::OneOf(
                vec![
                    CharSet::Char('a'),
                    CharSet::Char('b'),
                    CharSet::AlphaNum(true),
                ]
                .into(),
            ),
            "",
        );
    }

    #[test]
    fn parse_charset_in_set_1() {
        test_parse(
            "ab\\w]",
            Parser::parse_charset_in_set,
            CharSet::Char('a'),
            "b\\w]",
        );
        test_parse(
            "b\\w]",
            Parser::parse_charset_in_set,
            CharSet::Char('b'),
            "\\w]",
        );
        test_parse(
            "\\w]",
            Parser::parse_charset_in_set,
            CharSet::AlphaNum(true),
            "]",
        );
    }

    #[test]
    fn parse_set_1() {
        test_parse(
            "[ab\\w]",
            Parser::parse_set,
            Pattern::OneOf(
                vec![
                    CharSet::Char('a'),
                    CharSet::Char('b'),
                    CharSet::AlphaNum(true),
                ]
                .into(),
            ),
            "",
        );
        test_parse(
            "[^ab\\w]",
            Parser::parse_set,
            Pattern::NotOneOf(
                vec![
                    CharSet::Char('a'),
                    CharSet::Char('b'),
                    CharSet::AlphaNum(true),
                ]
                .into(),
            ),
            "",
        );
        test_parse(
            "[ab\\Wc]",
            Parser::parse_set,
            Pattern::OneOf(
                vec![
                    CharSet::Char('a'),
                    CharSet::Char('b'),
                    CharSet::AlphaNum(false),
                    CharSet::Char('c'),
                ]
                .into(),
            ),
            "",
        );
    }

    #[test]
    // p1 = w | p2 card
    fn parse_pattern_1() {
        test_parse(
            "abc\\wdef",
            Parser::parse_pattern,
            Pattern::And(
                vec![
                    Pattern::word("abc"),
                    Pattern::Char(CharSet::AlphaNum(true)),
                    Pattern::word("def"),
                ]
                .into(),
            ),
            "",
        );
    }

    #[test]
    fn parse_and() {
        test_parse("a", Parser::parse_and_no_par, Pattern::char('a'), "");
        test_parse("ab", Parser::parse_and_no_par, Pattern::word("ab"), "");
        test_parse("a|ab", Parser::parse_and_no_par, Pattern::char('a'), "|ab");
        test_parse(
            "ab|ab",
            Parser::parse_and_no_par,
            Pattern::word("ab"),
            "|ab",
        );
    }

    #[test]
    fn parse_or_1() {
        test_parse(
            "a|bc",
            Parser::parse_or_no_par,
            Pattern::Or(vec![Pattern::char('a'), Pattern::word("bc")].into()),
            "",
        );
    }

    #[test]
    // p1 = w | p2 card
    fn parse_pattern_2() {
        test_parse(
            "(a|bc)",
            Parser::parse_pattern,
            Pattern::Or(vec![Pattern::char('a'), Pattern::word("bc")].into()),
            "",
        );
    }

    #[test]
    // p1 = w | p2 card
    fn parse_pattern_3() {
        test_parse(
            "a|bc",
            Parser::parse_pattern,
            Pattern::Or(vec![Pattern::char('a'), Pattern::word("bc")].into()),
            "",
        );
    }

    #[test]
    // p1 = w | p2 card
    fn parse_pattern_4() {
        test_parse(
            "a(bcd(e|fg))+",
            Parser::parse_pattern,
            Pattern::And(
                vec![
                    Pattern::char('a'),
                    Pattern::one_or_more(Pattern::And(
                        vec![
                            Pattern::word("bcd"),
                            Pattern::Or(vec![Pattern::char('e'), Pattern::word("fg")].into()),
                        ]
                        .into(),
                    )),
                ]
                .into(),
            ),
            "",
        );
    }

    #[test]
    // p1 = w | p2 card
    fn parse_pattern_5() {
        test_parse(
            "a|bc|",
            Parser::parse_pattern,
            Pattern::Or(vec![Pattern::char('a'), Pattern::word("bc"), Pattern::word("")].into()),
            "",
        );
    }

    #[test]
    fn parse_rule_escaped_1() {
        let input = "A \"\\\"\"";
        let mut lexer = Tokenizer::new(input);
        let maybe_token = lexer.consume_grammar_rule();
        assert_eq!(
            (maybe_token, lexer.lexer.input()),
            (
                Ok(RuleToken {
                    name: "A".into(),
                    pattern: "\\\"",
                    error: ""
                }),
                ""
            )
        );
    }

    #[test]
    fn parse_rule_escaped_2() {
        let input = "A \"\\\\\"\"";
        let mut lexer = Tokenizer::new(input);
        let maybe_token = lexer.consume_grammar_rule();
        assert_eq!(
            (maybe_token, lexer.lexer.input()),
            (
                Ok(RuleToken {
                    name: "A".into(),
                    pattern: "\\\\",
                    error: ""
                }),
                "\""
            )
        );
    }
}
