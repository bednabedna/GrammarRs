#[derive(Debug, PartialEq)]
pub enum GrammarParseError<'input> {
    MissingRuleIdentifier(&'input str),
    MissingPatternStartQuote(&'input str),
    MissingPatternEndQuote(&'input str),
    MissingErrorStartQuote(&'input str),
    MissingErrorEndQuote(&'input str),
    EmptyGrammar,
    InvalidPattern(&'input str),
    InvalidGrammar,
    InvalidRangeInSet(&'input str),
    UnknownRule(&'input str),
    DuplicatedRuleName(&'input str),
}

impl<'input> std::fmt::Display for GrammarParseError<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GrammarParseError::MissingRuleIdentifier(rule) => {
                if rule.is_empty() {
                    write!(f, "missing first rule identifier")
                } else {
                    write!(f, "missing rule identifier after rule \"{}\"", rule)
                }
            }
            GrammarParseError::MissingPatternStartQuote(rule) => {
                write!(f, "missing pattern start quote in rule \"{}\"", rule)
            }
            GrammarParseError::MissingPatternEndQuote(rule) => {
                write!(f, "missing pattern end quote in rule \"{}\"", rule)
            }
            GrammarParseError::MissingErrorStartQuote(rule) => write!(
                f,
                "missing error start quote identifier in rule \"{}\"",
                rule
            ),
            GrammarParseError::MissingErrorEndQuote(rule) => {
                write!(f, "missing error end quote identifier in rule \"{}\"", rule)
            }
            GrammarParseError::EmptyGrammar => write!(f, "grammar doesn't have any rule"),
            GrammarParseError::InvalidPattern(rule) => {
                write!(f, "invalid pattern in rule \"{}\"", rule)
            }
            GrammarParseError::InvalidGrammar => write!(f, "invalid grammar"),
            GrammarParseError::InvalidRangeInSet(rule) => {
                write!(f, "invalid range in set in rule \"{}\"", rule)
            }
            GrammarParseError::UnknownRule(rule) => write!(f, "use of unknown rule \"{}\"", rule),
            GrammarParseError::DuplicatedRuleName(rule) => {
                write!(f, "multiple definition of rule \"{}\"", rule)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum PatternParseError<'input> {
    UnknownRule(&'input str),
    InvalidPattern,
    InvalidRangeInSet,
}
