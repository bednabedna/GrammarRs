use super::pattern::ErrorEntry;
use super::pattern::Rule;
use super::token::Token;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Grammar {
    rules: Box<[Rule]>,
}

impl Grammar {
    pub fn new(rules: Box<[Rule]>) -> Self {
        Grammar { rules }
    }

    pub fn tokenize<'input, 'rules>(
        &'rules self,
        input: &'input str,
    ) -> Result<Token<'input, 'rules>, Option<Box<[ErrorEntry<'rules, 'input>]>>> {
        self.rules().first().ok_or(None).and_then(|first_rule| {
            let mut matches = Vec::new();
            first_rule
                .pattern()
                .consume(&self.rules, &mut matches, input)
                .map_err(|maybe_err| maybe_err.map(|err| err.to_boxed_slice()))
                .and_then(|remainder| {
                    if remainder.is_empty() {
                        Ok(Token {
                            rule: first_rule.name(),
                            input: input,
                            children: if matches.is_empty() {
                                None
                            } else {
                                Some(matches.into_boxed_slice())
                            },
                        })
                    } else {
                        Err(None)
                    }
                })
        })
    }

    pub fn rules(&self) -> &[Rule] {
        self.rules.as_ref()
    }

    pub fn build_index<'g>(&'g self) -> HashMap<&'g str, &'g Rule> {
        let mut index = HashMap::with_capacity(self.rules().len());
        for rule in self.rules() {
            index.insert(rule.name().as_ref(), rule);
        }
        index
    }
}

#[cfg(test)]
mod grammar_tests {
    use super::super::token::Token;
    use super::*;

    #[test]
    fn parse_grammar() {
        let g = Grammar::parse("A \"#_{A}_#|@\"").expect("valid grammar");
        assert_eq!(
            g.tokenize("@"),
            Ok(Token {
                input: "@",
                rule: g.rules[0].name(),
                children: None
            })
        );
    }

    #[test]
    fn print() {
        let g = Grammar::parse("L \"{E}(,?{E})*\" E \"\\s*{W}\\s*\\(\\s*ID\\s*:\\s*{ID}\\s*\\)\\s*\" W \"\\w+\" ID \"\\d+\"").expect("valid grammar");
        assert_eq!(g.tokenize("ciao (ID: 34) bella (ID: 11)").expect("grammar matches").to_string(), "L [E [W -> \"ciao\", ID -> \"34\"] -> \"ciao (ID: 34) \", E [W -> \"bella\", ID -> \"11\"] -> \"bella (ID: 11)\"] -> \"ciao (ID: 34) bella (ID: 11)\"");
    }

    const LIST_GRAMMAR: &str = r#"
        L1 "\({L2}\)|"
        L2  "\[{L3}\]"
        L3  "\{{L1}\}"
        "#;

    #[test]
    fn list() {
        let g = Grammar::parse(LIST_GRAMMAR).expect("valid list grammar");

        assert!(g.tokenize("").is_ok());
        assert!(g.tokenize("([{}])").is_ok());
        assert!(g.tokenize("([{([{}])}])").is_ok());

        assert!(g.tokenize("()").is_err());
        assert!(g.tokenize("([])").is_err());
        assert!(g.tokenize("[]").is_err());
        assert!(g.tokenize("[()]").is_err());
        assert!(g.tokenize("(())").is_err());
        assert!(g.tokenize("([()])").is_err());
        assert!(g.tokenize("([[]])").is_err());
        assert!(g.tokenize("([([()])])").is_err());
    }

    const JSON_GRAMMAR: &str = r#"
        V "\s*({S}|{L}|{O}|{B}|{N})\s*"
        B "true|false"
        N "(-?0(\.\d+)|0|-?[1-9]\d*(\.\d+)?)"
        S "\"(\\.|[^\"])*\""
        L "\[{V}(,{V})*\]"
        O "\{(\s*{S}\s*:{V}(,\s*{S}\s*:{V})*)?\}"
        "#;

    #[test]
    fn json() {
        let g = Grammar::parse(JSON_GRAMMAR).expect("valid json grammar");
        assert_eq!(
            g.tokenize("{\"@\" : 2, \"#\": [true, [4, 3]]  }")
                .expect("grammar matches")
                .to_string(),
            "V [O [S -> \"\"@\"\", V [N -> \"2\"] -> \" 2\", S -> \"\"#\"\", V [L [V [B -> \"true\"] -> \"true\", V [L [V [N -> \"4\"] -> \"4\", V [N -> \"3\"] -> \" 3\"] -> \"[4, 3]\"] -> \" [4, 3]\"] -> \"[true, [4, 3]]\"] -> \" [true, [4, 3]]  \"] -> \"{\"@\" : 2, \"#\": [true, [4, 3]]  }\"] -> \"{\"@\" : 2, \"#\": [true, [4, 3]]  }\""
        );
        assert!(g.tokenize("2").is_ok());
        assert!(g.tokenize("200").is_ok());
        assert!(g.tokenize("-2").is_ok());
        assert!(g.tokenize("0").is_ok());
        assert!(g.tokenize("0.123").is_ok());
        assert!(g.tokenize("0.0").is_ok());
        assert!(g.tokenize("-0.0").is_ok());
        assert!(g.tokenize("-0.123").is_ok());
        assert!(g.tokenize("123000.12300").is_ok());
        assert!(g.tokenize("-123000.12300").is_ok());

        assert!(g.tokenize("-0").is_err());
        assert!(g.tokenize("-01").is_err());
        assert!(g.tokenize("00").is_err());
        assert!(g.tokenize("0134").is_err());
        assert!(g.tokenize(".123").is_err());
        assert!(g.tokenize("-00.123").is_err());

        assert!(g.tokenize("\"\"").is_ok());
        assert!(g.tokenize("\"a\"").is_ok());
        assert!(g.tokenize("\"asd\"").is_ok());
        assert!(g.tokenize(r#""\"""#).is_ok());
        assert!(g.tokenize(r#""\\\"""#).is_ok());
        assert!(g.tokenize(r#""\\\\\"""#).is_ok());

        assert!(g.tokenize(r#"""""#).is_err());
        assert!(g.tokenize(r#""\\"""#).is_err());
        assert!(g.tokenize(r#""\\\\"""#).is_err());
        assert!(g.tokenize(r#""\\\\\\"""#).is_err());
    }

    const RULES_GRAMMAR: &str = r#"
        GRAMMAR "{RULE}+\s*"
        RULE "\s*{NAME}\s*\"{PATTERN}\""
        NAME "[$_\a][$_\w]*"
        PATTERN "(\\.|[^\"])*"
    "#;

    #[test]
    fn rules() {
        let g = Grammar::parse(RULES_GRAMMAR).expect("valid rules grammar");
        assert_eq!(
            g.tokenize(JSON_GRAMMAR)
                .expect("grammar matches")
                .to_string(),
            "GRAMMAR [RULE [NAME -> \"V\", PATTERN -> \"\\s*({S}|{L}|{O}|{B}|{N})\\s*\"] -> \"\n        V \"\\s*({S}|{L}|{O}|{B}|{N})\\s*\"\", RULE [NAME -> \"B\", PATTERN -> \"true|false\"] -> \"\n        B \"true|false\"\", RULE [NAME -> \"N\", PATTERN -> \"(-?0(\\.\\d+)|0|-?[1-9]\\d*(\\.\\d+)?)\"] -> \"\n        N \"(-?0(\\.\\d+)|0|-?[1-9]\\d*(\\.\\d+)?)\"\", RULE [NAME -> \"S\", PATTERN -> \"\\\"(\\\\.|[^\\\"])*\\\"\"] -> \"\n        S \"\\\"(\\\\.|[^\\\"])*\\\"\"\", RULE [NAME -> \"L\", PATTERN -> \"\\[{V}(,{V})*\\]\"] -> \"\n        L \"\\[{V}(,{V})*\\]\"\", RULE [NAME -> \"O\", PATTERN -> \"\\{(\\s*{S}\\s*:{V}(,\\s*{S}\\s*:{V})*)?\\}\"] -> \"\n        O \"\\{(\\s*{S}\\s*:{V}(,\\s*{S}\\s*:{V})*)?\\}\"\"] -> \"\n        V \"\\s*({S}|{L}|{O}|{B}|{N})\\s*\"\n        B \"true|false\"\n        N \"(-?0(\\.\\d+)|0|-?[1-9]\\d*(\\.\\d+)?)\"\n        S \"\\\"(\\\\.|[^\\\"])*\\\"\"\n        L \"\\[{V}(,{V})*\\]\"\n        O \"\\{(\\s*{S}\\s*:{V}(,\\s*{S}\\s*:{V})*)?\\}\"\n        \""
        );
    }

    const SQL_SELECT_GRAMMAR: &str = r#"
        QUERY "select\s+{FIELD}(\s*,\s*{FIELD})\s+from\s+{FROM}(\s+where\s+{WHERE})?"
        FIELD "{?ID}"
        FROM  "\(({QUERY}|{ID})\)|{QUERY}|{ID}"
        WHERE "{?EXPR}"

        FUNC   "{FNAME}\(\s*{?EXPR}\s*(,\s*{?EXPR}\s*)*\)"
        FNAME  "{?ID}"
       
        EXPR   "{?BINARY}|{?FACTOR}"
        FACTOR "\({?EXPR}\)|{?UNARY}|{FUNC}|{?TERM}"
        TERM   "{?COST}|{ID}"
        COST   "{STR_C}|{NUM_C}|{BOOL_C}"
        
        UNARY  "{NOT}"
        NOT    "!{?EXPR}"

        BINARY "{OR}|{AND}|{ORDER}|{SUM}|{MUL}"
        OR     "({AND}|{ORDER}|{SUM}|{MUL}|{?FACTOR})\s*or\s*({OR}|{AND}|{ORDER}|{SUM}|{MUL}|{?FACTOR})"
        AND    "({ORDER}|{SUM}|{MUL}|{?FACTOR})\s*and\s*({AND}|{ORDER}|{SUM}|{MUL}|{?FACTOR})"
        ORDER  "({SUM}|{MUL}|{?FACTOR})\s*{OP_ORD}\s*({ORDER}|{SUM}|{MUL}|{?FACTOR})"
        SUM    "({MUL}|{?FACTOR})\s*{OP_SUM}\s*({SUM}|{MUL}|{?FACTOR})"
        MUL    "({?FACTOR})\s*{OP_MUL}\s*({MUL}|{?FACTOR})"
        
        OP_ORD ">=|<=|!=|<|>|="
        OP_SUM "\+|-"
        OP_MUL "\*|/"

        ID     "[$_\a][$_\w]*"
        STR_C  "\"(\\.|[^\"])*\""
        NUM_C  "(-?0(\.\d+)|0|-?[1-9]\d*(\.\d+)?)"
        BOOL_C "true|false"
    "#;

    fn matches_to_xml_rec(matches: &Token, buffer: &mut String) {
        buffer.push('<');
        let escaped_name = matches
            .rule
            .replace("\"", "&quot;")
            .replace("<", "&lt;")
            .replace(">", "&gt;");
        *buffer += escaped_name.as_ref();
        *buffer += " input=\"";
        *buffer += matches
            .input
            .replace("\"", "&quot;")
            .replace("<", "&lt;")
            .replace(">", "&gt;")
            .as_ref();
        buffer.push('\"');
        if let Some(children) = matches.children.as_ref() {
            buffer.push('>');
            for child in children.into_iter() {
                matches_to_xml_rec(child, buffer);
            }
            buffer.push('<');
            buffer.push('/');
            *buffer += escaped_name.as_ref();
        } else {
            buffer.push('/');
        }
        buffer.push('>');
    }

    fn matches_to_xml(matches: &Token) -> String {
        let mut buffer = String::with_capacity(64);
        matches_to_xml_rec(matches, &mut buffer);
        buffer
    }

    #[test]
    fn sql_select() {
        let g = Grammar::parse(SQL_SELECT_GRAMMAR).expect("valid rules grammar");
        let matches = g
            .tokenize(
                "select field1, field2 from user where a + 1 > 3 and (d <= 4 + 5 * sqrt(21) + 4)",
            )
            .unwrap();
        std::fs::write("test.xml", matches_to_xml(&matches)).unwrap();
    }

    #[test]
    fn sql_select2() {
        let g = Grammar::parse(SQL_SELECT_GRAMMAR).expect("valid rules grammar");
        let matches = g
            .tokenize(
                "select field1, field2 from (select field1, field2 from user where field1 = true)  where field2 = false",
            )
            .unwrap();
        std::fs::write("test2.xml", matches_to_xml(&matches)).unwrap();
    }
}
