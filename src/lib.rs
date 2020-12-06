mod charset;
mod grammar;
mod parser;
mod pattern;
mod token;

pub use crate::grammar::Grammar;
pub use parser::errors;
pub use pattern::ErrorEntry;
pub type TokenizeErrors<'a, 'b> = Box<[ErrorEntry<'a, 'b>]>;
pub use pattern::Pattern;
pub use token::Token;

#[cfg(test)]
mod regex_tests {
    use super::pattern::Pattern;

    #[test]
    fn test_regex() {
        let positive_tests = vec![
            vec!["", ""],
            vec!["a?", "", "a"],
            vec!["a*", "", "a", "aa", "aaa", "aaaaa"],
            vec!["a+", "a", "aa", "aaa", "aaaaaaaaa"],
            vec!["ab+c", "abc", "abbbc"],
            vec!["\\d{2,4}", "11", "111", "1111"],
            vec!["[0123]\\d-[01]\\d-\\d\\d", "00-00-00", "33-13-99"],
            vec!["[0-23]\\d-[0-1]\\d-\\d\\d", "00-00-00", "33-13-99"],
            vec!["[0-23]\\d-[0-1]\\d-\\d{2, 2}", "00-00-00", "33-13-99"],
            vec!["[^\\s@]+@.+", "ciao.bello@gmail.com", "user@example.it"],
            vec!["a(bcd(e|fg))+", "abcde", "abcdfg", "abcdebcdfg"],
            vec![
                "(3[01]|[12]\\d|0[1-9])-(1[0-2]|0[1-9])-\\d{2}",
                "01-10-00",
                "31-12-99",
                "01-01-99",
            ],
        ];
        for test in positive_tests {
            if let Ok(regex) = Pattern::parse(test[0]) {
                for s in test.iter().skip(1) {
                    if !regex.matches(s) {
                        panic!("regex '{}' didn't match valid input '{}'", regex, s);
                    }
                }
            } else {
                panic!(format!("invalid regex '{}'", test[0]));
            }
        }
        let negative_tests = vec![
            vec!["ab+c", "ac", "aabc", "abcc"],
            vec!["\\d{2,4}", "1", "11111"],
            vec!["[0123]\\d-[01]\\d-\\d\\d", "40-00-00", "03-23-99"],
            vec!["[0-23]\\d-[0-1]\\d-\\d\\d", "40-00-00", "03-23-99"],
            vec![
                "[0-23]\\d-[0-1]\\d-\\d{2, 2}",
                "40-00-00",
                "03-23-99",
                "00-00-111",
            ],
            vec![
                "(3[01]|[12]\\d|0[1-9])-(1[0-2]|0[1-9])-\\d{2}",
                "33-11-99",
                "30-13-99",
                "00-12-99",
                "10-00-99",
                "10-10-999",
            ],
            vec!["[^\\s@]+@.+", "ciao.bellogmail.com", "user@"],
            vec!["", "a"],
            vec!["a?", "aa", "aaa"],
            vec!["a*", "b"],
            vec!["a+", ""],
        ];
        for test in negative_tests {
            if let Ok(regex) = Pattern::parse(test[0]) {
                for s in test.iter().skip(1) {
                    if regex.matches(s) {
                        panic!("regex '{}' matched invalid input '{}'", regex, s);
                    }
                }
            } else {
                panic!(format!("invalid regex '{}'", test[0]));
            }
        }
    }
}
