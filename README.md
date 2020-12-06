# GrammarRs

This is a Rust library that parses grammars definitions of languages and compiles them in parsers able to recognised that language.

Every grammar is composed by multiple rules, every rule is an identifier followed by a quoted regex. These regexes are implemented directly in the library and they differ from normal regexes because they can reference rules using the syntax ```{RULE_IDENTFIER}```.


For example compiling the following string
```rust
let json_grammar = r#"
        VALUE "\s*({STRING}|{LIST}|{OBJECT}|{BOOLEAN}|{NUMBER})\s*"
        BOOLEAN "true|false"
        NUMBER "(-?0(\.\d+)|0|-?[1-9]\d*(\.\d+)?)"
        STRING "\"(\\.|[^\"])*\""
        LIST "\[{VALUE}(,{VALUE})*\]"
        OBJECT "\{(\s*{STRING}\s*:{VALUE}(,\s*{STRING}\s*:{VALUE})*)?\}"
 "#;

let tokens = Grammar::parse(json_grammar).expect("valid json grammar")
             .tokenize("{\"@\" : 2, \"#\": [true, [4, 3]]  }");
```
creates a parser able to recognize the JSON language, and such parser can construct a syntactic tree of the rules that match the input using the method ```tokenize```. This tree is formed by nodes that keep track of the input matched and all sub rules matched.
