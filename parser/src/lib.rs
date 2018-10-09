#[macro_use]
extern crate nom;

extern crate pest;

#[macro_use]
extern crate pest_derive;

use pest::iterators::*;
use pest::*;

#[derive(Parser)]
#[grammar = "c.pest"]
struct CParser;

#[derive(Debug)]
struct LineInfo {
    /// (min, max, index into filenames)
    ranges: Vec<(usize, usize, usize)>,
    filenames: Vec<String>,
}

impl LineInfo {
    pub fn new() -> Self { LineInfo { ranges: vec![], filenames: vec![] } }
    pub fn add_file(&mut self, filename: &str, start_line: usize, end_line: usize) {
        let ind = self.filenames.len();
        self.filenames.push(filename.to_string());
        self.ranges.push((start_line, end_line, ind));
    }
    pub fn get_file_and_line_start<'a>(&'a self, line_number: usize) -> Option<(&'a str, usize)> {
        for &(start, end, index) in self.ranges.iter() {
            if line_number >= start && line_number < end {
                return Some((&self.filenames[index][..], start))
            }
        }
        None
    }
}

fn apply_pre_processor(file: &str, working_dir: &str) -> (String, Vec<String>, LineInfo) {
    use std::process::{Command, Stdio};
    let result = String::from_utf8_lossy(
        &Command::new("cpp")
            .arg(file)
            .current_dir(working_dir)
            .output()
            .unwrap().stdout[..])
        .to_string();
    remove_line_markers(result)
}

fn parse_linemarker(linemarker: &str) -> String {
    let mut string = String::with_capacity(linemarker.len());
    let mut in_quote = false;
    for ch in linemarker.chars() {
        if in_quote {
            if ch == '"' {
                return string;
            }
            string.push(ch);
        } else {
            if ch == '"' {
                in_quote = true;
                continue
            }
        }
    }
    string
}

fn remove_line_markers(s: String) -> (String, Vec<String>, LineInfo) {
    let mut lineinfo = LineInfo::new();
    let mut filtered = String::with_capacity(s.len());
    let mut line_number = 0;
    let mut last_end = 0;
    let mut filename = "".to_string();
    let mut lines = vec![];
    for line in s.lines() {
        if let Some('#') = line.chars().next() {
            lineinfo.add_file(&filename[..], last_end, line_number);
            filename = parse_linemarker(line);
            last_end = line_number;
            continue;
        }
        lines.push(line.to_string());
        line_number += 1;
        filtered.push_str(line);
        filtered.push('\n');
    }
    lineinfo.add_file(&filename[..], last_end, line_number);
    (filtered, lines, lineinfo)
}


use std::path::Path;
/*
fn parse<F: Into<Path>, F2>(file: F, working_dir: F2) -> Result<Pairs<Rule>, Error<Rule>>
    where   F2: Into<Path>,
            F:  Into<Path> {
    let (src, line_info) = apply_pre_processor(
                file.into().as_str().unwrap(),
                working_dir.into().as_str().unwrap());
    let result = CParser::parse(Rule::program, &src);

    match result {
        Err(e) => {

        }
    }
}*/

#[cfg(test)]
mod test {
    use pest::Parser;
    use super::{ CParser, Rule, apply_pre_processor };
    use Rule::*;

    const _parser_text: &'static str = include_str!("c.pest");

    const binary_search_example: &'static str = include_str!("../../examples/binary_search.c");
    const binary_tree_example: &'static str = include_str!("../../examples/binary_tree.c");

    const special_errors: &'static [(&'static [Rule], &'static str)] = &[
        (&[gt, lt, gte, lte, equals, neq, lsh, rsh, struct_deref_op, postfix_index, postfix_call, postfix_dot, assign_operator],
        "Are you missing a semicolon or an arithmetic operator?"),
        (&[sizeof_expr, mul_expr],
        "Are you missing an arithmetic operator?"),
        (&[declaration_specifiers, storage_class_specifier, type_specifier, type_qualifier],
        "Are you missing a type specifier?"),
        (&[gt, lt, gte, lte, equals, neq, lsh, rsh, assign_operator],
        "Are you missing a right-paren?"),
        (&[abstract_declarator],
        "Are you missing a right-paren or missing a complete type specifier?")
    ];

    #[test]
    fn oofc() {
        let (src, lines, line_info) = apply_pre_processor("binary_tree.c", "../examples");
        use pest;
        use pest::error::{InputLocation, ErrorVariant};
        use std::mem::transmute;
        match CParser::parse(Rule::program, &src) {
            Err(e) => {
                let start_pos = match &e.location {
                    InputLocation::Pos(p) => *p,
                    InputLocation::Span((start, _end)) => *start,
                };
                let position = pest::Position::new(&src[..], start_pos).unwrap();
                let (line, col) = position.line_col();
                let err_msg = match e.variant {
                    ErrorVariant::ParsingError { positives, negatives } => {
                        let mut msg = None;
                        for (rules, err_msg) in special_errors {
                            if positives == *rules {
                                msg = Some(err_msg);
                            }
                        }
                        match msg {
                            None => format!("Expected one of: {:?}", positives),
                            Some(err_msg) => format!("Hint: {}\n  Expected one of: {:?}", err_msg, positives)
                        }
                    },
                    ErrorVariant::CustomError { message } => message
                };
                if let Some((filename, linestart)) = line_info.get_file_and_line_start(line) {
                    println!(r#"Encountered error in file "{}" {}:{}:
  |
  |  {}
  | {}^
  {}
"#, filename, line - linestart + 1, col, lines[line - 1], "-".to_string().repeat(col), err_msg);
                }
            },
            Ok(parsed) => {
                println!("{:#?}", parsed);
            },
        };
    }
}
