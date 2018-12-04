use pest::error::ErrorVariant;
use pest::iterators::Pairs;
use pest::Parser;

#[derive(Parser)]
#[grammar = "hc.pest"]
pub struct CParser;

#[derive(Debug, Clone)]
pub struct LineInfo {
    /// (min, max, index into filenames)
    pub ranges: Vec<(usize, usize, usize)>,
    pub filenames: Vec<String>,
}

impl LineInfo {
    pub fn new() -> Self { LineInfo { ranges: vec![], filenames: vec![] } }
    pub fn add_file(&mut self, filename: &str, start_line: usize, end_line: usize) {
        let ind = self.filenames.len();
        if start_line != end_line {
            self.filenames.push(filename.to_string());
            self.ranges.push((start_line, end_line, ind));
        }
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

pub fn apply_pre_processor(file: &str, working_dir: &str) -> (String, Vec<String>, LineInfo) {
    use std::process::Command;
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
        } else if ch == '"' {
            in_quote = true;
        }
    }
    string
}

pub fn remove_line_markers(s: String) -> (String, Vec<String>, LineInfo) {
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

use self::Rule::*;
use pest::error::*;

const ERROR_HINTS: &[(&[Rule], &str)] = &[
    (&[gt, lt, gte, lte, equals, neq, lsh, rsh, struct_deref_op, postfix_index, postfix_call, postfix_dot, assign_operator],
    "Are you missing a semicolon or an arithmetic operator?"),
    (&[sizeof_expr, mul_expr],
    "Are you missing an arithmetic operator?"),
    (&[declaration_specifiers, storage_class_specifier, type_specifier],
    "Are you missing a type specifier?"),
    (&[gt, lt, gte, lte, equals, neq, lsh, rsh, assign_operator],
    "Are you missing a right-paren?"),
    (&[abstract_declarator],
    "Are you missing a right-paren or missing a complete type specifier?")
];

pub fn parse<'r>(src: &'r str) -> Result<Pairs<'r, Rule>, (String, (usize, usize))> {
    match CParser::parse(Rule::program, &src[..]) {
        Ok(pairs) => Ok(pairs),
        Err(e) => {
            // let start_pos = match &e.location {
                // InputLocation::Pos(p) => *p,
                // InputLocation::Span((start, _end)) => *start,
            // };
            // let position = pest::Position::new(&src[..], start_pos).unwrap();
            // let (line, col) = position.line_col();
            Err((match &e.variant {
                ErrorVariant::ParsingError { positives, negatives: _s } => {
                    let mut msg = None;
                    for (rules, err_msg) in ERROR_HINTS {
                        if *positives == *rules {
                            msg = Some(err_msg);
                            break
                        }
                    }
                    match msg {
                        None => format!("Expected one of: {:?}; {:?}", positives, _s),
                        Some(err_msg) => format!("Hint: {}\n  Expected one of: {:?}", err_msg, positives)
                    }
                },
                ErrorVariant::CustomError { message } => message.to_string(),
            }, match e.location {
                InputLocation::Pos(a) => (a, a),
                InputLocation::Span((a, b)) => (a, b),
            }))
        }
    }
}