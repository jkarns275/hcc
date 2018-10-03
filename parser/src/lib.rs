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
            if line_number >= start && start < end {
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
    println!("Linemarker: {}", linemarker);
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
            filename = parse_linemarker(line);
            lineinfo.add_file(&filename[..], last_end, line_number);
            last_end = line_number;
            continue;
        }
        lines.push(line.to_string());
        println!("{}. {}", line_number, line);
        line_number += 1;
        filtered.push_str(line);
        filtered.push('\n');
    }
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

    const _parser_text: &'static str = include_str!("c.pest");

    const binary_search_example: &'static str = include_str!("../../examples/binary_search.c");
    const binary_tree_example: &'static str = include_str!("../../examples/binary_tree.c");

    #[test]
    fn oofc() {
        let (src, lines, line_info) = apply_pre_processor("binary_tree.c", "../examples");
        use pest;
        use pest::error::{InputLocation, ErrorVariant};
        use std::mem::transmute;
        struct Junk<'a> {
            pub input: &'a [u8],
            pub pos: usize,
        }
        println!("{:?}", line_info);
        match CParser::parse(Rule::program, &src) {
            Err(e) => {
                let start_pos = match &e.location {
                    InputLocation::Pos(p) => *p,
                    InputLocation::Span((start, _end)) => *start,
                };
                let bytes = src.as_bytes();
                let position = unsafe { transmute::<Junk, pest::Position>(Junk { input: bytes, pos: start_pos }) };
                let (line, col) = position.line_col();
                println!("Calculated line and column: {}, {}", line, col);
                let err_msg = match e.variant {
                    ErrorVariant::ParsingError { positives, negatives } => format!("Expected one of: {:?}", positives),
                    ErrorVariant::CustomError { message } => message
                };
                if let Some((filename, linestart)) = line_info.get_file_and_line_start(line) {
                    println!(r#"Encountered error in file "{}" {}:{}:
  |
  |  {}
  | {}^
  {}
"#, filename, line - linestart - 2, col, lines[line - 2], "-".to_string().repeat(col), err_msg);
                }
            },
            Ok(parsed) => {
                println!("{:#?}", parsed);
            },
        };
    }
}
