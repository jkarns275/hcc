use ast;
use parser;
mod lib;
use lib::ast;
use lib::parser;
use parser::LineInfo;
use ast::PosSpan;
use pest::Span;

#[repr(u8)]
#[derive(Copy, Clone)]
pub enum Color {
    Black = 0,
    Red = 1,
    Green = 2,
    Yellow = 3,
    Blue = 4,
    Magenta = 5,
    Cyan = 6,
    White = 7,
    Default = 9,
}

impl Color {
    pub fn as_fg(self) -> u8 { self as u8 + 30 }
    pub fn as_bg(self) -> u8 { self as u8 + 40 }
}

#[derive(Copy, Clone)]
pub enum IssueType {
    Error, Warning, Note,
}

impl IssueType {
    pub fn as_ansi_colors(self) -> (u8, u8) {
        (match self {
            IssueType::Error    => Color::Red.as_fg(),
            IssueType::Warning  => Color::Yellow.as_fg(),
            IssueType::Note     => Color::Cyan.as_fg(),
        }, Color::Default.as_bg())
    }

    pub fn as_str(self) -> &'static str {
        match self {
            IssueType::Error    => "error",
            IssueType::Warning  => "warning",
            IssueType::Note     => "note",
        }
    }
}

const CSI: &'static str = "\x1b[";

macro_rules! set_color {
    ( $fg:expr, $bg:expr ) => ({ print!("{}{}m", CSI, $fg); print!("{}{}m", CSI, $bg); })
}

macro_rules! reset_color {
    () => ({ print!("{}{}m", CSI, Color::Default.as_fg()); print!("{}{}m", CSI, Color::Default.as_bg()); })
}

pub struct IssuePrinter {
    pub program_text: String,
    pub line_info: LineInfo,
    pub lines: Vec<String>,
}
impl IssuePrinter {
    pub fn print_issue(&self, issue_type: IssueType, span: PosSpan, err_msg: &str) {
        let (line, col) = Span::new(self.program_text.as_str(), span.start, span.end).unwrap()
            .start_pos().line_col();
        let (filename, linestart) = self.line_info.get_file_and_line_start(line).unwrap();
        let line_n_str = (line - linestart).to_string();
        let indent = " ".repeat(line_n_str.len() + 2);
        let (fg, bg) = issue_type.as_ansi_colors();
        set_color!(fg, bg);
        print!("{}:", issue_type.as_str());
        reset_color!();
        println!(" {}", err_msg);
        println!("{}---> {}:{}:{}", indent, filename, line_n_str, col);
        println!("{}|", indent);
        println!(" {} |  {}", line_n_str, self.lines[line - 1]);
        //  | {}^
        // lines[line - 1], "-".to_string().repeat(col));
    }
}

extern crate argparse;

use argparse::*;
use argparse::ArgumentParser;
use argparse::StoreTrue;

fn main() {
    let mut check = false;

    {
        let mut ap = ArgumentParser::new();
        ap.set_description("HolyC");
        ap.refer(&mut check)
            .add_option(&["-c", "--check"], StoreTrue, "check syntax only");
    }

}