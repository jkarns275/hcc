extern crate pest;
use pest::*;

extern crate libhcc;
use libhcc::{ast, parser, visitors::*};

use typecheck::*;
use codegen::*;
use parser::*;
use ast::*;

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
pub enum NotiType {
    Error, Warning, Note, Verbose,
}

impl NotiType {
    pub fn as_ansi_colors(self) -> (u8, u8) {
        (match self {
            NotiType::Error    => Color::Red.as_fg(),
            NotiType::Warning  => Color::Yellow.as_fg(),
            NotiType::Note     => Color::Cyan.as_fg(),
            NotiType::Verbose  => Color::Magenta.as_fg(),
        }, Color::Default.as_bg())
    }

    pub fn as_str(self) -> &'static str {
        match self {
            NotiType::Error    => "error",
            NotiType::Warning  => "warning",
            NotiType::Note     => "note",
            NotiType::Verbose  => "verbose",
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

pub struct NotiPrinter {
    pub program_text: String,
    pub line_info: LineInfo,
    pub lines: Vec<String>,
}

impl NotiPrinter {
    pub fn print_noti(&self, noti_type: NotiType, span: PosSpan, noti_msg: &str) {
        let (line, col) = Span::new(self.program_text.as_str(), span.start, span.end).unwrap()
            .start_pos().line_col();
        let (endline, endcol) = Span::new(self.program_text.as_str(), span.start, span.end).unwrap()
            .end_pos().line_col();
        let (filename, linestart) = self.line_info.get_file_and_line_start(line).unwrap();
        let line_n_str = (line - linestart).to_string();
        let indent = " ".repeat(line_n_str.len() + 2);
        let (fg, bg) = noti_type.as_ansi_colors();
        let lines = noti_msg.lines().collect::<Vec<_>>();
        set_color!(fg, bg);
        print!("{}:", noti_type.as_str());
        reset_color!();
        println!(" {}", lines[0]);
        println!("{}---> {}:{}:{}", indent, filename, line_n_str, col);
        println!("{}|", indent);
        println!(" {} |  {}", line_n_str, &self.lines[line - 1][..]);
        println!("{}|  {}{}\n{}|", indent, " ".to_string().repeat(col - 1), 
            match endcol - col {
                1 | 0 => {
                    "^".to_string()
                },
                x @ _ => {
                    let mut x = x;
                    if line != endline {
                        x = self.lines[line - 1].len() - col;
                    }
                    let mut s = String::with_capacity(x);
                    s.push('^');
                    for _ in 0..x-2 {
                        s.push('~')
                    }
                    s.push('^');
                    s
                }
            }, indent
        );
        if lines.len() > 1 {
            let (fg, bg) = NotiType::Note.as_ansi_colors();
            print!("{}=", indent);
            set_color!(fg, bg);
            print!(" note: ");
            reset_color!();
            println!("{}", lines[1]);
            for i in 2..lines.len() {
                println!("{}", lines[i]);
            }
        }
        println!("");
        //  | {}^
        // lines[line - 1], "-".to_string().repeat(col));
    }
}

pub fn cli_notify(err_msg: &str, noti_type: NotiType) {
    let (fg, bg) = noti_type.as_ansi_colors();
    set_color!(fg, bg);
    print!("{}:", noti_type.as_str());
    reset_color!();
    println!(" {}", err_msg);
}

extern crate argparse;

use argparse::*;
use argparse::ArgumentParser;
use argparse::StoreTrue;
use std::fs;

fn main() {
    let mut check = false;
    let mut verbose = false;
    let mut file: Option<String> = None;
    let mut wd: String = "./".to_string();
    let mut output_file = "a.c".to_string();

    let mut usage = vec![];
    {
        let mut ap = ArgumentParser::new();
        ap.set_description("HolyC Compiler");
        ap.refer(&mut file)
            .add_argument("file", StoreOption, "source file");
        ap.refer(&mut check)
            .add_option(&["-c", "--check"], StoreTrue, "check syntax only");
        ap.refer(&mut verbose)
            .add_option(&["-v", "--verbose"], StoreTrue, "verbose output");
        ap.refer(&mut wd)
            .add_option(&["-w", "--dir"], Store, "working directory");
        ap.refer(&mut output_file)
            .add_option(&["-o", "--output"], Store, "output file");
        ap.print_help("hcc", &mut usage).unwrap();
        ap.parse_args_or_exit();
    }

    if file.is_none() {
        cli_notify("No source file supplied.", NotiType::Note);
        let usage = String::from_utf8_lossy(&usage).to_string();
        println!("{}", usage);
        return
    }

    {
        let src = file.unwrap();
        let (src, lines, line_info) = apply_pre_processor(src.as_str(), &wd);
        let parse_res = parse(&src[..]);
        let noti_printer = NotiPrinter {
            program_text: src.to_string(),
            line_info: line_info.clone(),
            lines: lines.clone(),
        };
        match parse_res {
            Ok(parse_result) => {
                if verbose {
                    cli_notify("parse ok.", NotiType::Note);
                    cli_notify(format!("{:?}", parse_result).as_str(), NotiType::Verbose);
                }
                let mut ast = Ast::from_pairs(parse_result);
                match ast {
                    Ok(ast) => {
                        if verbose {
                            cli_notify("ast construction ok.", NotiType::Note);
                        }
                        match TypeChecker::typecheck(ast) {
                            (Ok(ast), _warnings) => {
                                if verbose {
                                    cli_notify("type check ok.", NotiType::Note);
                                }
                                if check { return }
                                let output = CG::codegen(ast);
                                fs::write(output_file, output).unwrap();
                            },
                            (Err((errors, idstore)), _warnings) => {
                                for error in errors {
                                    if error.ty.is_error() { continue }
                                    let err_str = error.to_string(&lines[..], &idstore, &line_info, &src[..]);
                                    noti_printer.print_noti(NotiType::Error, error.span, &err_str[..])
                                }
                            },
                        }
                    },
                    Err(e) => {
                        for error in e {
                            noti_printer.print_noti(NotiType::Error,
                                                        PosSpan {
                                                            start: error.span.start,
                                                            end: error.span.start },
                                                        error.err_msg.as_str());
                        }
                    },
                }
            },
            Err((string, (start, end))) => {
                noti_printer.print_noti(NotiType::Error, PosSpan { start, end },
                                        string.as_str());
            },
        }
    }
}
