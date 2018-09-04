use errors;
use nom::ErrorKind;

fn is_hex_digit(c: char) -> bool {
    match c {
        x if x >= 'a' && x <= 'f' => true,
        x if x >= 'A' && x <= 'F' => true,
        x if x >= '0' && x <= '9' => true,
        _ => false
    }
}

fn is_dec_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

named!(pub hex_lit<&str, i64>,
    do_parse!(
        sign:       opt!(ws!(char!('-')))                   >>
                    pair!(char!('0'), one_of!("xX"))        >>
        digits:     return_error!(ErrorKind::Custom(errors::INVALID_INTEGER_LITERAL),
                    take_while_m_n!(1, 16, is_hex_digit))   >>
        ({
            match i64::from_str_radix(digits, 16) {
                Ok(item) => if sign.is_some() { -item } else { item },
                _ => unreachable!()
            }
        })
    )
);

named!(pub bin_lit<&str, i64>,
    do_parse!(
        sign:       opt!(ws!(char!('-')))                                       >>
                    pair!(char!('0'), one_of!("bB"))                            >>
        digits:     take_while1!(|x| (x as u8).wrapping_sub('0' as u8) <= 1)    >>
        ({
            match i64::from_str_radix(digits, 2) {
                Ok(item) => if sign.is_some() { -item } else { item },
                _ => unreachable!()
            }
        })
    )
);

named!(pub dec_lit<&str, i64>,
    do_parse!(
        sign:   opt!(ws!(char!('-')))       >>
        digits: return_error!(ErrorKind::Custom(errors::INVALID_INTEGER_LITERAL),
                take_while1!(is_dec_digit)) >>
        ({
            match i64::from_str_radix(digits, 10) {
                Ok(item) => if sign.is_some() { -item } else { item },
                _ => unreachable!()
            }
        })
    )
);

named!(pub int_literal<&str, i64>, alt!(hex_lit | bin_lit | dec_lit));

/// Hacky shortcut to parsing bools. The only thing that will be passed to ths fn is the string
/// 'true' or the string 'false', so this works (since the parse in a sense guarantees that)
fn str_to_bool(s: &str) -> bool { s.starts_with("t") }

named!(pub bool_literal<&str, bool>, map!(alt!(tag!("true") | tag!("false")), str_to_bool));

#[derive(PartialEq, Eq, Debug)]
pub enum ArrayLiteral<T: Clone> {
    /// Represents a manually typed out array.
    /// e.g. `[0, 1, 2, 3]` would result in `Long(vec[0, 1, 2, 3])`
    Long(Vec<T>),

    /// Represents a shorthand array of a certain value; (value, len).
    /// e.g. `[10; 32]` would result in an array of 32 elements all set to 10
    Short(T, i64),
}

named!(pub int_array_literal_long<&str, ArrayLiteral<i64>>,
    do_parse!(
                    ws!(char!('['))                                 >>
        values:     separated_list!(ws!(char!(',')), int_literal)   >>
                    ws!(char!(']'))                                 >>
        (ArrayLiteral::Long(values))
    )
);

named!(pub int_array_literal_short<&str, ArrayLiteral<i64>>,
    do_parse!(
                    ws!(char!('['))     >>
        value:      int_literal         >>
                    ws!(char!(';'))     >>
        len:        int_literal         >>
                    ws!(char!(']'))     >>
        (ArrayLiteral::Short(value, len))
    )
);

named!(pub bool_array_literal_long<&str, ArrayLiteral<bool>>,
    do_parse!(
                    ws!(char!('['))                                 >>
        bools:      separated_list!(ws!(char!(',')), bool_literal)  >>
                    ws!(char!(']'))                                 >>
        (ArrayLiteral::Long(bools))
    )
);

named!(pub bool_array_literal_short<&str, ArrayLiteral<bool>>,
    do_parse!(
                    ws!(char!('['))     >>
        value:      bool_literal        >>
                    ws!(char!(';'))     >>
        len:        int_literal         >>
                    ws!(char!(']'))     >>
        (ArrayLiteral::Short(value, len))
    )
);

#[cfg(test)]
mod tests {
    use literals::*;

    /// ## Important Note!
    /// Many of the tests here have extra white space or junk characters at the end of them. This is
    /// because nom  will keep parsing until it has reason to believe it is not done. For example,
    /// when parsing a list of digits "1234" it will give an error because it isn't sure that it is
    /// has all of the digits / hit the end of the integer. Adding a space (as long as `ws!` wasn't
    /// used) or some other junk character will allow it to parse

    #[test]
    fn parse_decimal_int() {
        assert_eq!(int_literal("4 "), Ok((" ", 4)));
        assert_eq!(int_literal("400 "), Ok((" ", 400)));
        assert_eq!(int_literal("-400000000000123 "), Ok((" ", -400000000000123)));
    }

    #[test]
    fn parse_hex_int() {
        assert_eq!(int_literal("0x4 "), Ok((" ", 4)));
        assert_eq!(int_literal("0xBEEF "), Ok((" ", 0xBEEF)));
        assert_eq!(int_literal("-0xBEEF "), int_literal("-0XBEEF "));
    }

    #[test]
    fn parse_binary_int() {
        assert_eq!(int_literal("0b100 "), Ok((" ", 0b100)));
        assert_eq!(int_literal("-0b100 "), Ok((" ", -0b100)));
    }

    #[test]
    fn parse_int_short_array() {
        assert_eq!(int_array_literal_short("[-0b1010; 100] junk"),
                   Ok(("junk", ArrayLiteral::Short(-0b1010, 100))));
    }

    #[test]
    fn parse_int_long_array() {
        assert_eq!(int_array_literal_long("[1, 2, 3, 0x4, 0X5, 0b110, 0B111, -8] junk"),
                   Ok(("junk", ArrayLiteral::Long(vec![1, 2, 3, 4, 5, 6, 7,-8]))));
    }

    #[test]
    fn parse_bool_long_array() {
        assert_eq!(bool_array_literal_long("[true, false, true, true] junk"),
                   Ok(("junk", ArrayLiteral::Long(vec![true, false, true, true]))));
    }

    #[test]
    fn parse_bool_short_array() {
        assert_eq!(bool_array_literal_short("[false; 100] junk"),
                   Ok(("junk", ArrayLiteral::Short(false, 100))));
            assert_eq!(bool_array_literal_short("[true; 100] junk"),
                   Ok(("junk", ArrayLiteral::Short(true, 100))));
    }
}