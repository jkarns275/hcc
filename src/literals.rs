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
        _prefix:    pair!(char!('0'), one_of!("xX"))        >>
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
        sign:       opt!(ws!(char!('-')))                           >>
        _prefix:    pair!(char!('0'), one_of!("bB"))                >>
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
        _bopen:     ws!(char!('['))                                 >>
        values:     separated_list!(ws!(char!(',')), int_literal)   >>
        _bclose:    ws!(char!(']'))                                 >>
        (ArrayLiteral::Long(values))
    )
);

named!(pub int_array_literal_short<&str, ArrayLiteral<i64>>,
    do_parse!(
        _bopen:     ws!(char!('['))     >>
        value:      int_literal         >>
        _semicolon: ws!(char!(';'))     >>
        len:        int_literal         >>
        _bclose:    ws!(char!(']'))     >>
        (ArrayLiteral::Short(value, len))
    )
);

named!(pub bool_array_literal_long<&str, ArrayLiteral<bool>>,
    do_parse!(
        _bopen:     ws!(char!('['))                             >>
        bools:      separated_list!(ws!(char!(',')), bool_literal)   >>
        _bclose:    ws!(char!(']'))                             >>
        (ArrayLiteral::Long(bools))
    )
);

named!(pub bool_array_literal_short<&str, ArrayLiteral<bool>>,
    do_parse!(
        _bopen:     ws!(char!('['))     >>
        value:      bool_literal        >>
        _semicolon: ws!(char!(';'))     >>
        len:        int_literal         >>
        _bclose:    ws!(char!(']'))     >>
        (ArrayLiteral::Short(value, len))
    )
);
