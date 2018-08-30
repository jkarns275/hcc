#[macro_use]
extern crate nom;

mod literals;
mod errors;

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
