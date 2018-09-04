fn ident_head_char(c: char) -> bool {
    match c {
        'a'...'z' | 'A'...'Z' | '_'   => true,
        _                           => false
    }
}

fn ident_tail_char(c: char) -> bool {
     match c {
        'a'...'z' | 'A'...'Z' | '0'...'9' | '_' |
        // Unicode emoji ranges... Probably doesn't encompass the entirety of valid emojis, and
        // probably covers some characters that are not emojis or even guaranteed to be valid unicode
        '\u{203C}'...'\u{3299}' | '\u{1F000}'...'\u{1F644}' => true,
        _                                       => false
    }
}

named!(pub ident<&str, String>,
    do_parse!(
        head:  take_while_m_n!(1, 1, ident_head_char)   >>
        tail:   take_while!(ident_tail_char)            >>
        ({
            let mut x = head.to_string();
            x.push_str(tail);
            x
        })
    )
);

