use exprs::*;

#[derive(Eq, PartialEq, Debug)]
pub struct Field {
    pub ty:     String,
    pub name:   String
}

#[derive(Eq, PartialEq, Debug)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<Field>,
}

named!(pub field<&str, Field>,
    do_parse!(
        name:   ws!(ident)      >>
                ws!(char!(':')) >>
        ty:     ws!(ident)      >>
        (Field { ty: ty.to_string(), name: name.to_string() })
    )
);

named!(pub parse_struct<&str, Struct>,
    do_parse!(
                ws!(tag!("struct")) >>
        name:   ws!(ident)          >>
                ws!(char!('{'))     >>
        fields: ws!(separated_list!(char!(','), field)) >>
                ws!(char!('}'))     >>
        (Struct { name, fields })
    )
);

pub struct Impl {

}

#[cfg(test)]
mod tests {

    use super::{parse_struct, Struct, Field};

    #[test]
    fn basic_struct() {
        let a = parse_struct(r#"
        struct TestStruct {
            a: i64,
            b: i64,
            c: i64
        }junk"#);
        assert_eq!(a, Ok(("junk",
                          Struct {
                              name: "TestStruct".to_string(),
                              fields: vec![
                                  Field { ty: "i64".to_string(), name: "a".to_string() },
                                  Field { ty: "i64".to_string(), name: "b".to_string() },
                                  Field { ty: "i64".to_string(), name: "c".to_string() },
                              ]
                          }
        )));
    }

}