// use nom::{
//     IResult,
//     combinator::{all_consuming, opt},
//     sequence::{preceded, tuple},
// };
// use whitespace::ws;

// pub fn parse_idl(input: &str) {
//     let (control, metadata) = all_consuming(preceded(
//         opt(ws),
//         tuple((control::control_section, metadata::metadata_section)),
//     ))(input)
//     .unwrap();
// }

pub mod comment {
    use nom::{
        IResult,
        branch::alt,
        bytes::complete::tag,
        character::complete::{line_ending, not_line_ending},
        combinator::{map, opt},
        sequence::delimited,
    };

    /// Comment = DocumentationComment / LineComment
    pub fn comment(input: &str) -> IResult<&str, &str> {
        map(
            alt((documentation_comment, line_comment)),
            Option::unwrap_or_default,
        )(input)
    }

    /// DocumentationComment = "///" *NotNL NL
    pub fn documentation_comment(input: &str) -> IResult<&str, Option<&str>> {
        delimited(tag("///"), opt(not_line_ending), line_ending)(input)
    }

    /// LineComment = "//" [(%x09 / %x20-2E / %x30-10FFF) *NotNL] NL ; First character after "//" can't be "/"
    pub fn line_comment(input: &str) -> IResult<&str, Option<&str>> {
        delimited(tag("//"), opt(not_line_ending), line_ending)(input)
    }
}

pub mod whitespace {
    use nom::{
        IResult,
        branch::alt,
        character::complete::{char, line_ending, multispace1, space0},
        combinator::{opt, recognize},
        multi::many1,
        sequence::delimited,
    };

    /// WS = 1*(SP / NL / Comment / Comma) ; whitespace
    pub fn ws(input: &str) -> IResult<&str, Vec<&str>> {
        many1(alt((multispace1, super::comment::comment, comma)))(input)
    }

    /// Comma = ","
    pub fn comma(input: &str) -> IResult<&str, &str> {
        recognize(char(','))(input)
    }

    /// BR = [SP] 1*(Comment / NL) [WS]; line break followed by whitespace
    pub fn br(input: &str) -> IResult<&str, Vec<&str>> {
        delimited(
            space0,
            many1(alt((super::comment::comment, line_ending))),
            opt(ws),
        )(input)
    }
}

pub mod control {
    use nom::{
        IResult,
        character::complete::{char, space0},
        combinator::cut,
        multi::many0,
        sequence::{delimited, preceded, separated_pair, terminated},
    };

    use crate::{
        node_values::{NodeValue, node_object_key, node_value},
        whitespace::br,
    };

    /// ControlSection = *(ControlStatement)
    pub fn control_section(input: &str) -> IResult<&str, Vec<(&str, NodeValue<'_>)>> {
        many0(control_statement)(input)
    }

    /// ControlStatement = "$" NodeObjectKey [SP] ":" [SP] NodeValue BR
    pub fn control_statement(input: &str) -> IResult<&str, (&str, NodeValue<'_>)> {
        preceded(
            char('$'),
            cut(terminated(
                separated_pair(
                    node_object_key,
                    delimited(space0, char(':'), space0),
                    node_value,
                ),
                br,
            )),
        )(input)
    }
}

pub mod metadata {
    use nom::{
        IResult,
        bytes::complete::tag,
        character::complete::{char, space0, space1},
        combinator::cut,
        multi::many0,
        sequence::{delimited, preceded, separated_pair, terminated, tuple},
    };

    use crate::{
        node_values::{NodeValue, node_object_key, node_value},
        whitespace::br,
    };

    /// MetadataSection = *(MetadataStatement)
    pub fn metadata_section(input: &str) -> IResult<&str, Vec<(&str, NodeValue<'_>)>> {
        many0(metadata_statement)(input)
    }

    pub fn metadata_statement(input: &str) -> IResult<&str, (&str, NodeValue<'_>)> {
        preceded(
            tuple((tag("metadata"), space1)),
            cut(terminated(
                separated_pair(
                    node_object_key,
                    delimited(space0, char('='), space0),
                    node_value,
                ),
                br,
            )),
        )(input)
    }
}

pub mod node_values {
    use nom::{
        IResult,
        branch::alt,
        bytes::complete::{tag, take_while_m_n},
        character::complete::{char, line_ending, one_of, space0},
        combinator::{cut, map, opt, recognize},
        multi::{many0, many1, separated_list0},
        number::complete::recognize_float,
        sequence::{delimited, preceded, separated_pair, terminated, tuple},
    };

    use crate::{
        shape_id::{identifier, shape_id},
        whitespace::ws,
    };

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum NodeValue<'a> {
        Array(Vec<NodeValue<'a>>),
        Object(Vec<NodeKeyValuePair<'a>>),
        Number(&'a str),
        Keyword(&'a str),
        String(&'a str),
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct NodeKeyValuePair<'a> {
        pub key: &'a str,
        pub value: NodeValue<'a>,
    }

    /// NodeValue =
    //   NodeArray
    // / NodeObject
    // / Number
    // / NodeKeyword
    // / NodeStringValue
    pub fn node_value(input: &str) -> IResult<&str, NodeValue<'_>> {
        alt((
            map(node_array, NodeValue::Array),
            map(node_object, NodeValue::Object),
            map(recognize_float, NodeValue::Number),
            map(node_keyword, NodeValue::Keyword),
            map(node_string_value, NodeValue::String),
        ))(input)
    }

    /// NodeArray = "[" [WS] *(NodeValue [WS]) "]"
    pub fn node_array(input: &str) -> IResult<&str, Vec<NodeValue<'_>>> {
        preceded(
            char('['),
            cut(terminated(
                preceded(opt(ws), separated_list0(ws, node_value)),
                char(']'),
            )),
        )(input)
    }

    /// NodeObject = "{" [WS] [NodeObjectKvp *(WS NodeObjectKvp)] [WS] "}"
    pub fn node_object(input: &str) -> IResult<&str, Vec<NodeKeyValuePair<'_>>> {
        preceded(
            char('{'),
            cut(terminated(
                preceded(opt(ws), separated_list0(ws, node_object_kvp)),
                char('}'),
            )),
        )(input)
    }

    /// NodeObjectKvp = NodeObjectKey [WS] ":" [WS] NodeValue
    pub fn node_object_kvp(input: &str) -> IResult<&str, NodeKeyValuePair<'_>> {
        map(
            separated_pair(
                node_object_key,
                tuple((opt(ws), char(':'), opt(ws))),
                node_value,
            ),
            |(key, value)| NodeKeyValuePair { key, value },
        )(input)
    }

    /// NodeKeyword = %s"true" / %s"false" / %s"null"
    pub fn node_keyword(input: &str) -> IResult<&str, &str> {
        alt((tag("true"), tag("false"), tag("null")))(input)
    }

    /// NodeStringValue = ShapeId / TextBlock / QuotedText
    pub fn node_string_value(input: &str) -> IResult<&str, &str> {
        alt((shape_id, text_block, quoted_text))(input)
    }

    /// TextBlock = ThreeDquotes [SP] NL *TextBlockContent ThreeDquotes
    pub fn text_block(input: &str) -> IResult<&str, &str> {
        delimited(
            tag(r#"""""#),
            preceded(
                tuple((space0, line_ending)),
                recognize(many0(text_block_content)),
            ),
            tag(r#"""""#),
        )(input)
    }

    // TextBlockContent = QuotedChar / (1*2DQUOTE 1*QuotedChar)
    pub fn text_block_content(input: &str) -> IResult<&str, &str> {
        alt((
            quoted_char,
            preceded(many1(tag(r#""""#)), recognize(many1(quoted_char))),
        ))(input)
    }

    /// NodeObjectKey = QuotedText / Identifier
    pub fn node_object_key(input: &str) -> IResult<&str, &str> {
        alt((quoted_text, identifier))(input)
    }

    /// QuotedText = DQUOTE *QuotedChar DQUOTE
    pub fn quoted_text(input: &str) -> IResult<&str, &str> {
        preceded(
            char('"'),
            cut(terminated(recognize(many0(quoted_char)), char('"'))),
        )(input)
    }

    /// QuotedChar =
    ///     %x09        ; tab
    ///   / %x20-21     ; space - "!"
    ///   / %x23-5B     ; "#" - "["
    ///   / %x5D-10FFFF ; "]"+
    ///   / EscapedChar
    ///   / NL
    pub fn quoted_char(input: &str) -> IResult<&str, &str> {
        alt((
            take_while_m_n(1, 1, |c| matches!(c, '\t' | ' '..='!' | '#'..='[' | ']'..)),
            escaped_char,
            line_ending,
        ))(input)
    }

    /// EscapedChar =
    ///     Escape (Escape / DQUOTE / %s"b" / %s"f"
    ///             / %s"n" / %s"r" / %s"t" / "/"
    ///             / UnicodeEscape)
    pub fn escaped_char(input: &str) -> IResult<&str, &str> {
        preceded(
            char('\\'),
            alt((recognize(one_of(r#"bfnrt\/""#)), unicode_escape)),
        )(input)
    }

    /// UnicodeEscape =     %s"u" Hex Hex Hex Hex
    pub fn unicode_escape(input: &str) -> IResult<&str, &str> {
        preceded(
            char('u'),
            take_while_m_n(4, 4, |c: char| c.is_ascii_hexdigit()),
        )(input)
    }
}

pub mod shape_id {
    use nom::{
        IResult,
        branch::alt,
        bytes::complete::{take_while, take_while1},
        character::complete::{alpha1, alphanumeric1, char},
        combinator::{opt, recognize},
        multi::separated_list1,
        sequence::{preceded, separated_pair, tuple},
    };

    /// ShapeId = RootShapeId [ShapeIdMember]
    pub fn shape_id(input: &str) -> IResult<&str, &str> {
        recognize(tuple((root_shape_id, opt(shape_id_member))))(input)
    }

    /// RootShapeId = AbsoluteRootShapeId / Identifier
    pub fn root_shape_id(input: &str) -> IResult<&str, &str> {
        alt((absolute_root_shape_id, identifier))(input)
    }

    /// AbsoluteRootShapeId = Namespace "#" Identifier
    pub fn absolute_root_shape_id(input: &str) -> IResult<&str, &str> {
        recognize(separated_pair(namespace, char('#'), identifier))(input)
    }

    /// Namespace = Identifier *("." Identifier)
    pub fn namespace(input: &str) -> IResult<&str, &str> {
        recognize(separated_list1(char('.'), identifier))(input)
    }

    /// IdentifierStart *IdentifierChars
    pub fn identifier(input: &str) -> IResult<&str, &str> {
        recognize(tuple((identifier_start, take_while(is_identifier_char))))(input)
    }

    /// IdentifierStart = (1*"_" (ALPHA / DIGIT)) / ALPHA
    pub fn identifier_start(input: &str) -> IResult<&str, &str> {
        alt((preceded(take_while1(|v| v == '_'), alphanumeric1), alpha1))(input)
    }

    /// IdentifierChars = ALPHA / DIGIT / "_"
    pub fn is_identifier_char(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    /// ShapeIdMember = "$" Identifier
    pub fn shape_id_member(input: &str) -> IResult<&str, &str> {
        preceded(char('$'), identifier)(input)
    }
}

#[cfg(test)]
mod test {
    mod control_section {
        use crate::{control::control_section, node_values::NodeValue};

        #[test]
        fn smoke() {
            let (remaining, res) = control_section("$version: \"2.0\"\n$hello: 123\n").unwrap();

            assert_eq!(remaining, "");
            assert_eq!(res, &[
                ("version", NodeValue::String("2.0")),
                ("hello", NodeValue::Number("123")),
            ]);
        }
    }

    mod metadata_section {
        use crate::{metadata::metadata_section, node_values::NodeValue};

        #[test]
        fn smoke() {
            let (remaining, res) = metadata_section(
                "metadata \"foo\" = [\"baz\", \"bar\"]\nmetadata \"qux\" = \"test\"\n",
            )
            .unwrap();

            assert_eq!(remaining, "");
            assert_eq!(res, &[
                (
                    "foo",
                    NodeValue::Array(vec![NodeValue::String("baz"), NodeValue::String("bar")])
                ),
                ("qux", NodeValue::String("test")),
            ]);
        }
    }
}
