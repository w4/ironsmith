#![deny(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]

use nom::{
    combinator::{all_consuming, opt},
    sequence::{preceded, tuple},
};
use whitespace::ws;

pub enum Error<'a> {
    Nom(nom::Err<nom::error::Error<&'a str>>),
}

// TODO: integrate with https://docs.rs/nom_locate and https://docs.rs/nom-supreme to provide better errors.
pub fn parse_idl(input: &str) -> Result<(), Error<'_>> {
    let (_, (_control, _metadata)) = all_consuming(preceded(
        opt(ws),
        tuple((control::control_section, metadata::metadata_section)),
    ))(input)
    .map_err(Error::Nom)?;

    Ok(())
}

pub mod comment {
    use nom::{
        IResult,
        branch::alt,
        bytes::complete::tag,
        character::complete::{line_ending, not_line_ending},
        combinator::{map, opt},
        sequence::delimited,
    };

    /// ```
    /// Comment = DocumentationComment / LineComment
    /// ```
    pub fn comment(input: &str) -> IResult<&str, &str> {
        map(
            alt((documentation_comment, line_comment)),
            Option::unwrap_or_default,
        )(input)
    }

    /// ```
    /// DocumentationComment = "///" *NotNL NL
    /// ```
    pub fn documentation_comment(input: &str) -> IResult<&str, Option<&str>> {
        delimited(tag("///"), opt(not_line_ending), line_ending)(input)
    }

    /// ```
    /// LineComment = "//" [(%x09 / %x20-2E / %x30-10FFF) *NotNL] NL ; First character after "//" can't be "/"
    /// ```
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

    /// ```
    /// WS = 1*(SP / NL / Comment / Comma) ; whitespace
    /// ```
    pub fn ws(input: &str) -> IResult<&str, Vec<&str>> {
        many1(alt((multispace1, super::comment::comment, comma)))(input)
    }

    /// ```
    /// Comma = ","
    /// ```
    pub fn comma(input: &str) -> IResult<&str, &str> {
        recognize(char(','))(input)
    }

    /// ```
    /// BR = [SP] 1*(Comment / NL) [WS]; line break followed by whitespace
    /// ```
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

    /// ```
    /// ControlSection = *(ControlStatement)
    /// ```
    pub fn control_section(input: &str) -> IResult<&str, Vec<(&str, NodeValue<'_>)>> {
        many0(control_statement)(input)
    }

    /// ```
    /// ControlStatement = "$" NodeObjectKey [SP] ":" [SP] NodeValue BR
    /// ```
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

    /// ```
    /// MetadataSection = *(MetadataStatement)
    /// ```
    pub fn metadata_section(input: &str) -> IResult<&str, Vec<(&str, NodeValue<'_>)>> {
        many0(metadata_statement)(input)
    }

    /// ```
    /// MetadataStatement = %s"metadata" SP NodeObjectKey [SP] "=" [SP] NodeValue BR
    /// ```
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
        Keyword(Keyword),
        String(&'a str),
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct NodeKeyValuePair<'a> {
        pub key: &'a str,
        pub value: NodeValue<'a>,
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Keyword {
        True,
        False,
        Null,
    }

    /// ```
    /// NodeValue =
    //   NodeArray
    // / NodeObject
    // / Number
    // / NodeKeyword
    // / NodeStringValue
    // ```
    pub fn node_value(input: &str) -> IResult<&str, NodeValue<'_>> {
        alt((
            map(node_array, NodeValue::Array),
            map(node_object, NodeValue::Object),
            map(recognize_float, NodeValue::Number),
            map(node_keyword, NodeValue::Keyword),
            map(node_string_value, NodeValue::String),
        ))(input)
    }

    /// ```
    /// NodeArray = "[" [WS] *(NodeValue [WS]) "]"
    /// ```
    pub fn node_array(input: &str) -> IResult<&str, Vec<NodeValue<'_>>> {
        preceded(
            char('['),
            cut(terminated(
                preceded(opt(ws), separated_list0(ws, node_value)),
                char(']'),
            )),
        )(input)
    }

    /// ```
    /// NodeObject = "{" [WS] [NodeObjectKvp *(WS NodeObjectKvp)] [WS] "}"
    /// ```
    pub fn node_object(input: &str) -> IResult<&str, Vec<NodeKeyValuePair<'_>>> {
        preceded(
            char('{'),
            cut(terminated(
                delimited(opt(ws), separated_list0(ws, node_object_kvp), opt(ws)),
                char('}'),
            )),
        )(input)
    }

    /// ```
    /// NodeObjectKvp = NodeObjectKey [WS] ":" [WS] NodeValue
    /// ```
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

    /// ```
    /// NodeKeyword = %s"true" / %s"false" / %s"null"
    /// ```
    pub fn node_keyword(input: &str) -> IResult<&str, Keyword> {
        alt((
            map(tag("true"), |_| Keyword::True),
            map(tag("false"), |_| Keyword::False),
            map(tag("null"), |_| Keyword::Null),
        ))(input)
    }

    /// ```
    /// NodeStringValue = ShapeId / TextBlock / QuotedText
    /// ```
    pub fn node_string_value(input: &str) -> IResult<&str, &str> {
        alt((shape_id, text_block, quoted_text))(input)
    }

    /// ```
    /// TextBlock = ThreeDquotes [SP] NL *TextBlockContent ThreeDquotes
    /// ```
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

    // ```
    // TextBlockContent = QuotedChar / (1*2DQUOTE 1*QuotedChar)
    // ```
    pub fn text_block_content(input: &str) -> IResult<&str, &str> {
        alt((
            quoted_char,
            preceded(many1(tag(r#""""#)), recognize(many1(quoted_char))),
        ))(input)
    }

    /// ```
    /// NodeObjectKey = QuotedText / Identifier
    /// ```
    pub fn node_object_key(input: &str) -> IResult<&str, &str> {
        alt((quoted_text, identifier))(input)
    }

    /// ```
    /// QuotedText = DQUOTE *QuotedChar DQUOTE
    /// ```
    pub fn quoted_text(input: &str) -> IResult<&str, &str> {
        preceded(
            char('"'),
            cut(terminated(recognize(many0(quoted_char)), char('"'))),
        )(input)
    }

    /// ```
    /// QuotedChar =
    ///     %x09        ; tab
    ///   / %x20-21     ; space - "!"
    ///   / %x23-5B     ; "#" - "["
    ///   / %x5D-10FFFF ; "]"+
    ///   / EscapedChar
    ///   / NL
    /// ```
    pub fn quoted_char(input: &str) -> IResult<&str, &str> {
        alt((
            take_while_m_n(1, 1, |c| matches!(c, '\t' | ' '..='!' | '#'..='[' | ']'..)),
            escaped_char,
            line_ending,
        ))(input)
    }

    /// ```
    /// EscapedChar =
    ///     Escape (Escape / DQUOTE / %s"b" / %s"f"
    ///             / %s"n" / %s"r" / %s"t" / "/"
    ///             / UnicodeEscape)
    /// ```
    pub fn escaped_char(input: &str) -> IResult<&str, &str> {
        preceded(
            char('\\'),
            alt((recognize(one_of(r#"bfnrt\/""#)), unicode_escape)),
        )(input)
    }

    /// ```
    /// UnicodeEscape =     %s"u" Hex Hex Hex Hex
    /// ```
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

    /// ```
    /// ShapeId = RootShapeId [ShapeIdMember]
    /// ```
    pub fn shape_id(input: &str) -> IResult<&str, &str> {
        recognize(tuple((root_shape_id, opt(shape_id_member))))(input)
    }

    /// ```
    /// RootShapeId = AbsoluteRootShapeId / Identifier
    /// ```
    pub fn root_shape_id(input: &str) -> IResult<&str, &str> {
        alt((absolute_root_shape_id, identifier))(input)
    }

    /// ```
    /// AbsoluteRootShapeId = Namespace "#" Identifier
    /// ```
    pub fn absolute_root_shape_id(input: &str) -> IResult<&str, &str> {
        recognize(separated_pair(namespace, char('#'), identifier))(input)
    }

    /// ```
    /// Namespace = Identifier *("." Identifier)
    /// ```
    pub fn namespace(input: &str) -> IResult<&str, &str> {
        recognize(separated_list1(char('.'), identifier))(input)
    }

    /// ```
    /// IdentifierStart *IdentifierChars
    /// ```
    pub fn identifier(input: &str) -> IResult<&str, &str> {
        recognize(tuple((identifier_start, take_while(is_identifier_char))))(input)
    }

    /// ```
    /// IdentifierStart = (1*"_" (ALPHA / DIGIT)) / ALPHA
    /// ```
    pub fn identifier_start(input: &str) -> IResult<&str, &str> {
        alt((preceded(take_while1(|v| v == '_'), alphanumeric1), alpha1))(input)
    }

    /// ```
    /// IdentifierChars = ALPHA / DIGIT / "_"
    /// ```
    #[must_use]
    pub fn is_identifier_char(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    /// ```
    /// ShapeIdMember = "$" Identifier
    /// ```
    pub fn shape_id_member(input: &str) -> IResult<&str, &str> {
        preceded(char('$'), identifier)(input)
    }
}

pub mod shapes {
    use nom::{
        IResult,
        branch::alt,
        bytes::complete::tag,
        character::complete::{char, space0, space1},
        combinator::{cut, map, opt},
        multi::{many0, separated_list0, separated_list1},
        sequence::{delimited, preceded, separated_pair, tuple},
    };

    use crate::{
        node_values::{NodeKeyValuePair, NodeValue, node_object, node_value},
        shape_id::{absolute_root_shape_id, identifier, namespace, shape_id},
        traits::{ApplyStatement, Trait, apply_statement, trait_statements},
        whitespace::{br, comma, ws},
    };

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct ShapeSection<'a> {
        pub namespace: &'a str,
        pub uses: Vec<&'a str>,
        pub shapes: Vec<ShapeOrApply<'a>>,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum ShapeOrApply<'a> {
        Shape(ShapeWithTraits<'a>),
        Apply(ApplyStatement<'a>),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct ShapeWithTraits<'a> {
        pub traits: Vec<Trait<'a>>,
        pub shape: Shape<'a>,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Shape<'a> {
        Simple(SimpleShape<'a>),
        Enum(EnumShape<'a>),
        Aggregate(AggregateShape<'a>),
        Entity(EntityShape<'a>),
        Operation(OperationShape<'a>),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct SimpleShape<'a> {
        pub type_name: SimpleTypeName,
        pub identifier: &'a str,
        pub mixins: Vec<&'a str>,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum SimpleTypeName {
        Blob,
        Boolean,
        Document,
        String,
        Byte,
        Short,
        Integer,
        Long,
        Float,
        Double,
        BigInteger,
        BigDecimal,
        Timestamp,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct EnumShape<'a> {
        pub type_name: EnumTypeName,
        pub identifier: &'a str,
        pub mixins: Vec<&'a str>,
        pub members: Vec<EnumShapeMember<'a>>,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum EnumTypeName {
        Enum,
        IntEnum,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct EnumShapeMember<'a> {
        pub traits: Vec<Trait<'a>>,
        pub identifier: &'a str,
        pub value: Option<NodeValue<'a>>,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct AggregateShape<'a> {
        pub type_name: AggregateTypeName,
        pub identifier: &'a str,
        pub for_resource: Option<&'a str>,
        pub mixins: Vec<&'a str>,
        pub members: Vec<ShapeMember<'a>>,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum AggregateTypeName {
        Structure,
        Union,
        Map,
        List,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct ShapeMember<'a> {
        pub traits: Vec<Trait<'a>>,
        pub identifier: &'a str,
        pub shape_id: Option<&'a str>,
        pub value: Option<NodeValue<'a>>,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct EntityShape<'a> {
        pub type_name: EntityTypeName,
        pub identifier: &'a str,
        pub mixins: Vec<&'a str>,
        pub nodes: Vec<NodeKeyValuePair<'a>>,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum EntityTypeName {
        Resource,
        Service,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct OperationShape<'a> {
        pub identifier: &'a str,
        pub mixins: Vec<&'a str>,
        pub body: Vec<OperationProperty<'a>>,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum OperationProperty<'a> {
        Input(OperationPropertyShape<'a>),
        Output(OperationPropertyShape<'a>),
        Errors(Vec<&'a str>),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum OperationPropertyShape<'a> {
        Explicit(&'a str),
        Inline(InlineAggregateShape<'a>),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct InlineAggregateShape<'a> {
        pub traits: Vec<Trait<'a>>,
        pub for_resource: Option<&'a str>,
        pub mixins: Vec<&'a str>,
        pub members: Vec<ShapeMember<'a>>,
    }

    /// ```
    /// ShapeSection = [NamespaceStatement UseSection [ShapeStatements]]
    /// ```
    pub fn shape_section(input: &str) -> IResult<&str, Option<ShapeSection<'_>>> {
        opt(map(
            tuple((namespace_statement, use_section, shape_statements)),
            |(namespace, uses, shapes)| ShapeSection {
                namespace,
                uses,
                shapes,
            },
        ))(input)
    }

    /// ```
    /// NamespaceStatement = %s"namespace" SP Namespace BR
    /// ```
    pub fn namespace_statement(input: &str) -> IResult<&str, &str> {
        preceded(tag("namespace"), cut(delimited(space1, namespace, br)))(input)
    }

    /// ```
    /// UseSection = *(UseStatement)
    /// ```
    pub fn use_section(input: &str) -> IResult<&str, Vec<&str>> {
        many0(use_statement)(input)
    }

    /// ```
    /// UseStatement = %s"use" SP AbsoluteRootShapeId BR
    /// ```
    pub fn use_statement(input: &str) -> IResult<&str, &str> {
        preceded(
            tag("use"),
            cut(delimited(space1, absolute_root_shape_id, br)),
        )(input)
    }

    /// ```
    /// ShapeOrApplyStatement *(BR ShapeOrApplyStatement)
    /// ```
    pub fn shape_statements(input: &str) -> IResult<&str, Vec<ShapeOrApply<'_>>> {
        separated_list0(br, shape_or_apply_statement)(input)
    }

    /// ```
    /// ShapeOrApplyStatement = ShapeStatement / ApplyStatement
    /// ```
    pub fn shape_or_apply_statement(input: &str) -> IResult<&str, ShapeOrApply<'_>> {
        alt((
            map(shape_statement, ShapeOrApply::Shape),
            map(apply_statement, ShapeOrApply::Apply),
        ))(input)
    }

    /// ```
    /// ShapeStatement = TraitStatements Shape
    /// ```
    pub fn shape_statement(input: &str) -> IResult<&str, ShapeWithTraits<'_>> {
        map(tuple((trait_statements, shape)), |(traits, shape)| {
            ShapeWithTraits { traits, shape }
        })(input)
    }

    /// ```
    /// Shape =
    ///     SimpleShape
    ///   / EnumShape
    ///   / AggregateShape
    ///   / EntityShape
    ///   / OperationShape
    /// ```
    pub fn shape(input: &str) -> IResult<&str, Shape<'_>> {
        alt((
            map(simple_shape, Shape::Simple),
            map(enum_shape, Shape::Enum),
            map(aggregate_shape, Shape::Aggregate),
            map(entity_shape, Shape::Entity),
            map(operation_shape, Shape::Operation),
        ))(input)
    }

    /// ```
    /// SimpleShape = SimpleTypeName SP Identifier [Mixins]
    /// ```
    pub fn simple_shape(input: &str) -> IResult<&str, SimpleShape<'_>> {
        map(
            tuple((
                simple_type_name,
                cut(preceded(space1, identifier)),
                opt(mixins),
            )),
            |(type_name, identifier, mixins)| SimpleShape {
                type_name,
                identifier,
                mixins: mixins.unwrap_or_default(),
            },
        )(input)
    }

    /// ```
    /// SimpleTypeName =
    ///   %s"blob" / %s"boolean" / %s"document" / %s"string"
    /// / %s"byte" / %s"short" / %s"integer" / %s"long"
    /// / %s"float" / %s"double" / %s"bigInteger"
    /// / %s"bigDecimal" / %s"timestamp"
    /// ```
    pub fn simple_type_name(input: &str) -> IResult<&str, SimpleTypeName> {
        alt((
            map(tag("blob"), |_| SimpleTypeName::Blob),
            map(tag("boolean"), |_| SimpleTypeName::Boolean),
            map(tag("document"), |_| SimpleTypeName::Document),
            map(tag("string"), |_| SimpleTypeName::String),
            map(tag("byte"), |_| SimpleTypeName::Byte),
            map(tag("short"), |_| SimpleTypeName::Short),
            map(tag("integer"), |_| SimpleTypeName::Integer),
            map(tag("long"), |_| SimpleTypeName::Long),
            map(tag("float"), |_| SimpleTypeName::Float),
            map(tag("double"), |_| SimpleTypeName::Double),
            map(tag("bigInteger"), |_| SimpleTypeName::BigInteger),
            map(tag("bigDecimal"), |_| SimpleTypeName::BigDecimal),
            map(tag("timestamp"), |_| SimpleTypeName::Timestamp),
        ))(input)
    }

    /// ```
    /// Mixins = [SP] %s"with" [WS] "[" [WS] 1*(ShapeId [WS]) "]"
    /// ```
    pub fn mixins(input: &str) -> IResult<&str, Vec<&str>> {
        preceded(
            space0,
            preceded(
                tag("with"),
                cut(preceded(
                    opt(ws),
                    delimited(char('['), separated_list1(ws, shape_id), char(']')),
                )),
            ),
        )(input)
    }

    /// ```
    /// EnumShape = EnumTypeName SP Identifier [Mixins] [WS] EnumShapeMembers
    /// ```
    pub fn enum_shape(input: &str) -> IResult<&str, EnumShape<'_>> {
        map(
            tuple((
                enum_type_name,
                cut(preceded(space1, identifier)),
                opt(mixins),
                cut(preceded(opt(ws), enum_shape_members)),
            )),
            |(type_name, identifier, mixins, members)| EnumShape {
                type_name,
                identifier,
                mixins: mixins.unwrap_or_default(),
                members,
            },
        )(input)
    }

    /// ```
    /// EnumTypeName = %s"enum" / %s"intEnum"
    /// ```
    pub fn enum_type_name(input: &str) -> IResult<&str, EnumTypeName> {
        alt((
            map(tag("enum"), |_| EnumTypeName::Enum),
            map(tag("intEnum"), |_| EnumTypeName::IntEnum),
        ))(input)
    }

    /// ```
    /// EnumShapeMembers = "{" [WS] 1*(EnumShapeMember [WS]) "}"
    /// ```
    pub fn enum_shape_members(input: &str) -> IResult<&str, Vec<EnumShapeMember<'_>>> {
        delimited(
            char('{'),
            delimited(opt(ws), separated_list0(ws, enum_shape_member), opt(ws)),
            char('}'),
        )(input)
    }

    /// ```
    /// EnumShapeMember = TraitStatements Identifier [ValueAssignment]
    /// ```
    pub fn enum_shape_member(input: &str) -> IResult<&str, EnumShapeMember<'_>> {
        map(
            tuple((trait_statements, identifier, opt(value_assignment))),
            |(traits, identifier, value)| EnumShapeMember {
                traits,
                identifier,
                value,
            },
        )(input)
    }

    /// ```
    /// ValueAssignment = [SP] "=" [SP] NodeValue [SP] [Comma] BR
    /// ```
    pub fn value_assignment(input: &str) -> IResult<&str, NodeValue<'_>> {
        delimited(
            tuple((space0, char('='), space0)),
            node_value,
            opt(tuple((space0, comma))),
        )(input)
    }

    /// ```
    /// AggregateShape =
    ///     AggregateTypeName SP Identifier [ForResource] [Mixins]
    ///      [WS] ShapeMembers
    /// ```
    pub fn aggregate_shape(input: &str) -> IResult<&str, AggregateShape<'_>> {
        map(
            tuple((
                aggregate_shape_name,
                cut(preceded(space1, identifier)),
                opt(for_resource),
                opt(mixins),
                cut(preceded(opt(ws), shape_members)),
            )),
            |(type_name, identifier, for_resource, mixins, members)| AggregateShape {
                type_name,
                identifier,
                for_resource,
                mixins: mixins.unwrap_or_default(),
                members,
            },
        )(input)
    }

    /// ```
    /// AggregateTypeName = %s"list" / %s"map" / %s"union" / %s"structure"
    /// ```
    pub fn aggregate_shape_name(input: &str) -> IResult<&str, AggregateTypeName> {
        alt((
            map(tag("list"), |_| AggregateTypeName::List),
            map(tag("map"), |_| AggregateTypeName::Map),
            map(tag("union"), |_| AggregateTypeName::Union),
            map(tag("structure"), |_| AggregateTypeName::Structure),
        ))(input)
    }

    /// ```
    /// ForResource = SP %s"for" SP ShapeId
    /// ```
    pub fn for_resource(input: &str) -> IResult<&str, &str> {
        preceded(tuple((space1, tag("for"), space1)), shape_id)(input)
    }

    /// ```
    /// ShapeMembers = "{" [WS] *(ShapeMember [WS]) "}"
    /// ```
    pub fn shape_members(input: &str) -> IResult<&str, Vec<ShapeMember<'_>>> {
        delimited(
            tuple((char('{'), opt(ws))),
            separated_list1(ws, shape_member),
            tuple((opt(ws), char('}'))),
        )(input)
    }

    /// ```
    /// ShapeMember = TraitStatements (ExplicitShapeMember / ElidedShapeMember) [ValueAssignment]
    /// ```
    pub fn shape_member(input: &str) -> IResult<&str, ShapeMember<'_>> {
        map(
            tuple((
                trait_statements,
                alt((
                    map(explicit_shape_member, |(identifier, shape_name)| {
                        (identifier, Some(shape_name))
                    }),
                    map(elided_shape_member, |identifier| (identifier, None)),
                )),
                opt(value_assignment),
            )),
            |(traits, (identifier, shape_id), value)| ShapeMember {
                traits,
                identifier,
                shape_id,
                value,
            },
        )(input)
    }

    /// ```
    /// ExplicitShapeMember = Identifier [SP] ":" [SP] ShapeId
    /// ```
    pub fn explicit_shape_member(input: &str) -> IResult<&str, (&str, &str)> {
        separated_pair(
            identifier,
            tuple((space0, char(':'), space0)),
            cut(shape_id),
        )(input)
    }

    /// ```
    /// ElidedShapeMember = "$" Identifier
    /// ```
    pub fn elided_shape_member(input: &str) -> IResult<&str, &str> {
        preceded(char('$'), cut(identifier))(input)
    }

    /// ```
    /// EntityShape = EntityTypeName SP Identifier [Mixins] [WS] NodeObject
    /// ```
    pub fn entity_shape(input: &str) -> IResult<&str, EntityShape<'_>> {
        map(
            tuple((
                separated_pair(entity_type_name, cut(space1), cut(identifier)),
                opt(mixins),
                cut(preceded(opt(ws), node_object)),
            )),
            |((type_name, identifier), mixins, nodes)| EntityShape {
                type_name,
                identifier,
                mixins: mixins.unwrap_or_default(),
                nodes,
            },
        )(input)
    }

    /// ```
    /// EntityTypeName = %s"service" / %s"resource"
    /// ```
    pub fn entity_type_name(input: &str) -> IResult<&str, EntityTypeName> {
        alt((
            map(tag("service"), |_| EntityTypeName::Service),
            map(tag("resource"), |_| EntityTypeName::Resource),
        ))(input)
    }

    /// ```
    /// OperationShape = %s"operation" SP Identifier [Mixins] [WS] OperationBody
    /// ```
    pub fn operation_shape(input: &str) -> IResult<&str, OperationShape<'_>> {
        map(
            preceded(
                tag("operation"),
                cut(tuple((
                    preceded(space1, identifier),
                    opt(mixins),
                    preceded(opt(ws), operation_body),
                ))),
            ),
            |(identifier, mixins, body)| OperationShape {
                identifier,
                mixins: mixins.unwrap_or_default(),
                body,
            },
        )(input)
    }

    /// ```
    /// OperationBody = "{" [WS] *(OperationProperty [WS]) "}"
    /// ```
    pub fn operation_body(input: &str) -> IResult<&str, Vec<OperationProperty<'_>>> {
        delimited(
            tuple((char('{'), opt(ws))),
            separated_list0(ws, operation_property),
            tuple((opt(ws), char('}'))),
        )(input)
    }

    /// ```
    /// OperationProperty = OperationInput / OperationOutput / OperationErrors
    /// ```
    pub fn operation_property(input: &str) -> IResult<&str, OperationProperty<'_>> {
        alt((
            map(operation_input, OperationProperty::Input),
            map(operation_output, OperationProperty::Output),
            map(operation_errors, OperationProperty::Errors),
        ))(input)
    }

    /// ```
    /// OperationInput = %s"input" [WS] (InlineAggregateShape / (":" [WS] ShapeId))
    /// ```
    pub fn operation_input(input: &str) -> IResult<&str, OperationPropertyShape<'_>> {
        preceded(tuple((tag("input"), opt(ws))), inline_or_explicit_shape)(input)
    }

    /// ```
    /// OperationOutput = %s"output" [WS] (InlineAggregateShape / (":" [WS] ShapeId))
    /// ```
    pub fn operation_output(input: &str) -> IResult<&str, OperationPropertyShape<'_>> {
        preceded(tuple((tag("output"), opt(ws))), inline_or_explicit_shape)(input)
    }

    /// ```
    /// OperationErrors = %s"errors" [WS] ":" [WS] "[" [WS] *(ShapeId [WS]) "]"
    /// ```
    pub fn operation_errors(input: &str) -> IResult<&str, Vec<&str>> {
        preceded(
            tuple((tag("errors"), opt(ws))),
            cut(preceded(
                tuple((char(':'), opt(ws))),
                delimited(
                    tuple((char('['), opt(ws))),
                    separated_list0(ws, shape_id),
                    tuple((opt(ws), char(']'))),
                ),
            )),
        )(input)
    }

    pub fn inline_or_explicit_shape(input: &str) -> IResult<&str, OperationPropertyShape<'_>> {
        alt((
            map(inline_aggregate_shape, OperationPropertyShape::Inline),
            map(
                preceded(tag(":"), cut(preceded(opt(ws), shape_id))),
                OperationPropertyShape::Explicit,
            ),
        ))(input)
    }

    /// ```
    /// InlineAggregateShape = ":=" [WS] TraitStatements [ForResource] [Mixins] [WS] ShapeMembers
    /// ```
    pub fn inline_aggregate_shape(input: &str) -> IResult<&str, InlineAggregateShape<'_>> {
        map(
            preceded(
                tag(":="),
                cut(preceded(
                    opt(ws),
                    tuple((
                        trait_statements,
                        opt(for_resource),
                        opt(mixins),
                        preceded(opt(ws), shape_members),
                    )),
                )),
            ),
            |(traits, for_resource, mixins, members)| InlineAggregateShape {
                traits,
                for_resource,
                mixins: mixins.unwrap_or_default(),
                members,
            },
        )(input)
    }
}

pub mod traits {
    use nom::{
        IResult,
        branch::alt,
        bytes::complete::tag,
        character::complete::{char, space1},
        combinator::{cut, map, opt},
        multi::{separated_list0, separated_list1},
        sequence::{delimited, preceded, terminated, tuple},
    };

    use crate::{
        node_values::{NodeKeyValuePair, NodeValue, node_object_kvp, node_value},
        shape_id::shape_id,
        whitespace::ws,
    };

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Trait<'a> {
        pub shape_id: &'a str,
        pub body: Option<TraitBody<'a>>,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum TraitBody<'a> {
        Structure(Vec<NodeKeyValuePair<'a>>),
        Node(NodeValue<'a>),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct ApplyStatement<'a> {
        pub shape_id: &'a str,
        pub traits: Vec<Trait<'a>>,
    }

    /// ```
    /// TraitStatements = *(Trait [WS])
    /// ```
    pub fn trait_statements(input: &str) -> IResult<&str, Vec<Trait<'_>>> {
        terminated(separated_list0(ws, trait_), opt(ws))(input)
    }

    /// ```
    /// Trait = "@" ShapeId [TraitBody]
    /// ```
    pub fn trait_(input: &str) -> IResult<&str, Trait<'_>> {
        map(
            preceded(char('@'), cut(tuple((shape_id, opt(trait_body))))),
            |(shape_id, body)| Trait {
                shape_id,
                body: body.flatten(),
            },
        )(input)
    }

    /// ```
    /// TraitBody = "(" [WS] [TraitStructure / TraitNode] ")"
    /// ```
    pub fn trait_body(input: &str) -> IResult<&str, Option<TraitBody<'_>>> {
        preceded(
            char('('),
            cut(terminated(
                preceded(
                    opt(ws),
                    opt(alt((
                        map(trait_structure, TraitBody::Structure),
                        map(trait_node, TraitBody::Node),
                    ))),
                ),
                char(')'),
            )),
        )(input)
    }

    /// ```
    /// TraitStructure = 1*(NodeObjectKvp [WS])
    /// ```
    pub fn trait_structure(input: &str) -> IResult<&str, Vec<NodeKeyValuePair<'_>>> {
        separated_list1(ws, node_object_kvp)(input)
    }

    /// ```
    /// TraitNode = NodeValue [WS]
    /// ```
    pub fn trait_node(input: &str) -> IResult<&str, NodeValue<'_>> {
        terminated(node_value, opt(ws))(input)
    }

    /// ```
    /// ApplyStatement = ApplyStatementSingular / ApplyStatementBlock
    /// ApplyStatementSingular = %s"apply" SP ShapeId WS Trait
    /// ApplyStatementBlock = %s"apply" SP ShapeId WS "{" [WS] TraitStatements "}"
    /// ```
    pub fn apply_statement(input: &str) -> IResult<&str, ApplyStatement<'_>> {
        let apply_statement_singular = map(trait_, |v| vec![v]);
        let apply_statement_block =
            delimited(char('{'), preceded(opt(ws), trait_statements), char('}'));

        map(
            preceded(
                tag("apply"),
                cut(tuple((
                    delimited(space1, shape_id, ws),
                    alt((apply_statement_singular, apply_statement_block)),
                ))),
            ),
            |(shape_id, traits)| ApplyStatement { shape_id, traits },
        )(input)
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

    mod simple_shape {
        use crate::{
            node_values::{NodeKeyValuePair, NodeValue},
            shapes::{ShapeOrApply, SimpleShape, SimpleTypeName, shape_section},
            traits::{Trait, TraitBody},
        };

        #[test]
        fn smoke() {
            let (remaining, res) = shape_section("namespace com.foo // This is also a comment\n\n// Another comment\nstring MyString").unwrap();

            assert_eq!(remaining, "");
            assert_eq!(
                res,
                Some(crate::shapes::ShapeSection {
                    namespace: "com.foo",
                    uses: vec![],
                    shapes: vec![ShapeOrApply::Shape(crate::shapes::ShapeWithTraits {
                        traits: vec![],
                        shape: crate::shapes::Shape::Simple(SimpleShape {
                            type_name: SimpleTypeName::String,
                            identifier: "MyString",
                            mixins: vec![]
                        })
                    })]
                })
            );
        }

        #[test]
        fn with_mixin() {
            let (remaining, res) =
                shape_section("namespace com.foo\n\nstring MyString with [IdBearer]").unwrap();

            assert_eq!(remaining, "");
            assert_eq!(
                res,
                Some(crate::shapes::ShapeSection {
                    namespace: "com.foo",
                    uses: vec![],
                    shapes: vec![ShapeOrApply::Shape(crate::shapes::ShapeWithTraits {
                        traits: vec![],
                        shape: crate::shapes::Shape::Simple(SimpleShape {
                            type_name: SimpleTypeName::String,
                            identifier: "MyString",
                            mixins: vec!["IdBearer"]
                        })
                    })]
                })
            );
        }

        #[test]
        fn with_multiple_mixins() {
            let (remaining, res) =
                shape_section("namespace com.foo\n\nstring MyString with [IdBearer, Abc]").unwrap();

            assert_eq!(remaining, "");
            assert_eq!(
                res,
                Some(crate::shapes::ShapeSection {
                    namespace: "com.foo",
                    uses: vec![],
                    shapes: vec![ShapeOrApply::Shape(crate::shapes::ShapeWithTraits {
                        traits: vec![],
                        shape: crate::shapes::Shape::Simple(SimpleShape {
                            type_name: SimpleTypeName::String,
                            identifier: "MyString",
                            mixins: vec!["IdBearer", "Abc"]
                        })
                    })]
                })
            );
        }

        #[test]
        fn with_simple_trait() {
            let (remaining, res) =
                shape_section("namespace com.foo\n\n@myTrait\nstring MyString").unwrap();

            assert_eq!(remaining, "");
            assert_eq!(
                res,
                Some(crate::shapes::ShapeSection {
                    namespace: "com.foo",
                    uses: vec![],
                    shapes: vec![ShapeOrApply::Shape(crate::shapes::ShapeWithTraits {
                        traits: vec![Trait {
                            shape_id: "myTrait",
                            body: None,
                        }],
                        shape: crate::shapes::Shape::Simple(SimpleShape {
                            type_name: SimpleTypeName::String,
                            identifier: "MyString",
                            mixins: vec![]
                        })
                    })]
                })
            );
        }

        #[test]
        fn with_kv_trait() {
            let (remaining, res) = shape_section("namespace com.foo \n\n@myTrait(key: \"value\", otherKey: \"otherValue\")\nstring MyString").unwrap();

            assert_eq!(remaining, "");
            assert_eq!(
                res,
                Some(crate::shapes::ShapeSection {
                    namespace: "com.foo",
                    uses: vec![],
                    shapes: vec![ShapeOrApply::Shape(crate::shapes::ShapeWithTraits {
                        traits: vec![Trait {
                            shape_id: "myTrait",
                            body: Some(TraitBody::Structure(vec![
                                NodeKeyValuePair {
                                    key: "key",
                                    value: NodeValue::String("value"),
                                },
                                NodeKeyValuePair {
                                    key: "otherKey",
                                    value: NodeValue::String("otherValue"),
                                }
                            ])),
                        }],
                        shape: crate::shapes::Shape::Simple(SimpleShape {
                            type_name: SimpleTypeName::String,
                            identifier: "MyString",
                            mixins: vec![]
                        })
                    })]
                })
            );
        }

        #[test]
        fn with_node_trait() {
            let (remaining, res) =
                shape_section("namespace com.foo\n@myTrait(123)\nstring MyString").unwrap();

            assert_eq!(remaining, "");
            assert_eq!(
                res,
                Some(crate::shapes::ShapeSection {
                    namespace: "com.foo",
                    uses: vec![],
                    shapes: vec![ShapeOrApply::Shape(crate::shapes::ShapeWithTraits {
                        traits: vec![Trait {
                            shape_id: "myTrait",
                            body: Some(TraitBody::Node(NodeValue::Number("123"))),
                        }],
                        shape: crate::shapes::Shape::Simple(SimpleShape {
                            type_name: SimpleTypeName::String,
                            identifier: "MyString",
                            mixins: vec![]
                        })
                    })]
                })
            );
        }

        #[test]
        fn with_multiple_traits() {
            let (remaining, res) =
                shape_section("namespace com.foo\n@myTrait\n@myOtherTrait\nstring MyString")
                    .unwrap();

            assert_eq!(remaining, "");
            assert_eq!(
                res,
                Some(crate::shapes::ShapeSection {
                    namespace: "com.foo",
                    uses: vec![],
                    shapes: vec![ShapeOrApply::Shape(crate::shapes::ShapeWithTraits {
                        traits: vec![
                            Trait {
                                shape_id: "myTrait",
                                body: None,
                            },
                            Trait {
                                shape_id: "myOtherTrait",
                                body: None,
                            }
                        ],
                        shape: crate::shapes::Shape::Simple(SimpleShape {
                            type_name: SimpleTypeName::String,
                            identifier: "MyString",
                            mixins: vec![]
                        })
                    })]
                })
            );
        }
    }

    mod enum_shape {
        use crate::{
            node_values::NodeValue,
            shapes::{
                EnumShape, EnumShapeMember, EnumTypeName, Shape, ShapeOrApply, ShapeWithTraits,
                shape_section,
            },
            traits::Trait,
        };

        #[test]
        fn simple() {
            let (remaining, res) = shape_section(
                r#"namespace smithy.example

enum Suit {
    DIAMOND
    CLUB
    HEART
    SPADE
}"#,
            )
            .unwrap();

            assert_eq!(remaining, "");
            assert_eq!(
                res,
                Some(crate::shapes::ShapeSection {
                    namespace: "smithy.example",
                    uses: vec![],
                    shapes: vec![ShapeOrApply::Shape(ShapeWithTraits {
                        traits: vec![],
                        shape: Shape::Enum(EnumShape {
                            type_name: EnumTypeName::Enum,
                            identifier: "Suit",
                            mixins: vec![],
                            members: vec![
                                EnumShapeMember {
                                    traits: vec![],
                                    identifier: "DIAMOND",
                                    value: None,
                                },
                                EnumShapeMember {
                                    traits: vec![],
                                    identifier: "CLUB",
                                    value: None,
                                },
                                EnumShapeMember {
                                    traits: vec![],
                                    identifier: "HEART",
                                    value: None,
                                },
                                EnumShapeMember {
                                    traits: vec![],
                                    identifier: "SPADE",
                                    value: None,
                                },
                            ]
                        })
                    })]
                })
            );
        }

        #[test]
        fn with_value() {
            let (remaining, res) = shape_section(
                r#"namespace smithy.example

enum Suit {
    @deprecated
    DIAMOND = "diamond"

    CLUB = "club" HEART = "heart"
    @deprecated
    SPADE = "spade"
}"#,
            )
            .unwrap();

            assert_eq!(remaining, "");
            assert_eq!(
                res,
                Some(crate::shapes::ShapeSection {
                    namespace: "smithy.example",
                    uses: vec![],
                    shapes: vec![ShapeOrApply::Shape(ShapeWithTraits {
                        traits: vec![],
                        shape: Shape::Enum(EnumShape {
                            type_name: EnumTypeName::Enum,
                            identifier: "Suit",
                            mixins: vec![],
                            members: vec![
                                EnumShapeMember {
                                    traits: vec![Trait {
                                        shape_id: "deprecated",
                                        body: None,
                                    }],
                                    identifier: "DIAMOND",
                                    value: Some(NodeValue::String("diamond")),
                                },
                                EnumShapeMember {
                                    traits: vec![],
                                    identifier: "CLUB",
                                    value: Some(NodeValue::String("club")),
                                },
                                EnumShapeMember {
                                    traits: vec![],
                                    identifier: "HEART",
                                    value: Some(NodeValue::String("heart")),
                                },
                                EnumShapeMember {
                                    traits: vec![Trait {
                                        shape_id: "deprecated",
                                        body: None,
                                    }],
                                    identifier: "SPADE",
                                    value: Some(NodeValue::String("spade")),
                                },
                            ]
                        })
                    })]
                })
            );
        }
    }

    mod apply_statement {
        use crate::{
            node_values::{NodeKeyValuePair, NodeValue},
            shapes::{ShapeOrApply, shape_section},
            traits::{Trait, TraitBody},
        };

        #[test]
        fn single() {
            let (remaining, res) = shape_section(
                "namespace com.foo\napply MyString @documentation(\"This is my string!\")",
            )
            .unwrap();

            assert_eq!(remaining, "");
            assert_eq!(
                res,
                Some(crate::shapes::ShapeSection {
                    namespace: "com.foo",
                    uses: vec![],
                    shapes: vec![ShapeOrApply::Apply(crate::traits::ApplyStatement {
                        shape_id: "MyString",
                        traits: vec![Trait {
                            shape_id: "documentation",
                            body: Some(TraitBody::Node(NodeValue::String("This is my string!"))),
                        }],
                    })]
                })
            );
        }

        #[test]
        fn multiple() {
            let (remaining, res) = shape_section(
                "namespace com.foo\napply MyString {\n    @documentation(\"This is my string!\")\n    @length(min: 1, max: 10)\n}",
            )
            .unwrap();

            assert_eq!(remaining, "");
            assert_eq!(
                res,
                Some(crate::shapes::ShapeSection {
                    namespace: "com.foo",
                    uses: vec![],
                    shapes: vec![ShapeOrApply::Apply(crate::traits::ApplyStatement {
                        shape_id: "MyString",
                        traits: vec![
                            Trait {
                                shape_id: "documentation",
                                body: Some(TraitBody::Node(NodeValue::String(
                                    "This is my string!"
                                ))),
                            },
                            Trait {
                                shape_id: "length",
                                body: Some(TraitBody::Structure(vec![
                                    NodeKeyValuePair {
                                        key: "min",
                                        value: NodeValue::Number("1")
                                    },
                                    NodeKeyValuePair {
                                        key: "max",
                                        value: NodeValue::Number("10"),
                                    },
                                ])),
                            }
                        ],
                    })]
                })
            );
        }
    }

    mod aggregate {
        use crate::shapes::{
            AggregateShape, AggregateTypeName, Shape, ShapeMember, ShapeOrApply, ShapeSection,
            ShapeWithTraits, shape_section,
        };

        #[test]
        fn structure() {
            let (remaining, res) = shape_section(
                r#"namespace smithy.example

structure MyStructure for Test {
    a: MyString
    b: smithy.example#MyString
    $c
}"#,
            )
            .unwrap();

            assert_eq!(remaining, "");
            assert_eq!(
                res,
                Some(ShapeSection {
                    namespace: "smithy.example",
                    uses: vec![],
                    shapes: vec![ShapeOrApply::Shape(ShapeWithTraits {
                        traits: vec![],
                        shape: Shape::Aggregate(AggregateShape {
                            type_name: AggregateTypeName::Structure,
                            identifier: "MyStructure",
                            for_resource: Some("Test"),
                            mixins: vec![],
                            members: vec![
                                ShapeMember {
                                    traits: vec![],
                                    identifier: "a",
                                    shape_id: Some("MyString"),
                                    value: None,
                                },
                                ShapeMember {
                                    traits: vec![],
                                    identifier: "b",
                                    shape_id: Some("smithy.example#MyString"),
                                    value: None,
                                },
                                ShapeMember {
                                    traits: vec![],
                                    identifier: "c",
                                    shape_id: None,
                                    value: None,
                                },
                            ],
                        }),
                    })],
                })
            );
        }
    }

    mod entity {
        use crate::{
            node_values::{NodeKeyValuePair, NodeValue},
            shapes::{
                EntityShape, EntityTypeName, Shape, ShapeOrApply, ShapeSection, ShapeWithTraits,
                shape_section,
            },
        };

        #[test]
        fn service() {
            let (remaining, res) = shape_section(
                r#"namespace smithy.example

service ModelRepository {
    version: "2020-07-13",
    resources: [Model],
    operations: [PingService]
}"#,
            )
            .unwrap();

            assert_eq!(remaining, "");
            assert_eq!(
                res,
                Some(ShapeSection {
                    namespace: "smithy.example",
                    uses: vec![],
                    shapes: vec![ShapeOrApply::Shape(ShapeWithTraits {
                        traits: vec![],
                        shape: Shape::Entity(EntityShape {
                            type_name: EntityTypeName::Service,
                            identifier: "ModelRepository",
                            mixins: vec![],
                            nodes: vec![
                                NodeKeyValuePair {
                                    key: "version",
                                    value: NodeValue::String("2020-07-13"),
                                },
                                NodeKeyValuePair {
                                    key: "resources",
                                    value: NodeValue::Array(vec![NodeValue::String("Model")]),
                                },
                                NodeKeyValuePair {
                                    key: "operations",
                                    value: NodeValue::Array(vec![NodeValue::String("PingService")]),
                                },
                            ],
                        },),
                    },),],
                },)
            );
        }
    }

    mod operation {
        use crate::shapes::{
            InlineAggregateShape, OperationProperty, OperationPropertyShape, OperationShape, Shape,
            ShapeMember, ShapeOrApply, ShapeSection, ShapeWithTraits, shape_section,
        };

        #[test]
        fn smoke() {
            let (remaining, res) = shape_section(
                r#"namespace smithy.example

operation PingService {
    input: PingServiceInput,
    output := {
        username: String
        userId: String
    }
    errors: [UnavailableError, BadRequestError]
}"#,
            )
            .unwrap();

            assert_eq!(remaining, "");
            assert_eq!(
                res,
                Some(ShapeSection {
                    namespace: "smithy.example",
                    uses: vec![],
                    shapes: vec![ShapeOrApply::Shape(ShapeWithTraits {
                        traits: vec![],
                        shape: Shape::Operation(OperationShape {
                            identifier: "PingService",
                            mixins: vec![],
                            body: vec![
                                OperationProperty::Input(OperationPropertyShape::Explicit(
                                    "PingServiceInput",
                                )),
                                OperationProperty::Output(OperationPropertyShape::Inline(
                                    InlineAggregateShape {
                                        traits: vec![],
                                        for_resource: None,
                                        mixins: vec![],
                                        members: vec![
                                            ShapeMember {
                                                traits: vec![],
                                                identifier: "username",
                                                shape_id: Some("String"),
                                                value: None,
                                            },
                                            ShapeMember {
                                                traits: vec![],
                                                identifier: "userId",
                                                shape_id: Some("String"),
                                                value: None,
                                            }
                                        ]
                                    }
                                )),
                                OperationProperty::Errors(vec![
                                    "UnavailableError",
                                    "BadRequestError"
                                ]),
                            ],
                        }),
                    })],
                })
            );
        }
    }
}
