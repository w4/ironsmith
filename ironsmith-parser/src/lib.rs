#![deny(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]

use node_values::NodeValue;
use nom::{
    combinator::{all_consuming, opt},
    sequence::delimited,
    Finish, Parser,
};
use nom_language::error::{convert_error, VerboseError};
use shapes::ShapeSection;
use whitespace::ws;

type IResult<I, T> = nom::IResult<I, T, VerboseError<I>>;

/// Abstract syntax tree for a Smithy file
#[derive(Debug, Clone)]
pub struct Ast<'a> {
    /// Control section of the file
    pub control: Vec<(&'a str, NodeValue<'a>)>,
    /// Metadata section of the file
    pub metadata: Vec<(&'a str, NodeValue<'a>)>,
    /// Shapes defined within the file
    pub shapes: Option<ShapeSection<'a>>,
}

/// Parses an entire Smithy 2.0 IDL format file and returns the abstract syntax tree for it.
pub fn parse_ast(input: &str) -> Result<Ast<'_>, String> {
    // TODO: integrate with https://docs.rs/nom_locate and https://docs.rs/nom-supreme to provide better errors
    let (_rest, (control, metadata, shapes)) = all_consuming(delimited(
        opt(ws),
        (
            control::control_section,
            metadata::metadata_section,
            shapes::shape_section,
        ),
        opt(ws),
    ))
    .parse(input)
    .finish()
    .map_err(|e| convert_error(input, e))?;

    Ok(Ast {
        control,
        metadata,
        shapes,
    })
}

pub mod comment {
    use crate::IResult;
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{line_ending, not_line_ending},
        combinator::opt,
        sequence::delimited,
        Parser,
    };

    /// ```text
    /// Comment = DocumentationComment / LineComment
    /// ```
    pub fn comment(input: &str) -> IResult<&str, &str> {
        alt((documentation_comment, line_comment))
            .map(Option::unwrap_or_default)
            .parse(input)
    }

    /// ```text
    /// DocumentationComment = "///" *NotNL NL
    /// ```
    pub fn documentation_comment(input: &str) -> IResult<&str, Option<&str>> {
        delimited(tag("///"), opt(not_line_ending), line_ending).parse(input)
    }

    /// ```text
    /// LineComment = "//" [(%x09 / %x20-2E / %x30-10FFF) *NotNL] NL ; First character after "//" can't be "/"
    /// ```
    pub fn line_comment(input: &str) -> IResult<&str, Option<&str>> {
        delimited(tag("//"), opt(not_line_ending), line_ending).parse(input)
    }
}

pub mod whitespace {
    use crate::IResult;
    use nom::{
        branch::alt,
        character::complete::{char, line_ending, multispace1, space0},
        combinator::{opt, recognize},
        multi::many1,
        sequence::delimited,
        Parser,
    };

    /// ```text
    /// WS = 1*(SP / NL / Comment / Comma) ; whitespace
    /// ```
    pub fn ws(input: &str) -> IResult<&str, Vec<&str>> {
        many1(alt((multispace1, super::comment::comment, comma))).parse(input)
    }

    /// ```text
    /// Comma = ","
    /// ```
    pub fn comma(input: &str) -> IResult<&str, &str> {
        recognize(char(',')).parse(input)
    }

    /// ```text
    /// BR = [SP] 1*(Comment / NL) [WS]; line break followed by whitespace
    /// ```
    pub fn br(input: &str) -> IResult<&str, Vec<&str>> {
        delimited(
            space0,
            many1(alt((super::comment::comment, line_ending))),
            opt(ws),
        )
        .parse(input)
    }
}

pub mod control {
    use nom::{
        character::complete::{char, space0},
        combinator::cut,
        multi::many0,
        sequence::{delimited, preceded, separated_pair, terminated},
        Parser,
    };

    use crate::{
        node_values::{node_object_key, node_value, NodeValue},
        whitespace::br,
        IResult,
    };

    /// ```text
    /// ControlSection = *(ControlStatement)
    /// ```
    pub fn control_section(input: &str) -> IResult<&str, Vec<(&str, NodeValue<'_>)>> {
        many0(control_statement).parse(input)
    }

    /// ```text
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
        )
        .parse(input)
    }
}

pub mod metadata {
    use nom::{
        bytes::complete::tag,
        character::complete::{char, space0, space1},
        combinator::cut,
        multi::many0,
        sequence::{delimited, preceded, separated_pair, terminated},
        Parser,
    };

    use crate::{
        node_values::{node_object_key, node_value, NodeValue},
        whitespace::br,
        IResult,
    };

    /// ```text
    /// MetadataSection = *(MetadataStatement)
    /// ```
    pub fn metadata_section(input: &str) -> IResult<&str, Vec<(&str, NodeValue<'_>)>> {
        many0(metadata_statement).parse(input)
    }

    /// ```text
    /// MetadataStatement = %s"metadata" SP NodeObjectKey [SP] "=" [SP] NodeValue BR
    /// ```
    pub fn metadata_statement(input: &str) -> IResult<&str, (&str, NodeValue<'_>)> {
        preceded(
            (tag("metadata"), space1),
            cut(terminated(
                separated_pair(
                    node_object_key,
                    delimited(space0, char('='), space0),
                    node_value,
                ),
                br,
            )),
        )
        .parse(input)
    }
}

pub mod node_values {
    use nom::{
        branch::alt,
        bytes::complete::{tag, take_while_m_n},
        character::complete::{char, line_ending, one_of, space0},
        combinator::{cut, opt, recognize},
        multi::{many0, many1, separated_list0},
        number::complete::recognize_float,
        sequence::{delimited, preceded, separated_pair, terminated},
        Parser,
    };

    use crate::{
        shape_id::{identifier, shape_id, ShapeId},
        whitespace::ws,
        IResult,
    };

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum NodeValue<'a> {
        Array(Vec<NodeValue<'a>>),
        Object(Vec<NodeKeyValuePair<'a>>),
        Number(&'a str),
        Keyword(Keyword),
        String(StringNode<'a>),
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum StringNode<'a> {
        String(&'a str),
        ShapeId(ShapeId<'a>),
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

    /// ```text
    /// NodeValue =
    ///   NodeArray
    /// / NodeObject
    /// / Number
    /// / NodeKeyword
    /// / NodeStringValue
    /// ```
    pub fn node_value(input: &str) -> IResult<&str, NodeValue<'_>> {
        alt((
            node_array.map(NodeValue::Array),
            node_object.map(NodeValue::Object),
            recognize_float.map(NodeValue::Number),
            node_keyword.map(NodeValue::Keyword),
            node_string_value.map(NodeValue::String),
        ))
        .parse(input)
    }

    /// ```text
    /// NodeArray = "[" [WS] *(NodeValue [WS]) "]"
    /// ```
    pub fn node_array(input: &str) -> IResult<&str, Vec<NodeValue<'_>>> {
        preceded(
            char('['),
            cut(terminated(
                delimited(opt(ws), separated_list0(ws, node_value), opt(ws)),
                char(']'),
            )),
        )
        .parse(input)
    }

    /// ```text
    /// NodeObject = "{" [WS] [NodeObjectKvp *(WS NodeObjectKvp)] [WS] "}"
    /// ```
    pub fn node_object(input: &str) -> IResult<&str, Vec<NodeKeyValuePair<'_>>> {
        preceded(
            char('{'),
            cut(terminated(
                delimited(opt(ws), separated_list0(ws, node_object_kvp), opt(ws)),
                char('}'),
            )),
        )
        .parse(input)
    }

    /// ```text
    /// NodeObjectKvp = NodeObjectKey [WS] ":" [WS] NodeValue
    /// ```
    pub fn node_object_kvp(input: &str) -> IResult<&str, NodeKeyValuePair<'_>> {
        separated_pair(node_object_key, (opt(ws), char(':'), opt(ws)), node_value)
            .map(|(key, value)| NodeKeyValuePair { key, value })
            .parse(input)
    }

    /// ```text
    /// NodeKeyword = %s"true" / %s"false" / %s"null"
    /// ```
    pub fn node_keyword(input: &str) -> IResult<&str, Keyword> {
        alt((
            tag("true").map(|_| Keyword::True),
            tag("false").map(|_| Keyword::False),
            tag("null").map(|_| Keyword::Null),
        ))
        .parse(input)
    }

    /// ```text
    /// NodeStringValue = ShapeId / TextBlock / QuotedText
    /// ```
    pub fn node_string_value(input: &str) -> IResult<&str, StringNode<'_>> {
        alt((
            shape_id.map(StringNode::ShapeId),
            text_block.map(StringNode::String),
            quoted_text.map(StringNode::String),
        ))
        .parse(input)
    }

    /// ```text
    /// TextBlock = ThreeDquotes [SP] NL *TextBlockContent ThreeDquotes
    /// ```
    pub fn text_block(input: &str) -> IResult<&str, &str> {
        delimited(
            tag(r#"""""#),
            preceded((space0, line_ending), recognize(many0(text_block_content))),
            tag(r#"""""#),
        )
        .parse(input)
    }

    // ```
    // TextBlockContent = QuotedChar / (1*2DQUOTE 1*QuotedChar)
    // ```
    pub fn text_block_content(input: &str) -> IResult<&str, &str> {
        alt((
            quoted_char,
            preceded(many1(tag(r#""""#)), recognize(many1(quoted_char))),
        ))
        .parse(input)
    }

    /// ```text
    /// NodeObjectKey = QuotedText / Identifier
    /// ```
    pub fn node_object_key(input: &str) -> IResult<&str, &str> {
        alt((quoted_text, identifier)).parse(input)
    }

    /// ```text
    /// QuotedText = DQUOTE *QuotedChar DQUOTE
    /// ```
    pub fn quoted_text(input: &str) -> IResult<&str, &str> {
        preceded(
            char('"'),
            cut(terminated(recognize(many0(quoted_char)), char('"'))),
        )
        .parse(input)
    }

    /// ```text
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
        ))
        .parse(input)
    }

    /// ```text
    /// EscapedChar =
    ///     Escape (Escape / DQUOTE / %s"b" / %s"f"
    ///             / %s"n" / %s"r" / %s"t" / "/"
    ///             / UnicodeEscape)
    /// ```
    pub fn escaped_char(input: &str) -> IResult<&str, &str> {
        preceded(
            char('\\'),
            alt((recognize(one_of(r#"bfnrt\/""#)), unicode_escape)),
        )
        .parse(input)
    }

    /// ```text
    /// UnicodeEscape =     %s"u" Hex Hex Hex Hex
    /// ```
    pub fn unicode_escape(input: &str) -> IResult<&str, &str> {
        preceded(
            char('u'),
            take_while_m_n(4, 4, |c: char| c.is_ascii_hexdigit()),
        )
        .parse(input)
    }
}

pub mod shape_id {
    use crate::IResult;
    use nom::{
        branch::alt,
        bytes::complete::{take_while, take_while1},
        character::complete::{alpha1, alphanumeric1, char},
        combinator::{map, opt, recognize},
        multi::separated_list1,
        sequence::{preceded, separated_pair},
        Parser,
    };

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct AbsoluteRootShapeId<'a> {
        pub namespace: &'a str,
        pub identifier: &'a str,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum RootShapeId<'a> {
        Relative(&'a str),
        Absolute(AbsoluteRootShapeId<'a>),
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct ShapeId<'a> {
        pub root_shape_id: RootShapeId<'a>,
        pub shape_id_member: Option<&'a str>,
    }

    /// ```text
    /// ShapeId = RootShapeId [ShapeIdMember]
    /// ```
    pub fn shape_id(input: &str) -> IResult<&str, ShapeId<'_>> {
        map(
            (root_shape_id, opt(shape_id_member)),
            |(root_shape_id, shape_id_member)| ShapeId {
                root_shape_id,
                shape_id_member,
            },
        )
        .parse(input)
    }

    /// ```text
    /// RootShapeId = AbsoluteRootShapeId / Identifier
    /// ```
    pub fn root_shape_id(input: &str) -> IResult<&str, RootShapeId<'_>> {
        alt((
            absolute_root_shape_id.map(RootShapeId::Absolute),
            identifier.map(RootShapeId::Relative),
        ))
        .parse(input)
    }

    /// ```text
    /// AbsoluteRootShapeId = Namespace "#" Identifier
    /// ```
    pub fn absolute_root_shape_id(input: &str) -> IResult<&str, AbsoluteRootShapeId<'_>> {
        separated_pair(namespace, char('#'), identifier)
            .map(|(namespace, identifier)| AbsoluteRootShapeId {
                namespace,
                identifier,
            })
            .parse(input)
    }

    /// ```text
    /// Namespace = Identifier *("." Identifier)
    /// ```
    pub fn namespace(input: &str) -> IResult<&str, &str> {
        recognize(separated_list1(char('.'), identifier)).parse(input)
    }

    /// ```text
    /// IdentifierStart *IdentifierChars
    /// ```
    pub fn identifier(input: &str) -> IResult<&str, &str> {
        recognize((identifier_start, take_while(is_identifier_char))).parse(input)
    }

    /// ```text
    /// IdentifierStart = (1*"_" (ALPHA / DIGIT)) / ALPHA
    /// ```
    pub fn identifier_start(input: &str) -> IResult<&str, &str> {
        alt((preceded(take_while1(|v| v == '_'), alphanumeric1), alpha1)).parse(input)
    }

    /// ```text
    /// IdentifierChars = ALPHA / DIGIT / "_"
    /// ```
    #[must_use]
    pub fn is_identifier_char(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    /// ```text
    /// ShapeIdMember = "$" Identifier
    /// ```
    pub fn shape_id_member(input: &str) -> IResult<&str, &str> {
        preceded(char('$'), identifier).parse(input)
    }
}

pub mod shapes {
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{char, space0, space1},
        combinator::{cut, opt},
        multi::{many0, separated_list0, separated_list1},
        sequence::{delimited, preceded, separated_pair},
        Parser,
    };

    use crate::{
        node_values::{node_object, node_value, NodeKeyValuePair, NodeValue},
        shape_id::{
            absolute_root_shape_id, identifier, namespace, shape_id, AbsoluteRootShapeId, ShapeId,
        },
        traits::{apply_statement, trait_statements, ApplyStatement, Trait},
        whitespace::{br, comma, ws},
        IResult,
    };

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct ShapeSection<'a> {
        pub namespace: &'a str,
        pub uses: Vec<AbsoluteRootShapeId<'a>>,
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

    impl<'a> Shape<'a> {
        #[must_use]
        pub fn mixins(&self) -> &[ShapeId<'a>] {
            match self {
                Shape::Simple(shape) => &shape.mixins,
                Shape::Enum(shape) => &shape.mixins,
                Shape::Aggregate(shape) => &shape.mixins,
                Shape::Entity(shape) => &shape.mixins,
                Shape::Operation(shape) => &shape.mixins,
            }
        }

        #[must_use]
        pub fn resource_bound(&self) -> Option<ShapeId<'a>> {
            match self {
                Shape::Aggregate(shape) => shape.for_resource,
                _ => None,
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct SimpleShape<'a> {
        pub type_name: SimpleTypeName,
        pub identifier: &'a str,
        pub mixins: Vec<ShapeId<'a>>,
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
        pub mixins: Vec<ShapeId<'a>>,
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
        pub for_resource: Option<ShapeId<'a>>,
        pub mixins: Vec<ShapeId<'a>>,
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
        pub shape_id: Option<ShapeId<'a>>,
        pub value: Option<NodeValue<'a>>,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct EntityShape<'a> {
        pub type_name: EntityTypeName,
        pub identifier: &'a str,
        pub mixins: Vec<ShapeId<'a>>,
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
        pub mixins: Vec<ShapeId<'a>>,
        pub body: Vec<OperationProperty<'a>>,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum OperationProperty<'a> {
        Input(OperationPropertyShape<'a>),
        Output(OperationPropertyShape<'a>),
        Errors(Vec<ShapeId<'a>>),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum OperationPropertyShape<'a> {
        Explicit(ShapeId<'a>),
        Inline(InlineAggregateShape<'a>),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct InlineAggregateShape<'a> {
        pub traits: Vec<Trait<'a>>,
        pub for_resource: Option<ShapeId<'a>>,
        pub mixins: Vec<ShapeId<'a>>,
        pub members: Vec<ShapeMember<'a>>,
    }

    /// ```text
    /// ShapeSection = [NamespaceStatement UseSection [ShapeStatements]]
    /// ```
    pub fn shape_section(input: &str) -> IResult<&str, Option<ShapeSection<'_>>> {
        opt((namespace_statement, use_section, shape_statements).map(
            |(namespace, uses, shapes)| ShapeSection {
                namespace,
                uses,
                shapes,
            },
        ))
        .parse(input)
    }

    /// ```text
    /// NamespaceStatement = %s"namespace" SP Namespace BR
    /// ```
    pub fn namespace_statement(input: &str) -> IResult<&str, &str> {
        preceded(tag("namespace"), cut(delimited(space1, namespace, br))).parse(input)
    }

    /// ```text
    /// UseSection = *(UseStatement)
    /// ```
    pub fn use_section(input: &str) -> IResult<&str, Vec<AbsoluteRootShapeId<'_>>> {
        many0(use_statement).parse(input)
    }

    /// ```text
    /// UseStatement = %s"use" SP AbsoluteRootShapeId BR
    /// ```
    pub fn use_statement(input: &str) -> IResult<&str, AbsoluteRootShapeId<'_>> {
        preceded(
            tag("use"),
            cut(delimited(space1, absolute_root_shape_id, br)),
        )
        .parse(input)
    }

    /// ```text
    /// ShapeOrApplyStatement *(BR ShapeOrApplyStatement)
    /// ```
    pub fn shape_statements(input: &str) -> IResult<&str, Vec<ShapeOrApply<'_>>> {
        separated_list0(br, shape_or_apply_statement).parse(input)
    }

    /// ```text
    /// ShapeOrApplyStatement = ShapeStatement / ApplyStatement
    /// ```
    pub fn shape_or_apply_statement(input: &str) -> IResult<&str, ShapeOrApply<'_>> {
        alt((
            shape_statement.map(ShapeOrApply::Shape),
            apply_statement.map(ShapeOrApply::Apply),
        ))
        .parse(input)
    }

    /// ```text
    /// ShapeStatement = TraitStatements Shape
    /// ```
    pub fn shape_statement(input: &str) -> IResult<&str, ShapeWithTraits<'_>> {
        (trait_statements, shape)
            .map(|(traits, shape)| ShapeWithTraits { traits, shape })
            .parse(input)
    }

    /// ```text
    /// Shape =
    ///     SimpleShape
    ///   / EnumShape
    ///   / AggregateShape
    ///   / EntityShape
    ///   / OperationShape
    /// ```
    pub fn shape(input: &str) -> IResult<&str, Shape<'_>> {
        alt((
            simple_shape.map(Shape::Simple),
            enum_shape.map(Shape::Enum),
            aggregate_shape.map(Shape::Aggregate),
            entity_shape.map(Shape::Entity),
            operation_shape.map(Shape::Operation),
        ))
        .parse(input)
    }

    /// ```text
    /// SimpleShape = SimpleTypeName SP Identifier [Mixins]
    /// ```
    pub fn simple_shape(input: &str) -> IResult<&str, SimpleShape<'_>> {
        (
            simple_type_name,
            cut(preceded(space1, identifier)),
            opt(mixins),
        )
            .map(|(type_name, identifier, mixins)| SimpleShape {
                type_name,
                identifier,
                mixins: mixins.unwrap_or_default(),
            })
            .parse(input)
    }

    /// ```text
    /// SimpleTypeName =
    ///   %s"blob" / %s"boolean" / %s"document" / %s"string"
    /// / %s"byte" / %s"short" / %s"integer" / %s"long"
    /// / %s"float" / %s"double" / %s"bigInteger"
    /// / %s"bigDecimal" / %s"timestamp"
    /// ```
    pub fn simple_type_name(input: &str) -> IResult<&str, SimpleTypeName> {
        alt((
            tag("blob").map(|_| SimpleTypeName::Blob),
            tag("boolean").map(|_| SimpleTypeName::Boolean),
            tag("document").map(|_| SimpleTypeName::Document),
            tag("string").map(|_| SimpleTypeName::String),
            tag("byte").map(|_| SimpleTypeName::Byte),
            tag("short").map(|_| SimpleTypeName::Short),
            tag("integer").map(|_| SimpleTypeName::Integer),
            tag("long").map(|_| SimpleTypeName::Long),
            tag("float").map(|_| SimpleTypeName::Float),
            tag("double").map(|_| SimpleTypeName::Double),
            tag("bigInteger").map(|_| SimpleTypeName::BigInteger),
            tag("bigDecimal").map(|_| SimpleTypeName::BigDecimal),
            tag("timestamp").map(|_| SimpleTypeName::Timestamp),
        ))
        .parse(input)
    }

    /// ```text
    /// Mixins = [SP] %s"with" [WS] "[" [WS] 1*(ShapeId [WS]) "]"
    /// ```
    pub fn mixins(input: &str) -> IResult<&str, Vec<ShapeId<'_>>> {
        preceded(
            space0,
            preceded(
                tag("with"),
                cut(preceded(
                    opt(ws),
                    delimited(char('['), separated_list1(ws, shape_id), char(']')),
                )),
            ),
        )
        .parse(input)
    }

    /// ```text
    /// EnumShape = EnumTypeName SP Identifier [Mixins] [WS] EnumShapeMembers
    /// ```
    pub fn enum_shape(input: &str) -> IResult<&str, EnumShape<'_>> {
        (
            enum_type_name,
            cut(preceded(space1, identifier)),
            opt(mixins),
            cut(preceded(opt(ws), enum_shape_members)),
        )
            .map(|(type_name, identifier, mixins, members)| EnumShape {
                type_name,
                identifier,
                mixins: mixins.unwrap_or_default(),
                members,
            })
            .parse(input)
    }

    /// ```text
    /// EnumTypeName = %s"enum" / %s"intEnum"
    /// ```
    pub fn enum_type_name(input: &str) -> IResult<&str, EnumTypeName> {
        alt((
            tag("enum").map(|_| EnumTypeName::Enum),
            tag("intEnum").map(|_| EnumTypeName::IntEnum),
        ))
        .parse(input)
    }

    /// ```text
    /// EnumShapeMembers = "{" [WS] 1*(EnumShapeMember [WS]) "}"
    /// ```
    pub fn enum_shape_members(input: &str) -> IResult<&str, Vec<EnumShapeMember<'_>>> {
        delimited(
            char('{'),
            delimited(opt(ws), separated_list0(ws, enum_shape_member), opt(ws)),
            char('}'),
        )
        .parse(input)
    }

    /// ```text
    /// EnumShapeMember = TraitStatements Identifier [ValueAssignment]
    /// ```
    pub fn enum_shape_member(input: &str) -> IResult<&str, EnumShapeMember<'_>> {
        (trait_statements, identifier, opt(value_assignment))
            .map(|(traits, identifier, value)| EnumShapeMember {
                traits,
                identifier,
                value,
            })
            .parse(input)
    }

    /// ```text
    /// ValueAssignment = [SP] "=" [SP] NodeValue [SP] [Comma] BR
    /// ```
    pub fn value_assignment(input: &str) -> IResult<&str, NodeValue<'_>> {
        delimited(
            (space0, char('='), space0),
            node_value,
            opt((space0, comma)),
        )
        .parse(input)
    }

    /// ```text
    /// AggregateShape =
    ///     AggregateTypeName SP Identifier [ForResource] [Mixins]
    ///      [WS] ShapeMembers
    /// ```
    pub fn aggregate_shape(input: &str) -> IResult<&str, AggregateShape<'_>> {
        (
            aggregate_shape_name,
            cut(preceded(space1, identifier)),
            opt(for_resource),
            opt(mixins),
            cut(preceded(opt(ws), shape_members)),
        )
            .map(
                |(type_name, identifier, for_resource, mixins, members)| AggregateShape {
                    type_name,
                    identifier,
                    for_resource,
                    mixins: mixins.unwrap_or_default(),
                    members,
                },
            )
            .parse(input)
    }

    /// ```text
    /// AggregateTypeName = %s"list" / %s"map" / %s"union" / %s"structure"
    /// ```
    pub fn aggregate_shape_name(input: &str) -> IResult<&str, AggregateTypeName> {
        alt((
            tag("list").map(|_| AggregateTypeName::List),
            tag("map").map(|_| AggregateTypeName::Map),
            tag("union").map(|_| AggregateTypeName::Union),
            tag("structure").map(|_| AggregateTypeName::Structure),
        ))
        .parse(input)
    }

    /// ```text
    /// ForResource = SP %s"for" SP ShapeId
    /// ```
    pub fn for_resource(input: &str) -> IResult<&str, ShapeId<'_>> {
        preceded((space1, tag("for"), space1), shape_id).parse(input)
    }

    /// ```text
    /// ShapeMembers = "{" [WS] *(ShapeMember [WS]) "}"
    /// ```
    pub fn shape_members(input: &str) -> IResult<&str, Vec<ShapeMember<'_>>> {
        delimited(
            (char('{'), opt(ws)),
            separated_list0(ws, shape_member),
            (opt(ws), char('}')),
        )
        .parse(input)
    }

    /// ```text
    /// ShapeMember = TraitStatements (ExplicitShapeMember / ElidedShapeMember) [ValueAssignment]
    /// ```
    pub fn shape_member(input: &str) -> IResult<&str, ShapeMember<'_>> {
        (
            trait_statements,
            alt((
                explicit_shape_member
                    .map(|(identifier, shape_name)| (identifier, Some(shape_name))),
                elided_shape_member.map(|identifier| (identifier, None)),
            )),
            opt(value_assignment),
        )
            .map(|(traits, (identifier, shape_id), value)| ShapeMember {
                traits,
                identifier,
                shape_id,
                value,
            })
            .parse(input)
    }

    /// ```text
    /// ExplicitShapeMember = Identifier [SP] ":" [SP] ShapeId
    /// ```
    pub fn explicit_shape_member(input: &str) -> IResult<&str, (&str, ShapeId<'_>)> {
        separated_pair(identifier, (space0, char(':'), space0), cut(shape_id)).parse(input)
    }

    /// ```text
    /// ElidedShapeMember = "$" Identifier
    /// ```
    pub fn elided_shape_member(input: &str) -> IResult<&str, &str> {
        preceded(char('$'), cut(identifier)).parse(input)
    }

    /// ```text
    /// EntityShape = EntityTypeName SP Identifier [Mixins] [WS] NodeObject
    /// ```
    pub fn entity_shape(input: &str) -> IResult<&str, EntityShape<'_>> {
        (
            separated_pair(entity_type_name, cut(space1), cut(identifier)),
            opt(mixins),
            cut(preceded(opt(ws), node_object)),
        )
            .map(|((type_name, identifier), mixins, nodes)| EntityShape {
                type_name,
                identifier,
                mixins: mixins.unwrap_or_default(),
                nodes,
            })
            .parse(input)
    }

    /// ```text
    /// EntityTypeName = %s"service" / %s"resource"
    /// ```
    pub fn entity_type_name(input: &str) -> IResult<&str, EntityTypeName> {
        alt((
            tag("service").map(|_| EntityTypeName::Service),
            tag("resource").map(|_| EntityTypeName::Resource),
        ))
        .parse(input)
    }

    /// ```text
    /// OperationShape = %s"operation" SP Identifier [Mixins] [WS] OperationBody
    /// ```
    pub fn operation_shape(input: &str) -> IResult<&str, OperationShape<'_>> {
        preceded(
            tag("operation"),
            cut((
                preceded(space1, identifier),
                opt(mixins),
                preceded(opt(ws), operation_body),
            )),
        )
        .map(|(identifier, mixins, body)| OperationShape {
            identifier,
            mixins: mixins.unwrap_or_default(),
            body,
        })
        .parse(input)
    }

    /// ```text
    /// OperationBody = "{" [WS] *(OperationProperty [WS]) "}"
    /// ```
    pub fn operation_body(input: &str) -> IResult<&str, Vec<OperationProperty<'_>>> {
        delimited(
            (char('{'), opt(ws)),
            separated_list0(ws, operation_property),
            (opt(ws), char('}')),
        )
        .parse(input)
    }

    /// ```text
    /// OperationProperty = OperationInput / OperationOutput / OperationErrors
    /// ```
    pub fn operation_property(input: &str) -> IResult<&str, OperationProperty<'_>> {
        alt((
            operation_input.map(OperationProperty::Input),
            operation_output.map(OperationProperty::Output),
            operation_errors.map(OperationProperty::Errors),
        ))
        .parse(input)
    }

    /// ```text
    /// OperationInput = %s"input" [WS] (InlineAggregateShape / (":" [WS] ShapeId))
    /// ```
    pub fn operation_input(input: &str) -> IResult<&str, OperationPropertyShape<'_>> {
        preceded((tag("input"), opt(ws)), inline_or_explicit_shape).parse(input)
    }

    /// ```text
    /// OperationOutput = %s"output" [WS] (InlineAggregateShape / (":" [WS] ShapeId))
    /// ```
    pub fn operation_output(input: &str) -> IResult<&str, OperationPropertyShape<'_>> {
        preceded((tag("output"), opt(ws)), inline_or_explicit_shape).parse(input)
    }

    /// ```text
    /// OperationErrors = %s"errors" [WS] ":" [WS] "[" [WS] *(ShapeId [WS]) "]"
    /// ```
    pub fn operation_errors(input: &str) -> IResult<&str, Vec<ShapeId<'_>>> {
        preceded(
            (tag("errors"), opt(ws)),
            cut(preceded(
                (char(':'), opt(ws)),
                delimited(
                    (char('['), opt(ws)),
                    separated_list0(ws, shape_id),
                    (opt(ws), char(']')),
                ),
            )),
        )
        .parse(input)
    }

    pub fn inline_or_explicit_shape(input: &str) -> IResult<&str, OperationPropertyShape<'_>> {
        alt((
            inline_aggregate_shape.map(OperationPropertyShape::Inline),
            preceded(tag(":"), cut(preceded(opt(ws), shape_id)))
                .map(OperationPropertyShape::Explicit),
        ))
        .parse(input)
    }

    /// ```text
    /// InlineAggregateShape = ":=" [WS] TraitStatements [ForResource] [Mixins] [WS] ShapeMembers
    /// ```
    pub fn inline_aggregate_shape(input: &str) -> IResult<&str, InlineAggregateShape<'_>> {
        preceded(
            tag(":="),
            cut(preceded(
                opt(ws),
                (
                    trait_statements,
                    opt(for_resource),
                    opt(mixins),
                    preceded(opt(ws), shape_members),
                ),
            )),
        )
        .map(
            |(traits, for_resource, mixins, members)| InlineAggregateShape {
                traits,
                for_resource,
                mixins: mixins.unwrap_or_default(),
                members,
            },
        )
        .parse(input)
    }
}

pub mod traits {
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{char, space1},
        combinator::{cut, map, opt, recognize},
        multi::{separated_list0, separated_list1},
        sequence::{delimited, preceded, terminated},
        Parser,
    };

    use crate::{
        node_values::{node_object_kvp, node_value, NodeKeyValuePair, NodeValue},
        shape_id::{shape_id, ShapeId},
        whitespace::ws,
        IResult,
    };

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Trait<'a> {
        pub shape_id: ShapeId<'a>,
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

    /// ```text
    /// TraitStatements = *(Trait [WS])
    /// ```
    pub fn trait_statements(input: &str) -> IResult<&str, Vec<Trait<'_>>> {
        terminated(separated_list0(ws, trait_), opt(ws)).parse(input)
    }

    /// ```text
    /// Trait = "@" ShapeId [TraitBody]
    /// ```
    pub fn trait_(input: &str) -> IResult<&str, Trait<'_>> {
        preceded(char('@'), cut((shape_id, opt(trait_body))))
            .map(|(shape_id, body)| Trait {
                shape_id,
                body: body.flatten(),
            })
            .parse(input)
    }

    /// ```text
    /// TraitBody = "(" [WS] [TraitStructure / TraitNode] ")"
    /// ```
    pub fn trait_body(input: &str) -> IResult<&str, Option<TraitBody<'_>>> {
        preceded(
            char('('),
            cut(terminated(
                preceded(
                    opt(ws),
                    opt(alt((
                        trait_structure.map(TraitBody::Structure),
                        trait_node.map(TraitBody::Node),
                    ))),
                ),
                char(')'),
            )),
        )
        .parse(input)
    }

    /// ```text
    /// TraitStructure = 1*(NodeObjectKvp [WS])
    /// ```
    pub fn trait_structure(input: &str) -> IResult<&str, Vec<NodeKeyValuePair<'_>>> {
        separated_list1(ws, node_object_kvp).parse(input)
    }

    /// ```text
    /// TraitNode = NodeValue [WS]
    /// ```
    pub fn trait_node(input: &str) -> IResult<&str, NodeValue<'_>> {
        terminated(node_value, opt(ws)).parse(input)
    }

    /// ```text
    /// ApplyStatement = ApplyStatementSingular / ApplyStatementBlock
    /// ApplyStatementSingular = %s"apply" SP ShapeId WS Trait
    /// ApplyStatementBlock = %s"apply" SP ShapeId WS "{" [WS] TraitStatements "}"
    /// ```
    pub fn apply_statement(input: &str) -> IResult<&str, ApplyStatement<'_>> {
        let apply_statement_singular = map(trait_, |v| vec![v]);
        let apply_statement_block = delimited(
            char('{'),
            delimited(opt(ws), trait_statements, opt(ws)),
            char('}'),
        );

        preceded(
            tag("apply"),
            cut((
                delimited(space1, recognize(shape_id), ws),
                alt((apply_statement_singular, apply_statement_block)),
            )),
        )
        .map(|(shape_id, traits)| ApplyStatement { shape_id, traits })
        .parse(input)
    }
}

#[cfg(test)]
#[allow(clippy::needless_raw_string_hashes)]
mod test {
    use insta::{assert_debug_snapshot, glob};

    #[test]
    fn integration() {
        glob!("../fixtures", "*.smithy", |path| {
            let input = std::fs::read_to_string(path).unwrap();

            let ast = match crate::parse_ast(&input) {
                Ok(v) => v,
                Err(e) => panic!("Failed to parse {}: {e}", path.display()),
            };

            assert_debug_snapshot!(ast);
        });
    }

    mod control_section {
        use crate::{
            control::control_section,
            node_values::{NodeValue, StringNode},
        };

        #[test]
        fn smoke() {
            let (remaining, res) = control_section("$version: \"2.0\"\n$hello: 123\n").unwrap();

            assert_eq!(remaining, "");
            assert_eq!(
                res,
                &[
                    ("version", NodeValue::String(StringNode::String("2.0"))),
                    ("hello", NodeValue::Number("123")),
                ]
            );
        }
    }

    mod metadata_section {
        use crate::{
            metadata::metadata_section,
            node_values::{NodeValue, StringNode},
        };

        #[test]
        fn smoke() {
            let (remaining, res) = metadata_section(
                "metadata \"foo\" = [\"baz\", \"bar\"]\nmetadata \"qux\" = \"test\"\n",
            )
            .unwrap();

            assert_eq!(remaining, "");
            assert_eq!(
                res,
                &[
                    (
                        "foo",
                        NodeValue::Array(vec![
                            NodeValue::String(StringNode::String("baz")),
                            NodeValue::String(StringNode::String("bar"))
                        ])
                    ),
                    ("qux", NodeValue::String(StringNode::String("test"))),
                ]
            );
        }
    }

    mod simple_shape {
        use crate::{
            node_values::{NodeKeyValuePair, NodeValue, StringNode},
            shape_id::{RootShapeId, ShapeId},
            shapes::{shape_section, ShapeOrApply, SimpleShape, SimpleTypeName},
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
                            mixins: vec![ShapeId {
                                root_shape_id: RootShapeId::Relative("IdBearer"),
                                shape_id_member: None,
                            }]
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
                            mixins: vec![
                                ShapeId {
                                    root_shape_id: RootShapeId::Relative("IdBearer"),
                                    shape_id_member: None,
                                },
                                ShapeId {
                                    root_shape_id: RootShapeId::Relative("Abc"),
                                    shape_id_member: None,
                                }
                            ]
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
                            shape_id: ShapeId {
                                root_shape_id: RootShapeId::Relative("myTrait"),
                                shape_id_member: None,
                            },
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
                            shape_id: ShapeId {
                                root_shape_id: RootShapeId::Relative("myTrait"),
                                shape_id_member: None,
                            },
                            body: Some(TraitBody::Structure(vec![
                                NodeKeyValuePair {
                                    key: "key",
                                    value: NodeValue::String(StringNode::String("value")),
                                },
                                NodeKeyValuePair {
                                    key: "otherKey",
                                    value: NodeValue::String(StringNode::String("otherValue")),
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
                            shape_id: ShapeId {
                                root_shape_id: RootShapeId::Relative("myTrait"),
                                shape_id_member: None,
                            },
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
                                shape_id: ShapeId {
                                    root_shape_id: RootShapeId::Relative("myTrait"),
                                    shape_id_member: None,
                                },
                                body: None,
                            },
                            Trait {
                                shape_id: ShapeId {
                                    root_shape_id: RootShapeId::Relative("myOtherTrait"),
                                    shape_id_member: None,
                                },
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
            node_values::{NodeValue, StringNode},
            shape_id::{RootShapeId, ShapeId},
            shapes::{
                shape_section, EnumShape, EnumShapeMember, EnumTypeName, Shape, ShapeOrApply,
                ShapeWithTraits,
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
                                        shape_id: ShapeId {
                                            root_shape_id: RootShapeId::Relative("deprecated"),
                                            shape_id_member: None,
                                        },
                                        body: None,
                                    }],
                                    identifier: "DIAMOND",
                                    value: Some(NodeValue::String(StringNode::String("diamond"))),
                                },
                                EnumShapeMember {
                                    traits: vec![],
                                    identifier: "CLUB",
                                    value: Some(NodeValue::String(StringNode::String("club"))),
                                },
                                EnumShapeMember {
                                    traits: vec![],
                                    identifier: "HEART",
                                    value: Some(NodeValue::String(StringNode::String("heart"))),
                                },
                                EnumShapeMember {
                                    traits: vec![Trait {
                                        shape_id: ShapeId {
                                            root_shape_id: RootShapeId::Relative("deprecated"),
                                            shape_id_member: None,
                                        },
                                        body: None,
                                    }],
                                    identifier: "SPADE",
                                    value: Some(NodeValue::String(StringNode::String("spade"))),
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
            node_values::{NodeKeyValuePair, NodeValue, StringNode},
            shape_id::{RootShapeId, ShapeId},
            shapes::{shape_section, ShapeOrApply},
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
                            shape_id: ShapeId {
                                root_shape_id: RootShapeId::Relative("documentation"),
                                shape_id_member: None,
                            },
                            body: Some(TraitBody::Node(NodeValue::String(StringNode::String(
                                "This is my string!"
                            )))),
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
                                shape_id: ShapeId {
                                    root_shape_id: RootShapeId::Relative("documentation"),
                                    shape_id_member: None,
                                },
                                body: Some(TraitBody::Node(NodeValue::String(StringNode::String(
                                    "This is my string!"
                                )))),
                            },
                            Trait {
                                shape_id: ShapeId {
                                    root_shape_id: RootShapeId::Relative("length"),
                                    shape_id_member: None,
                                },
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
        use crate::{
            shape_id::{AbsoluteRootShapeId, RootShapeId, ShapeId},
            shapes::{
                shape_section, AggregateShape, AggregateTypeName, Shape, ShapeMember, ShapeOrApply,
                ShapeSection, ShapeWithTraits,
            },
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
                            for_resource: Some(ShapeId {
                                root_shape_id: RootShapeId::Relative("Test"),
                                shape_id_member: None,
                            }),
                            mixins: vec![],
                            members: vec![
                                ShapeMember {
                                    traits: vec![],
                                    identifier: "a",
                                    shape_id: Some(ShapeId {
                                        root_shape_id: RootShapeId::Relative("MyString"),
                                        shape_id_member: None,
                                    }),
                                    value: None,
                                },
                                ShapeMember {
                                    traits: vec![],
                                    identifier: "b",
                                    shape_id: Some(ShapeId {
                                        root_shape_id: RootShapeId::Absolute(AbsoluteRootShapeId {
                                            namespace: "smithy.example",
                                            identifier: "MyString",
                                        }),
                                        shape_id_member: None,
                                    }),
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
            node_values::{NodeKeyValuePair, NodeValue, StringNode},
            shape_id::{RootShapeId, ShapeId},
            shapes::{
                shape_section, EntityShape, EntityTypeName, Shape, ShapeOrApply, ShapeSection,
                ShapeWithTraits,
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
                                    value: NodeValue::String(StringNode::String("2020-07-13")),
                                },
                                NodeKeyValuePair {
                                    key: "resources",
                                    value: NodeValue::Array(vec![NodeValue::String(
                                        StringNode::ShapeId(ShapeId {
                                            root_shape_id: RootShapeId::Relative("Model"),
                                            shape_id_member: None,
                                        })
                                    )]),
                                },
                                NodeKeyValuePair {
                                    key: "operations",
                                    value: NodeValue::Array(vec![NodeValue::String(
                                        StringNode::ShapeId(ShapeId {
                                            root_shape_id: RootShapeId::Relative("PingService"),
                                            shape_id_member: None,
                                        })
                                    )]),
                                },
                            ],
                        },),
                    },),],
                },)
            );
        }
    }

    mod operation {
        use crate::{
            shape_id::{RootShapeId, ShapeId},
            shapes::{
                shape_section, InlineAggregateShape, OperationProperty, OperationPropertyShape,
                OperationShape, Shape, ShapeMember, ShapeOrApply, ShapeSection, ShapeWithTraits,
            },
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
                                    ShapeId {
                                        root_shape_id: RootShapeId::Relative("PingServiceInput"),
                                        shape_id_member: None,
                                    },
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
                                                shape_id: Some(ShapeId {
                                                    root_shape_id: RootShapeId::Relative("String"),
                                                    shape_id_member: None,
                                                }),
                                                value: None,
                                            },
                                            ShapeMember {
                                                traits: vec![],
                                                identifier: "userId",
                                                shape_id: Some(ShapeId {
                                                    root_shape_id: RootShapeId::Relative("String"),
                                                    shape_id_member: None,
                                                }),
                                                value: None,
                                            }
                                        ]
                                    }
                                )),
                                OperationProperty::Errors(vec![
                                    ShapeId {
                                        root_shape_id: RootShapeId::Relative("UnavailableError"),
                                        shape_id_member: None,
                                    },
                                    ShapeId {
                                        root_shape_id: RootShapeId::Relative("BadRequestError"),
                                        shape_id_member: None,
                                    },
                                ]),
                            ],
                        }),
                    })],
                })
            );
        }
    }
}
