//! Implements Smithy 2.0's [Semantic Model](https://smithy.io/2.0/spec/model.html#the-semantic-model).
//!
//! This crate implements conversions between `.smithy` file AST and the Semantic Model, which is the in-memory
//! model used by tools.
//!
//! The Semantic Model can be serialized down to JSON using `serde` to emit the
//! [JSON AST](https://smithy.io/2.0/spec/json-ast.html), or can be converted from an [`ironsmith_parser::Ast`].

use std::collections::BTreeMap;

use ironsmith_parser::{
    node_values::{NodeValue as AstNodeValue, StringNode},
    shape_id::AbsoluteRootShapeId,
    shapes::{
        AggregateShape as AstAggregateShape, AggregateTypeName, EntityShape, EntityTypeName,
        EnumShape as AstEnumShape, EnumTypeName, OperationProperty, OperationPropertyShape,
        OperationShape as AstOperationShape, Shape as AstShape, ShapeOrApply, SimpleShape,
        SimpleTypeName,
    },
    traits::TraitBody,
};
use serde::{Deserialize, Serialize};

pub enum Error<'a> {
    /// Invalid type for control field `{0}`, wanted value of type `{1}` but got `{2:?}`
    InvalidControlType(
        &'a str,
        &'a str,
        ironsmith_parser::node_values::NodeValue<'a>,
    ),
    MapMissingKey,
    MapMissingValue,
    MapInvalidMember,
    MalformedShapeId,
    InvalidType,
}

#[derive(Serialize, Deserialize)]
pub struct SemanticModel<'a> {
    smithy: &'a str,
    metadata: BTreeMap<&'a str, NodeValue<'a>>,
    shapes: BTreeMap<&'a str, OuterShape<'a>>,
}

impl<'a> TryFrom<ironsmith_parser::Ast<'a>> for SemanticModel<'a> {
    type Error = Error<'a>;

    fn try_from(value: ironsmith_parser::Ast<'a>) -> Result<Self, Self::Error> {
        let smithy = match value.control.into_iter().find(|(k, _)| *k == "version") {
            Some((_, AstNodeValue::String(StringNode::String(version)))) => version,
            Some((_, v)) => return Err(Error::InvalidControlType("version", "string", v)),
            None => "2.0",
        };

        let metadata = value
            .metadata
            .into_iter()
            .map(|(k, v)| (k, v.into()))
            .collect();

        let Some(ast_shapes) = value.shapes else {
            return Ok(SemanticModel {
                smithy,
                metadata,
                shapes: BTreeMap::new(),
            });
        };

        let imports = ast_shapes.uses.as_slice();
        let namespace = ast_shapes.namespace;

        let mut shapes = BTreeMap::new();

        for shape in ast_shapes.shapes {
            let shape = match shape {
                ShapeOrApply::Shape(shape) => shape,
                ShapeOrApply::Apply(apply) => {
                    // TODO: this also needs to try and merge if `shape_id` exists in the output map already
                    shapes.insert(
                        apply.shape_id,
                        OuterShape {
                            traits: transform_traits(apply.traits, namespace, imports),
                            mixins: vec![],
                            inner: Shape::Apply,
                        },
                    );
                    continue;
                }
            };

            let mixins = shape
                .shape
                .mixins()
                .iter()
                .copied()
                .map(|v| resolve_type_name(v, namespace, imports))
                .collect();
            let traits = transform_traits(shape.traits, namespace, imports);

            let (identifier, shape) = match shape.shape {
                AstShape::Simple(SimpleShape {
                    type_name,
                    identifier,
                    ..
                }) => (identifier, type_name.into()),
                AstShape::Enum(AstEnumShape {
                    type_name,
                    identifier,
                    members,
                    ..
                }) => {
                    let shape = AggregateShape {
                        members: members
                            .into_iter()
                            .map(|v| {
                                let mut traits = transform_traits(v.traits, namespace, imports);

                                if let Some(value) = v.value {
                                    traits.0.insert(
                                        ShapeId {
                                            namespace: "smithy.api",
                                            shape: "enumValue",
                                        },
                                        value.into(),
                                    );
                                }

                                let reference = ReferenceWithTraits {
                                    inner: Reference {
                                        target: ShapeId {
                                            namespace: "smithy.api",
                                            shape: "Unit",
                                        },
                                    },
                                    traits,
                                };

                                (v.identifier, reference)
                            })
                            .collect(),
                    };

                    let shape = match type_name {
                        EnumTypeName::IntEnum => Shape::IntEnum(shape),
                        EnumTypeName::Enum => Shape::Enum(shape),
                    };

                    (identifier, shape)
                }
                AstShape::Aggregate(AstAggregateShape {
                    type_name: AggregateTypeName::Map,
                    identifier,
                    mut members,
                    ..
                }) => {
                    let key = members
                        .iter()
                        .position(|v| v.identifier == "key")
                        .ok_or(Error::MapMissingKey)?;
                    let key = members.swap_remove(key);

                    let value = members
                        .iter()
                        .position(|v| v.identifier == "value")
                        .ok_or(Error::MapMissingValue)?;
                    let value = members.swap_remove(value);

                    if !members.is_empty() {
                        return Err(Error::MapInvalidMember);
                    }

                    let shape = MapShape {
                        key: ReferenceWithTraits {
                            inner: resolve_type_name(key.shape_id.unwrap(), namespace, imports),
                            traits: transform_traits(key.traits, namespace, imports),
                        },
                        value: ReferenceWithTraits {
                            inner: resolve_type_name(value.shape_id.unwrap(), namespace, imports), // TODO
                            traits: transform_traits(value.traits, namespace, imports),
                        },
                    };

                    (identifier, Shape::Map(shape))
                }
                AstShape::Aggregate(AstAggregateShape {
                    type_name: AggregateTypeName::List,
                    identifier,
                    mut members,
                    ..
                }) => {
                    let member = members
                        .iter()
                        .position(|v| v.identifier == "member")
                        .ok_or(Error::MapMissingKey)?;
                    let member = members.swap_remove(member);

                    if !members.is_empty() {
                        return Err(Error::MapInvalidMember);
                    }

                    let shape = ListShape {
                        member: ReferenceWithTraits {
                            inner: resolve_type_name(member.shape_id.unwrap(), namespace, imports),
                            traits: transform_traits(member.traits.clone(), namespace, imports),
                        },
                    };

                    (identifier, Shape::List(shape))
                }
                AstShape::Aggregate(AstAggregateShape {
                    type_name: type_name @ (AggregateTypeName::Structure | AggregateTypeName::Union),
                    identifier,
                    members,
                    ..
                }) => {
                    let shape = AggregateShape {
                        members: members
                            .into_iter()
                            .map(|v| {
                                let reference = ReferenceWithTraits {
                                    inner: resolve_type_name(
                                        v.shape_id.unwrap(),
                                        namespace,
                                        imports,
                                    ), // TODO
                                    traits: transform_traits(v.traits, namespace, imports),
                                };

                                (v.identifier, reference)
                            })
                            .collect(),
                    };

                    let shape = match type_name {
                        AggregateTypeName::Structure => Shape::Structure(shape),
                        AggregateTypeName::Union => Shape::Union(shape),
                        AggregateTypeName::Map | AggregateTypeName::List => unreachable!(),
                    };

                    (identifier, shape)
                }
                AstShape::Entity(EntityShape {
                    type_name: EntityTypeName::Service,
                    identifier,
                    mut nodes,
                    ..
                }) => {
                    let mut take = |key| {
                        nodes
                            .iter()
                            .position(|v| v.key == key)
                            .map(|index| nodes.swap_remove(index))
                            .map(|kv| unpack_node_value_to_shapes(kv.value, namespace, imports))
                            .transpose()
                    };

                    let operations = take("operations")?.unwrap_or_default();
                    let resources = take("resources")?.unwrap_or_default();
                    let errors = take("errors")?.unwrap_or_default();
                    let rename = nodes
                        .iter()
                        .position(|v| v.key == "rename")
                        .map(|index| nodes.swap_remove(index))
                        .map(|kv| match kv.value {
                            AstNodeValue::Object(v) => v
                                .into_iter()
                                .map(|rename| {
                                    let key = ShapeId::try_from(rename.key)?;
                                    let value = match rename.value {
                                        AstNodeValue::String(StringNode::String(s)) => s,
                                        _ => return Err(Error::InvalidType),
                                    };
                                    Ok((key, value))
                                })
                                .collect(),
                            _ => Err(Error::InvalidType),
                        })
                        .transpose()?
                        .unwrap_or_default();

                    let shape = ServiceShape {
                        operations,
                        resources,
                        errors,
                        rename,
                    };

                    (identifier, Shape::Service(shape))
                }
                AstShape::Entity(EntityShape {
                    type_name: EntityTypeName::Resource,
                    identifier,
                    mut nodes,
                    ..
                }) => {
                    let mut take_map = |key| {
                        nodes
                            .iter()
                            .position(|v| v.key == key)
                            .map(|index| nodes.swap_remove(index))
                            .map(|v| match v.value {
                                AstNodeValue::Object(obj) => obj
                                    .into_iter()
                                    .map(|v| {
                                        let value = unpack_node_value_to_shape(
                                            v.value, namespace, imports,
                                        )?
                                        .inner;
                                        Ok((v.key, value))
                                    })
                                    .collect(),
                                _ => Err(Error::InvalidType),
                            })
                            .transpose()
                    };

                    let identifiers = take_map("identifiers")?.unwrap_or_default();
                    let properties = take_map("properties")?.unwrap_or_default();

                    let mut take_one = |key| {
                        nodes
                            .iter()
                            .position(|v| v.key == key)
                            .map(|index| nodes.swap_remove(index))
                            .map(|kv| {
                                unpack_node_value_to_shape(kv.value, namespace, imports)
                                    .map(|v| v.inner)
                            })
                            .transpose()
                    };

                    let create = take_one("create")?;
                    let put = take_one("put")?;
                    let read = take_one("read")?;
                    let update = take_one("update")?;
                    let delete = take_one("delete")?;
                    let list = take_one("list")?;

                    let mut take_many = |key| {
                        nodes
                            .iter()
                            .position(|v| v.key == key)
                            .map(|index| nodes.swap_remove(index))
                            .map(|kv| unpack_node_value_to_shapes(kv.value, namespace, imports))
                            .transpose()
                    };

                    let operations = take_many("operations")?.unwrap_or_default();
                    let collection_operations =
                        take_many("collectionOperations")?.unwrap_or_default();
                    let resources = take_many("resources")?.unwrap_or_default();

                    let shape = ResourceShape {
                        identifiers,
                        properties,
                        create,
                        put,
                        read,
                        update,
                        delete,
                        list,
                        operations,
                        collection_operations,
                        resources,
                    };

                    (identifier, Shape::Resource(shape))
                }
                AstShape::Operation(AstOperationShape {
                    identifier, body, ..
                }) => {
                    let mut shape = OperationShape::default();

                    for val in body {
                        match val {
                            OperationProperty::Input(OperationPropertyShape::Explicit(
                                type_name,
                            )) => {
                                shape.input =
                                    Some(resolve_type_name(type_name, namespace, imports));
                            }
                            OperationProperty::Output(OperationPropertyShape::Explicit(
                                type_name,
                            )) => {
                                shape.output =
                                    Some(resolve_type_name(type_name, namespace, imports));
                            }
                            OperationProperty::Errors(errors) => {
                                shape.errors = errors
                                    .into_iter()
                                    .map(|type_name| {
                                        resolve_type_name(type_name, namespace, imports)
                                    })
                                    .collect();
                            }
                            _ => todo!(),
                        }
                    }

                    (identifier, Shape::Operation(shape))
                }
            };

            let shape = OuterShape {
                traits,
                mixins,
                inner: shape,
            };

            shapes.insert(identifier, shape);
        }

        Ok(Self {
            smithy,
            metadata,
            shapes,
        })
    }
}

fn unpack_node_value_to_shape<'a>(
    node_value: AstNodeValue<'a>,
    namespace: &'a str,
    imports: &[AbsoluteRootShapeId<'a>],
) -> Result<ReferenceWithTraits<'a>, Error<'a>> {
    match node_value {
        AstNodeValue::String(StringNode::ShapeId(type_name)) => Ok(ReferenceWithTraits {
            inner: resolve_type_name(type_name, namespace, imports),
            traits: Traits(BTreeMap::new()),
        }),
        _ => Err(Error::InvalidType),
    }
}

fn unpack_node_value_to_shapes<'a>(
    node_value: AstNodeValue<'a>,
    namespace: &'a str,
    imports: &[AbsoluteRootShapeId<'a>],
) -> Result<Vec<Reference<'a>>, Error<'a>> {
    let array = match node_value {
        AstNodeValue::Array(v) => v,
        _ => return Err(Error::InvalidType),
    };

    array
        .into_iter()
        .map(|v| unpack_node_value_to_shape(v, namespace, imports).map(|v| v.inner))
        .collect()
}

fn transform_traits<'a>(
    traits: Vec<ironsmith_parser::traits::Trait<'a>>,
    namespace: &'a str,
    imports: &[AbsoluteRootShapeId<'a>],
) -> Traits<'a> {
    Traits(
        traits
            .into_iter()
            .map(|v| {
                (
                    resolve_type_name(v.shape_id, namespace, imports).target,
                    match v.body {
                        Some(TraitBody::Structure(v)) => NodeValue::Object(
                            v.into_iter().map(|v| (v.key, v.value.into())).collect(),
                        ),
                        Some(TraitBody::Node(node)) => node.into(),
                        None => NodeValue::Object(BTreeMap::new()),
                    },
                )
            })
            .collect(),
    )
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ShapeId<'a> {
    pub namespace: &'a str,
    pub shape: &'a str,
}

impl<'a> TryFrom<&'a str> for ShapeId<'a> {
    type Error = Error<'a>;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        if let Some((namespace, shape)) = s.rsplit_once("#") {
            Ok(ShapeId { namespace, shape })
        } else {
            Err(Error::MalformedShapeId)
        }
    }
}

impl Serialize for ShapeId<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&format!("{}#{}", self.namespace, self.shape))
    }
}

impl<'a, 'de: 'a> Deserialize<'de> for ShapeId<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = <&str>::deserialize(deserializer)?;

        if let Some((namespace, shape)) = s.rsplit_once("#") {
            Ok(ShapeId { namespace, shape })
        } else {
            Err(serde::de::Error::custom(
                "malformed shape id, missing namespace separator",
            ))
        }
    }
}

fn resolve_type_name<'a>(
    type_name: ironsmith_parser::shape_id::ShapeId<'a>,
    namespace: &'a str,
    imports: &[AbsoluteRootShapeId<'a>],
) -> Reference<'a> {
    let target = match type_name.root_shape_id {
        ironsmith_parser::shape_id::RootShapeId::Relative(relative) => {
            if let Some(import) = imports.iter().find(|v| v.identifier == relative) {
                ShapeId {
                    namespace: import.namespace,
                    shape: import.identifier,
                }
            } else if BUILTINS.contains(relative) {
                ShapeId {
                    namespace: "smithy.api",
                    shape: relative,
                }
            } else {
                ShapeId {
                    namespace,
                    shape: relative,
                }
            }
        }
        ironsmith_parser::shape_id::RootShapeId::Absolute(absolute_root_shape_id) => ShapeId {
            namespace: absolute_root_shape_id.namespace,
            shape: absolute_root_shape_id.identifier,
        },
    };

    Reference { target }
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
pub enum NodeValue<'a> {
    Array(Vec<NodeValue<'a>>),
    Object(BTreeMap<&'a str, NodeValue<'a>>),
    Number(&'a str),
    Bool(bool),
    Null,
    String(&'a str),
}

impl<'a> From<AstNodeValue<'a>> for NodeValue<'a> {
    fn from(value: AstNodeValue<'a>) -> Self {
        match value {
            AstNodeValue::Array(vec) => Self::Array(vec.into_iter().map(Into::into).collect()),
            AstNodeValue::Object(vec) => {
                Self::Object(vec.into_iter().map(|v| (v.key, v.value.into())).collect())
            }
            AstNodeValue::Number(v) => NodeValue::Number(v),
            AstNodeValue::Keyword(ironsmith_parser::node_values::Keyword::True) => {
                NodeValue::Bool(true)
            }
            AstNodeValue::Keyword(ironsmith_parser::node_values::Keyword::False) => {
                NodeValue::Bool(false)
            }
            AstNodeValue::Keyword(ironsmith_parser::node_values::Keyword::Null) => NodeValue::Null,
            AstNodeValue::String(StringNode::String(s)) => NodeValue::String(s),
            AstNodeValue::String(StringNode::ShapeId(_)) => todo!(),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Traits<'a>(#[serde(borrow)] BTreeMap<ShapeId<'a>, NodeValue<'a>>);

#[derive(Serialize, Deserialize)]
pub struct OuterShape<'a> {
    pub traits: Traits<'a>,
    #[serde(borrow)]
    pub mixins: Vec<Reference<'a>>,
    #[serde(flatten)]
    pub inner: Shape<'a>,
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum Shape<'a> {
    String,
    Blob,
    Boolean,
    Document,
    Byte,
    Short,
    Integer,
    Long,
    Float,
    Double,
    BigInteger,
    BigDecimal,
    Timestamp,
    List(#[serde(borrow)] ListShape<'a>),
    Service(ServiceShape<'a>),
    Resource(ResourceShape<'a>),
    Operation(OperationShape<'a>),
    Map(MapShape<'a>),
    Structure(AggregateShape<'a>),
    Union(AggregateShape<'a>),
    Enum(AggregateShape<'a>),
    IntEnum(AggregateShape<'a>),
    Apply,
}

impl From<SimpleTypeName> for Shape<'_> {
    fn from(value: SimpleTypeName) -> Self {
        match value {
            SimpleTypeName::Blob => Self::Blob,
            SimpleTypeName::Boolean => Self::Boolean,
            SimpleTypeName::Document => Self::Document,
            SimpleTypeName::String => Self::String,
            SimpleTypeName::Byte => Self::Byte,
            SimpleTypeName::Short => Self::Short,
            SimpleTypeName::Integer => Self::Integer,
            SimpleTypeName::Long => Self::Long,
            SimpleTypeName::Float => Self::Float,
            SimpleTypeName::Double => Self::Double,
            SimpleTypeName::BigInteger => Self::BigInteger,
            SimpleTypeName::BigDecimal => Self::BigDecimal,
            SimpleTypeName::Timestamp => Self::Timestamp,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct ListShape<'a> {
    #[serde(borrow)]
    pub member: ReferenceWithTraits<'a>,
}

#[derive(Serialize, Deserialize)]
#[serde(bound(deserialize = "'a: 'de"))]
pub struct ServiceShape<'a> {
    #[serde(borrow)]
    pub operations: Vec<Reference<'a>>,
    #[serde(borrow)]
    pub resources: Vec<Reference<'a>>,
    #[serde(borrow)]
    pub errors: Vec<Reference<'a>>,
    pub rename: BTreeMap<ShapeId<'a>, &'a str>,
}

#[derive(Serialize, Deserialize)]
pub struct ResourceShape<'a> {
    pub identifiers: BTreeMap<&'a str, Reference<'a>>,
    pub properties: BTreeMap<&'a str, Reference<'a>>,
    pub create: Option<Reference<'a>>,
    pub put: Option<Reference<'a>>,
    pub read: Option<Reference<'a>>,
    pub update: Option<Reference<'a>>,
    pub delete: Option<Reference<'a>>,
    pub list: Option<Reference<'a>>,
    #[serde(borrow)]
    pub operations: Vec<Reference<'a>>,
    #[serde(rename = "collectionOperations")]
    pub collection_operations: Vec<Reference<'a>>,
    pub resources: Vec<Reference<'a>>,
}

#[derive(Serialize, Deserialize, Default)]
pub struct OperationShape<'a> {
    #[serde(borrow)]
    pub input: Option<Reference<'a>>,
    pub output: Option<Reference<'a>>,
    pub errors: Vec<Reference<'a>>,
}

#[derive(Serialize, Deserialize)]
pub struct ReferenceWithTraits<'a> {
    #[serde(flatten)]
    pub inner: Reference<'a>,
    #[serde(borrow)]
    pub traits: Traits<'a>,
}

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Reference<'a> {
    #[serde(borrow)]
    pub target: ShapeId<'a>,
}

#[derive(Serialize, Deserialize)]
pub struct MapShape<'a> {
    #[serde(borrow)]
    pub key: ReferenceWithTraits<'a>,
    pub value: ReferenceWithTraits<'a>,
}

#[derive(Serialize, Deserialize)]
pub struct AggregateShape<'a> {
    #[serde(borrow)]
    pub members: BTreeMap<&'a str, ReferenceWithTraits<'a>>,
}

/// A list of builtins provided by Smithy. If encountered during conversion to our semantic model, we'll pretend
/// these were imported from `smithy.api`.
static BUILTINS: phf::Set<&'static str> = phf::phf_set! {
    "String",
    "Blob",
    "BigInteger",
    "BigDecimal",
    "Timestamp",
    "Document",
    "Boolean",
    "Byte",
    "Short",
    "Integer",
    "Long",
    "Float",
    "Double",
    "PrimitiveBoolean",
    "PrimitiveByte",
    "PrimitiveShort",
    "PrimitiveInteger",
    "PrimitiveLong",
    "PrimitiveFloat",
    "PrimitiveDouble",
    "Unit",
    "trait",
    "deprecated",
    "documentation",
    "externalDocumentation",
    "auth",
    "protocolDefinition",
    "authDefinition",
    "httpBasicAuth",
    "httpDigestAuth",
    "httpBearerAuth",
    "httpApiKeyAuth",
    "traitValidators",
    "default",
    "addedDefault",
    "clientOptional",
    "optionalAuth",
    "examples",
    "error",
    "retryable",
    "readonly",
    "idempotent",
    "idempotencyToken",
    "internal",
    "jsonName",
    "xmlAttribute",
    "xmlFlattened",
    "xmlName",
    "xmlNamespace",
    "noReplace",
    "mediaType",
    "references",
    "resourceIdentifier",
    "private",
    "sensitive",
    "since",
    "streaming",
    "requiresLength",
    "tags",
    "title",
    "enum",
    "enumValue",
    "length",
    "range",
    "pattern",
    "required",
    "property",
    "notProperty",
    "nestedProperties",
    "recommended",
    "sparse",
    "uniqueItems",
    "unstable",
    "paginated",
    "http",
    "httpLabel",
    "httpQuery",
    "httpQueryParams",
    "httpHeader",
    "httpPrefixHeaders",
    "httpPayload",
    "httpError",
    "httpResponseCode",
    "cors",
    "eventPayload",
    "eventHeader",
    "idRef",
    "timestampFormat",
    "endpoint",
    "hostLabel",
    "suppress",
    "httpChecksumRequired",
    "input",
    "output",
    "unitType",
    "mixin",
    "requestCompression",
};
