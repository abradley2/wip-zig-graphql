const std = @import("std");
const Allocator = std.mem.Allocator;

pub const SchemaDocument: type = []SchemaDeclaration;

pub const SchemaDeclaration: type = union(enum) {
    directive_declaration: DirectiveDeclaration,
    type_delcaration: TypeDeclaration,
};

pub const DirectiveDeclaration: type = struct {
    name: []const u8,
    arguments: ?[]ArgumentDefinition,
    targets: []DirectiveLocation,
};

pub const DirectiveTarget: type = []const u8;

pub const TypeDeclaration: type = struct {
    type_ref: NamedTypeRef,
    graphql_type: GraphQlType,
    implements: ?[]NamedTypeRef,
    directives: []Directive,
};

pub const GraphQlType: type = union(enum) {
    object_type: Object,
    scalar_type: void,
    union_type: []NamedTypeRef,
    enum_type: [][]const u8,
};

pub const ObjectKind: type = enum {
    default_type,
    interface_type,
    input_type,
};

pub const Object: type = struct {
    kind: ObjectKind,
    fields: []Field,
};

pub const NamedTypeRef: type = []const u8;

pub const ArgumentDefinition: type = struct {
    name: []const u8,
    named_type: NamedType,
    default: ?Value,
};

pub const Directive: type = struct {
    name: []const u8,
    arguments: ?[]ValuePair,
};

pub const Field: type = struct {
    name: []const u8,
    field_type: NamedType,
    arguments: ?[]ArgumentDefinition,
    directives: ?[]Directive,
};

pub const NamedType: type = struct {
    is_list: bool,
    is_nullable: bool,
    child: ?*NamedType,
    type_ref: ?NamedTypeRef,
};

pub const DirectiveLocation: type = enum {
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    FIELD,
    FRAGMENT_DEFINITION,
    FRAGMENT_SPREAD,
    INLINE_FRAGMENT,
    VARIABLE_DEFINITION,
    SCHEMA,
    SCALAR,
    OBJECT,
    FIELD_DEFINITION,
    ARGUMENT_DEFINITION,
    INTERFACE,
    UNION,
    ENUM,
    ENUM_VALUE,
    INPUT_OBJECT,
    INPUT_FIELD_DEFINITION,

    pub fn fromString(s: []const u8) ?DirectiveLocation {
        const enum_type = @typeInfo(DirectiveLocation).@"enum";

        comptime var kvs: [enum_type.fields.len]struct { []const u8, DirectiveLocation } = undefined;
        for (enum_type.fields, 0..) |enum_field, idx| {
            kvs[idx] = .{ enum_field.name, @as(DirectiveLocation, @enumFromInt(enum_field.value)) };
        }

        const ssm: std.StaticStringMap(DirectiveLocation) = .initComptime(kvs);

        return ssm.get(s);
    }
};

pub const ValueType: type = enum {
    int_type,
    float_type,
    string_type,
    boolean_type,
    null_type,
    object_type,
    list_type,
};

pub const ValuePair: type = struct {
    key: []const u8,
    value: Value,
};

pub const Value: type = union(ValueType) {
    int_type: i64,
    float_type: f64,
    string_type: []const u8,
    boolean_type: bool,
    null_type: void,
    object_type: []ValuePair,
    list_type: []Value,
};
