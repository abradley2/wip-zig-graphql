const std = @import("std");
const Allocator = std.mem.Allocator;

pub const SchemaDocument: type = []SchemaDeclaration;

pub const SchemaDeclaration: type = union(enum) {
    directive_declaration: DirectiveDeclaration,
    type_declaration: TypeDeclaration,
};

pub const Declaration: type = struct {
    extends: bool,
    description: []const u8,
    definition: Definition,
};

pub const Definition: type = union(u8) {
    interface: InterfaceTypeDefinition,
    input: InputTypeDefinition,
    object: ObjectTypeDefinition,
};

pub const InterfaceTypeDefinition: type = struct {
    name: []const u8,
    implements: ?[]NamedType,
    directives: ?[]Directive,
    fields: ?[]Field,
};

pub const ObjectTypeDefinition: type = struct {
    name: []const u8,
    implements: ?[]NamedType,
    directives: ?[]Directive,
    fields: ?[]Field,
};

pub const InputTypeDefinition: type = struct {
    name: []const u8,
    directives: ?[]Directive,
    fields: ?[]Field,
};

pub const EnumTypeDefinition: type = struct {
    name: []const u8,
    directives: ?[]Directive,
    entries: ?[]EnumEntryDefinition,
};

pub const DirectiveDeclaration: type = struct {
    repeatable: bool,
    description: ?[]const u8 = null,
    name: []const u8,
    arguments: ?[]ArgumentDefinition,
    targets: []DirectiveLocation,
};

pub const DirectiveTarget: type = []const u8;

pub const TypeDeclaration: type = struct {
    description: ?[]const u8 = null,
    extends: bool,
    name: []const u8,
    definition: TypeDefinition,
    implements: ?[]NamedType,
    directives: ?[]Directive,
};

pub const TypeDefinition: type = union(enum) {
    schema_definition: ?[]Field,
    type_definition: ?[]Field,
    interface_definition: ?[]Field,
    input_definition: []Field,
    scalar_definition: void,
    enum_definition: []EnumEntryDefinition,
};

pub const EnumEntryDefinition: type = struct {
    name: []const u8,
    directives: ?[]Directive,
};

pub const NamedType: type = []const u8;

pub const ArgumentDefinition: type = struct {
    description: ?[]const u8,
    name: []const u8,
    graphql_type: GraphQlType,
    default: ?Value,
    directives: ?[]Directive,
};

pub const Directive: type = struct {
    name: []const u8,
    arguments: ?[]ValuePair,
};

pub const Field: type = struct {
    description: ?[]const u8,
    name: []const u8,
    graphql_type: GraphQlType,
    arguments: ?[]ArgumentDefinition,
    directives: ?[]Directive,
};

pub const GraphQlType: type = struct {
    is_list: bool,
    is_nullable: bool,
    child: ?*GraphQlType,
    named_type: ?NamedType,
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
        inline for (enum_type.fields, 0..) |enum_field, idx| {
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
