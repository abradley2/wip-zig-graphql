const std = @import("std");
const Allocator = std.mem.Allocator;

pub const OperationType: type = enum {
    query,
    mutation,
    subscription,
};

pub const Operation: type = struct {
    operation_type: OperationType,
    name: ?[]const u8,
    directives: ?[]Directive,
    selection_set: []SelectionField,
    variables: ?[]ArgumentDefinition,
};

pub const SelectionField: type = struct {
    label: ?[]const u8,
    arguments: ?[]ValuePair,
    directives: ?[]Directive,
    name: []const u8,
    selection_set: ?[]SelectionField,
};

pub const SchemaDocument: type = []SchemaDeclaration;

pub const SchemaDeclaration: type = struct {
    description: ?[]const u8,
    extends: bool,
    definition: Definition,
};

pub const Definition: type = union(enum(u8)) {
    directive_definition: DirectiveDefinition,
    interface_definition: InterfaceDefinition,
    input_definition: InputDefinition,
    type_definition: TypeDefinition,
    schema_definition: SchemaDefinition,
    enum_definition: EnumDefinition,
    scalar_definition: ScalarDefinition,
    union_definition: UnionDefinition,
};

pub const ScalarDefinition: type = struct {
    name: []const u8,
    directives: ?[]Directive,
};

pub const SchemaDefinition: type = struct {
    directives: ?[]Directive,
    fields: ?[]Field,
};

pub const InterfaceDefinition: type = struct {
    name: []const u8,
    implements: ?[][]const u8,
    directives: ?[]Directive,
    fields: ?[]Field,
};

pub const TypeDefinition: type = struct {
    name: []const u8,
    implements: ?[][]const u8,
    directives: ?[]Directive,
    fields: ?[]Field,
};

pub const InputDefinition: type = struct {
    name: []const u8,
    directives: ?[]Directive,
    fields: ?[]Field,
};

pub const EnumDefinition: type = struct {
    name: []const u8,
    directives: ?[]Directive,
    entries: ?[]EnumEntryDefinition,
};

pub const UnionDefinition: type = struct {
    name: []const u8,
    directives: ?[]Directive,
    entries: ?[][]const u8,
};

pub const DirectiveDefinition: type = struct {
    repeatable: bool,
    name: []const u8,
    arguments: ?[]ArgumentDefinition,
    targets: []DirectiveTarget,
};

pub const EnumEntryDefinition: type = struct {
    description: ?[]const u8,
    name: []const u8,
    directives: ?[]Directive,
};

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
    default_value: ?Value,
};

pub const GraphQlType: type = struct {
    is_list: bool,
    is_nullable: bool,
    child: ?*GraphQlType,
    named_type: ?[]const u8,

    pub fn name(self: GraphQlType) []const u8 {
        if (self.named_type) |n| return n;
        if (self.child) |c| return c.name();
        @panic("Invalid GraphQlType");
    }
};

pub const DirectiveTarget: type = enum {
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

    pub fn fromString(s: []const u8) ?DirectiveTarget {
        const enum_type = @typeInfo(DirectiveTarget).@"enum";

        comptime var kvs: [enum_type.fields.len]struct { []const u8, DirectiveTarget } = undefined;
        inline for (enum_type.fields, 0..) |enum_field, idx| {
            kvs[idx] = .{ enum_field.name, @as(DirectiveTarget, @enumFromInt(enum_field.value)) };
        }

        const ssm: std.StaticStringMap(DirectiveTarget) = .initComptime(kvs);

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
    variable_type,
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
    variable_type: []const u8,
};
