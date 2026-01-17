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

test "getDefinitionName" {
    const union_def: Definition = .{
        .union_definition = UnionDefinition{
            .name = "MyUnion",
            .directives = null,
            .entries = null,
        },
    };

    const input_def: Definition = .{
        .input_definition = InputDefinition{
            .name = "MyInput",
            .directives = null,
            .fields = null,
        },
    };

    try std.testing.expectEqualStrings("MyUnion", getDefinitionName(union_def));
    try std.testing.expectEqualStrings("MyInput", getDefinitionName(input_def));
}

pub fn getDefinitionName(definition: Definition) []const u8 {
    return CommonField(Definition, "name").get(definition);
}

fn CommonField(comptime T: type, comptime field_name: []const u8) type {
    const type_info = @typeInfo(T);
    const union_type: std.builtin.Type.Union = switch (type_info) {
        .@"union" => |v| v,
        else => @compileError("Must be a union type"),
    };

    inline for (union_type.fields) |union_field| {
        const struct_type: std.builtin.Type.Struct = switch (@typeInfo(union_field.type)) {
            .@"struct" => |v| v,
            else => @compileError("All members of this union must be a non-pointer struct"),
        };

        var found: bool = false;
        inline for (struct_type.fields) |struct_field| {
            if (std.mem.eql(u8, struct_field.name, field_name)) {
                found = true;
                break;
            }
        }

        if (!found) {
            @compileError("Type is missing common field");
        }
    }

    return struct {
        const Self = @This();
        const union_fields = union_type.fields;
        const UnionEnumType = union_type.tag_type orelse @compileError("Must declare an enum type for this union");

        pub fn get(t: T) []const u8 {
            const initialized_member_index: usize = @intCast(@intFromEnum(t));

            inline for (Self.union_fields, 0..) |union_field, enum_value| {
                if (enum_value == initialized_member_index) {
                    const a: union_field.type = @field(t, union_field.name);
                    return @field(a, field_name);
                }
            }
            @panic("Union enum index out of bounds");
        }
    };
}

pub const ScalarDefinition: type = struct {
    name: []const u8,
    directives: ?[]Directive,
};

pub const SchemaDefinition: type = struct {
    name: []const u8 = "schema",
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
    is_list: bool = false,
    is_nullable: bool = true,
    child: ?*const GraphQlType = null,
    named_type: ?[]const u8 = null,

    pub fn name(self: GraphQlType) []const u8 {
        if (self.child) |c| return c.name();
        if (self.named_type) |n| return n;
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
