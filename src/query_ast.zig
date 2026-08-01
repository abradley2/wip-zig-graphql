const schema_ast = @import("./schema_ast.zig");

const Value = schema_ast.Value;

pub const OperationType: type = enum(u8) {
    query,
    mutation,
    subscription,
};

pub const VariableDefinition: type = struct {
    name: []const u8,
    typ: schema_ast.GraphQlType,
};

pub const Operation: type = struct {
    typ: OperationType,
    variable_definitions: []const VariableDefinition,
};

pub const QueryField: type = struct {
    arguments: Arguments,
};

pub const Arguments: type = []Argument;

pub const Argument: type = struct {
    name: []const u8,
    value: ArgumentValue,
};

pub const ArgumentValue: type = union(enum(u8)) {
    literal: Value,
    variable: []const u8,
};
