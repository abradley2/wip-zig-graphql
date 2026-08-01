const std = @import("std");
const schema_ast = @import("./schema_ast.zig");
const query_ast = @import("./query_ast.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Value = schema_ast.Value;
const ValuePair = schema_ast.ValuePair;

pub fn queryDefinitionFromSchema(schema: schema_ast.SchemaDocument) !schema_ast.TypeDefinition {
    for (schema) |*schema_declaration| {
        if (!std.mem.eql(u8, "Query", schema_ast.getDefinitionName(schema_declaration.definition))) {
            continue;
        }
        switch (schema_declaration.definition) {
            .type_definition => |def| {
                return def;
            },
            else => {
                return error.InvalidQueryDefinition;
            },
        }
    }
    return error.MissingQueryDefinition;
}

pub fn execute(
    arena: Allocator,
    resolver: fn (
        arena: Allocator,
        parent_definition: schema_ast.TypeDefinition,
        field_definition: schema_ast.Field,
        query_field: *const query_ast.QueryField,
        parent_value: ?*const Value,
    ) anyerror!Value,
    schema: schema_ast.SchemaDocument,
    query: query_ast.Operation,
) !Value {
    var fields: ArrayList(ValuePair) = .empty;

    for (query.selection) |*query_field| {
        const added_field = try fields.addOne(arena);
        added_field.key = query_field.name;

        const query_definition = try queryDefinitionFromSchema(schema);

        if (query_field.label) |label| {
            added_field.key = label;
            try resolveQueryField(
                arena,
                resolver,
                query_definition,
                query_field,
                schema,
                null,
                &added_field.value,
            );
        }
    }

    return .{ .object_type = try fields.toOwnedSlice(arena) };
}

fn fieldToTypeDefinition(
    schema: schema_ast.SchemaDocument,
    field_definition: schema_ast.Field,
) error{UnknownType}!?schema_ast.TypeDefinition {
    for (schema) |schema_declaration| {
        if (!std.mem.eql(u8, schema_ast.getDefinitionName(schema_declaration.definition), field_definition.graphql_type.name())) {
            continue;
        }
        switch (schema_declaration.definition) {
            .type_definition => |type_definition| return type_definition,
            else => return null,
        }
    }

    return error.UnknownType;
}

pub fn resolveQueryField(
    arena: Allocator,
    resolver: fn (
        arena: Allocator,
        parent_definition: schema_ast.TypeDefinition,
        field_definition: schema_ast.Field,
        query_field: *const query_ast.QueryField,
        parent_value: ?*const Value,
    ) anyerror!Value,
    parent_type: schema_ast.TypeDefinition,
    query_field: *const query_ast.QueryField,
    schema: schema_ast.SchemaDocument,
    parent_value: ?*const Value,
    value: *Value,
) !void {
    const parent_fields = parent_type.fields orelse return;

    const field_definition: schema_ast.Field = ret: {
        for (parent_fields) |parent_field| {
            if (std.mem.eql(u8, parent_field.name, query_field.name)) {
                break :ret parent_field;
            }
        }
        return error.UnknownField;
    };

    if (field_definition.isResolver()) {
        value.* = try resolver(
            arena,
            parent_type,
            field_definition,
            query_field,
            parent_value,
        );
    }

    _ = schema;
}
