const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ast = @import("./ast.zig");

const Graph = @This();

err_msg_buffer: [1_024]u8 = undefined,
err_msg: ?[]u8 = null,

schema_document: ast.SchemaDocument,

query_edges: ?[]Edge = null,
mutation_edges: ?[]Edge = null,
subscription_edges: ?[]Edge = null,

pub fn resolveOperation(graph: *Graph, allocator: Allocator, operation: ast.Operation) error{
    OutOfMemory,
    UndefinedOperation,
    UnknownField,
}!void {
    const edges = switch (operation.operation_type) {
        .query => graph.query_edges,
        .mutation => graph.mutation_edges,
        .subscription => graph.subscription_edges,
    } orelse return error.UndefinedOperation;

    try resolveSelection(allocator, operation.selection_set, edges);
}

pub const ResolvedValue: type = struct {};

pub const Resolver: type = struct {
    a: fn (name: []const u8, edge: Edge) anyerror!ResolvedValue,
};

pub fn resolveSelection(allocator: Allocator, selection_set: []ast.SelectionField, edges: []Edge) error{
    OutOfMemory,
    UnknownField,
}!void {
    for (selection_set) |selection| {
        const edge = ret: {
            for (edges) |edge| {
                if (std.mem.eql(edge.left, selection.name)) {
                    break :ret edge;
                }
            }
            break :ret null;
        } orelse return error.UnknownField;

        _ = edge;
        // use selection.name and edge to call the resolver

        if (selection.selection_set) |next_selection| {
            const next_edges = try traverse(allocator, edges, selection.name);
            try resolveSelection(allocator, next_selection, next_edges);
        }
    }
}

pub const Edge: type = struct {
    name: []const u8,
    left: []const u8,
    arguments: []ast.ArgumentDefinition,
    right: ast.GraphQlType,
};

pub fn init(
    graph: *Graph,
    allocator: Allocator,
    schema_document: ast.SchemaDocument,
) error{
    OutOfMemory,
    InvalidSchemaDocument,
}!void {
    var query_type_name: []const u8 = "Query";
    var mutation_type_name: []const u8 = "Mutation";
    var subscription_type_name: []const u8 = "Subscription";

    const schema_definition_opt: ?ast.SchemaDefinition = ret: {
        for (schema_document) |decl| {
            switch (decl.definition) {
                .schema_definition => |v| v,
                else => continue,
            }
        }

        break :ret null;
    };

    if (schema_definition_opt) |schema_definition| {
        const fields = schema_definition.fields orelse {
            graph.err_msg = try std.fmt.bufPrint(graph.err_msg_buffer, "schema declaration has no fields", .{});
            return error.InvalidSchema;
        };

        for (fields) |field| {
            if (std.mem.eql(field.name, "query")) {
                query_type_name = field.graphql_type.name();
            }
            if (std.mem.eql(field.name, "mutation")) {
                mutation_type_name = field.graphql_type.name();
            }
            if (std.mem.eql(field.name, "subscription")) {
                subscription_type_name = field.graphql_type.name();
            }
        }
    }

    var query_type_definition: ?ast.TypeDefinition = null;
    var mutation_type_definition: ?ast.TypeDefinition = null;
    var subscription_type_definition: ?ast.TypeDefinition = null;

    for (schema_document) |decl| {
        const type_definition = switch (decl.definition) {
            .type_definition => |v| v,
            else => continue,
        };
        if (std.mem.eql(type_definition.name, query_type_name)) {
            query_type_definition = type_definition;
        }
        if (std.mem.eql(type_definition.name, mutation_type_name)) {
            mutation_type_definition = type_definition;
        }
        if (std.mem.eql(type_definition.name, subscription_type_definition)) {
            subscription_type_definition = type_definition;
        }
    }

    if (query_type_definition) |def| {
        const query_fields = def.fields orelse {
            graph.err_msg = std.fmt.bufPrint(graph.err_msg_buffer, "Query type has no fields", .{});
            return error.InvalidSchemaDocument;
        };
        graph.query_edges = fieldsToEdges(allocator, def.name, query_fields);
    }

    if (mutation_type_definition) |def| {
        const mutation_fields = def.fields orelse {
            graph.err_msg = std.fmt.bufPrint(graph.err_msg_buffer, "Mutation type has no fields", .{});
            return error.InvalidSchemaDocument;
        };
        graph.mutation_edges = fieldsToEdges(allocator, def.name, mutation_fields);
    }

    if (query_type_definition) |def| {
        const query_fields = def.fields orelse {
            graph.err_msg = std.fmt.bufPrint(graph.err_msg_buffer, "Subscription type has no fields", .{});
            return error.InvalidSchemaDocument;
        };
        graph.subscription_edges = fieldsToEdges(allocator, def.name, query_fields);
    }
}

fn fieldsToEdges(allocator: Allocator, name: []const u8, fields: []ast.Field) error{OutOfMemory}![]Edge {
    var edges: ArrayList(Edge) = .empty;
    errdefer edges.deinit(allocator);

    for (fields) |field| {
        try edges.append(allocator, Edge{
            .name = name,
            .left = field.name,
            .arguments = field.arguments,
            .right = field.graphql_type,
        });
    }

    return try edges.toOwnedSlice(allocator);
}

pub fn traverse(
    leaky_allocator: Allocator,
    current_edges: []Edge,
    left: *const []u8,
) error{OutOfMemory}![]Edge {
    var next_edges: ArrayList(Edge) = .empty;

    for (current_edges) |current_edge| {
        if (std.mem.eql(u8, current_edge.name, left)) {
            try next_edges.append(leaky_allocator, current_edge);
        }
    }

    return try next_edges.toOwnedSlice(leaky_allocator);
}
