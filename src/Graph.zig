const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ast = @import("./ast.zig");
const json = std.json;
const Value = json.Value;

const ZigAst = std.zig.Ast;

const Graph = @This();

err_msg_buffer: [1_024]u8 = undefined,
err_msg: ?[]u8 = null,

schema_document: ast.SchemaDocument,

query_edges: ?[]Edge = null,
mutation_edges: ?[]Edge = null,
subscription_edges: ?[]Edge = null,

eval_fn: *const fn (edge: *const Edge, allocator: Allocator) error{OutOfMemory}!json.Value,

pub fn evalOperations(graph: *Graph, allocator: Allocator, operations: []ast.Operation) !json.Value {
    if (operations.len == 0) return .null;

    return resolveOperation(graph, allocator, operations[0]);
}

pub fn resolveOperation(graph: *Graph, allocator: Allocator, operation: ast.Operation) error{
    OutOfMemory,
    UndefinedOperation,
    UnknownField,
}!json.Value {
    const edges = switch (operation.operation_type) {
        .query => graph.query_edges,
        .mutation => graph.mutation_edges,
        .subscription => graph.subscription_edges,
    } orelse return error.UndefinedOperation;

    const return_object = try resolveSelection(graph, allocator, operation.selection_set, edges);
    return .{ .object = return_object };
}

pub fn resolveSelection(graph: *Graph, allocator: Allocator, selection_set: []ast.SelectionField, edges: []Edge) error{
    OutOfMemory,
    UnknownField,
}!json.ObjectMap {
    var object_map: json.ObjectMap = try .init(allocator, &.{}, &.{});
    for (selection_set) |selection| {
        const edge = ret: {
            for (edges) |*edge| {
                std.debug.print("Comparing {s} to {s}\n", .{ edge.name, selection.name });
                if (std.mem.eql(u8, edge.name, selection.name)) {
                    break :ret edge;
                }
            }
            break :ret null;
        } orelse continue;

        const value = try graph.eval_fn(edge, allocator);
        // use selection.name and edge to call the resolver

        try object_map.put(allocator, edge.name, value);

        if (selection.selection_set) |next_selection| {
            const next_edges = try traverse(allocator, edges, selection.name);
            _ = try resolveSelection(graph, allocator, next_selection, next_edges);
        }
    }

    return object_map;
}

pub const Edge: type = struct {
    left: []const u8,
    name: []const u8,
    arguments: ?[]ast.ArgumentDefinition = null,
    right: ast.GraphQlType,
};

pub fn codegen(
    graph: *Graph,
    allocator: Allocator,
) error{OutOfMemory}!void {
    _ = graph;
    _ = allocator;
}

pub fn init(
    graph: *Graph,
    allocator: Allocator,
    schema_document: ast.SchemaDocument,
    eval_fn: *const fn (edge: *const Edge, allocator: Allocator) error{OutOfMemory}!json.Value,
) error{
    OutOfMemory,
    InvalidSchemaDocument,
}!void {
    graph.eval_fn = eval_fn;
    var query_type_name: []const u8 = "Query";
    var mutation_type_name: []const u8 = "Mutation";
    var subscription_type_name: []const u8 = "Subscription";

    const schema_definition_opt: ?ast.SchemaDefinition = ret: {
        for (schema_document) |decl| {
            switch (decl.definition) {
                .schema_definition => |v| break :ret v,
                else => continue,
            }
        }

        break :ret null;
    };

    if (schema_definition_opt) |schema_definition| {
        const fields = schema_definition.fields orelse {
            _ = std.fmt.bufPrint(&graph.err_msg_buffer, "schema declaration has no fields", .{}) catch "";
            return error.InvalidSchemaDocument;
        };

        for (fields) |field| {
            if (std.mem.eql(u8, field.name, "query")) {
                query_type_name = field.graphql_type.name();
            }
            if (std.mem.eql(u8, field.name, "mutation")) {
                mutation_type_name = field.graphql_type.name();
            }
            if (std.mem.eql(u8, field.name, "subscription")) {
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
        if (std.mem.eql(u8, type_definition.name, query_type_name)) {
            query_type_definition = type_definition;
        }
        if (std.mem.eql(u8, type_definition.name, mutation_type_name)) {
            mutation_type_definition = type_definition;
        }
        if (std.mem.eql(u8, type_definition.name, subscription_type_name)) {
            subscription_type_definition = type_definition;
        }
    }

    if (query_type_definition) |def| {
        const query_fields = def.fields orelse {
            graph.err_msg = std.fmt.bufPrint(&graph.err_msg_buffer, "Query type has no fields", .{}) catch "";
            return error.InvalidSchemaDocument;
        };
        graph.query_edges = try fieldsToEdges(allocator, def.name, query_fields);
    }

    if (mutation_type_definition) |def| {
        const mutation_fields = def.fields orelse {
            graph.err_msg = std.fmt.bufPrint(&graph.err_msg_buffer, "Mutation type has no fields", .{}) catch "";
            return error.InvalidSchemaDocument;
        };
        graph.mutation_edges = try fieldsToEdges(allocator, def.name, mutation_fields);
    }

    if (query_type_definition) |def| {
        const query_fields = def.fields orelse {
            graph.err_msg = std.fmt.bufPrint(&graph.err_msg_buffer, "Subscription type has no fields", .{}) catch "";
            return error.InvalidSchemaDocument;
        };
        graph.subscription_edges = try fieldsToEdges(allocator, def.name, query_fields);
    }
}

fn fieldsToEdges(allocator: Allocator, name: []const u8, fields: []ast.Field) error{OutOfMemory}![]Edge {
    var edges: ArrayList(Edge) = try .initCapacity(allocator, fields.len);
    errdefer edges.deinit(allocator);

    for (fields) |field| {
        try edges.append(allocator, Edge{
            .name = field.name,
            .left = name,
            .arguments = field.arguments orelse &[_]ast.ArgumentDefinition{},
            .right = field.graphql_type,
        });
    }

    return try edges.toOwnedSlice(allocator);
}

pub fn traverse(
    leaky_allocator: Allocator,
    current_edges: []Edge,
    left: []const u8,
) error{OutOfMemory}![]Edge {
    var next_edges: ArrayList(Edge) = .empty;

    for (current_edges) |current_edge| {
        if (std.mem.eql(u8, current_edge.name, left)) {
            try next_edges.append(leaky_allocator, current_edge);
        }
    }

    return try next_edges.toOwnedSlice(leaky_allocator);
}
