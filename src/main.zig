const std = @import("std");
const builtin = @import("builtin");
const graphql = @import("graphql");
const json = std.json;

const Allocator = std.mem.Allocator;
const Lexer = graphql.Lexer;
const Parser = graphql.Parser;
const Graph = graphql.Graph;
const GraphQlType = graphql.GraphQlType;
const Edge = Graph.Edge;

test "all tests" {
    std.testing.refAllDecls(@This());
}

const DebugAllocator: type = std.heap.DebugAllocator(.{});

pub fn main() !void {
    const input =
        \\type Todo {
        \\  id: ID!
        \\  name: String!
        \\  completed: Boolean!
        \\}
        \\
        \\type Query {
        \\  todos(filter_completed: Boolean = null): [Todo]
        \\}
    ;
    var lexer: Lexer = .init(input);
    var parser: Parser = try .init(&lexer);

    var debug_allocator: ?std.heap.DebugAllocator(.{}) = null;

    var allocator: Allocator = ret: {
        if (builtin.mode == .Debug or builtin.mode == .ReleaseSafe) {
            debug_allocator = DebugAllocator.init;
            break :ret debug_allocator.?.allocator();
        }
        break :ret std.heap.smp_allocator;
    };

    var arena_allocator: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    defer arena_allocator.deinit();
    const leaky_allocator = arena_allocator.allocator();

    const schema_document = try parser.parseSchemaDocument(leaky_allocator);

    const graph = try allocator.create(Graph);
    try graph.init(allocator, schema_document, &eval);

    const sample_query =
        \\todos(filter_completed: null) {
        \\  id
        \\  name
        \\  completed
        \\}
    ;

    lexer = .init(sample_query);
    parser = try .init(&lexer);
    const operations = try parser.parseQueryDocument(allocator);
    defer Parser.destroyOperations(operations, allocator);

    var operation_arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    defer operation_arena.deinit();
    const operation_allocator = operation_arena.allocator();

    const result = try graph.evalOperations(operation_allocator, operations);
    std.debug.print("got result: {}\n", .{result});
}

pub fn eval_Todo(edge: *const Edge, allocator: Allocator) error{OutOfMemory}!json.Value {
    _ = edge;
    _ = allocator;
    return json.Value{ .string = "Hello world!" };
}

pub fn eval(edge: *const Edge, allocator: Allocator) error{OutOfMemory}!json.Value {
    if (std.mem.eql(u8, edge.right.name(), "Todo")) {
        return eval_Todo(edge, allocator);
    }
    return .null;
}
