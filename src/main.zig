const std = @import("std");
const builtin = @import("builtin");
const graphql = @import("graphql");

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
    const input = @embedFile("test_fixtures/sdl_kitchen_sink.graphql");
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
    try Graph.init(graph, allocator, schema_document);

    std.debug.print("Done!\n", .{});

    const query = [_]Edge{
        Edge{
            .left = "Query",
            .name = "todos",
            .right = GraphQlType{
                .is_list = true,
                .child = &GraphQlType{
                    .named_type = "Todo",
                },
            },
        },
        Edge{
            .left = "Todo",
            .name = "sub_task",
            .right = GraphQlType{
                .child = &GraphQlType{
                    .named_type = "Todo",
                },
            },
        },
    };

    _ = query;
}
