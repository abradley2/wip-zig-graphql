const std = @import("std");
const graphql = @import("graphql");

const Lexer = graphql.Lexer;
const Parser = graphql.Parser;

test "all tests" {
    std.testing.refAllDecls(@This());
}

pub fn main() !void {
    const input = @embedFile("test_fixtures/sdl_kitchen_sink.graphql");
    var lexer: Lexer = .init(input);
    var parser: Parser = try .init(&lexer);

    var arena_allocator: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    defer arena_allocator.deinit();
    const allocator = arena_allocator.allocator();

    _ = try parser.parseSchemaDocument(allocator);
}
