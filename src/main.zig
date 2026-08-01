const std = @import("std");
const builtin = @import("builtin");
const graphql = @import("graphql");
const json = std.json;

const Allocator = std.mem.Allocator;
const Lexer = graphql.Lexer;
const SchemaParser = graphql.SchemaParser;

test "all tests" {
    std.testing.refAllDecls(@This());
}

pub fn main(init: std.process.Init) !void {
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
        \\ 
        \\schema {
        \\  query: Query
        \\}
    ;

    var lexer: Lexer = .init(input);
    var parser: SchemaParser = try .init(&lexer);

    const schema_document = try parser.parseSchemaDocument(init.gpa);

    SchemaParser.destroySchemaDocument(schema_document, init.gpa);
}
