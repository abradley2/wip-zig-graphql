const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const Lexer = @import("./Lexer.zig");
const Token = @import("./Token.zig");
const TokenType = Token.TokenType;
const ast = @import("./ast.zig");

lexer: *Lexer,
current_token: Token,
peek_token: Token,

error_info: ErrorInfo = .{},

pub const ErrorInfo: type = struct {
    wanted: []const u8 = "",
};

const Parser = @This();

const ParserError: type = error{
    NotImplemented,
    OutOfMemory,
    UnexpectedToken,
    EmptyInputDefinition,
};

const Error = ParserError || Lexer.Error;

pub fn init(lexer: *Lexer) Error!Parser {
    return .{
        .lexer = lexer,
        .current_token = try lexer.nextToken(),
        .peek_token = try lexer.peekToken(),
    };
}

fn advance(parser: *Parser) Error!void {
    parser.lexer.read();

    parser.current_token = try parser.lexer.nextToken();
    parser.peek_token = try parser.lexer.peekToken();
}

fn positionToLocation(position: usize, input: []const u8) struct { usize, usize } {
    var line: usize = 1;
    var column: usize = 1;

    for (input, 0..) |c, idx| {
        if (idx == position) break;

        if (c == '\n') {
            line += 1;
            column = 1;
            continue;
        }
        column += 1;
    }

    return .{ line, column };
}

test "schema definition language kitchen sink" {
    const input = @embedFile("test_fixtures/sdl_kitchen_sink.graphql");
    var lexer: Lexer = .init(input);
    var parser: Parser = try .init(&lexer);

    const schema_document = parseSchemaDocument(&parser, std.testing.allocator) catch |err| {
        if (err == Error.UnexpectedToken) {
            const line, const column = positionToLocation(parser.lexer.position, input);
            std.debug.print("\nWanted: {s}\n", .{parser.error_info.wanted});
            std.debug.print("Got: {s}\n", .{parser.current_token.token_text});
            std.debug.print("At: line {d}, column {d}\n", .{ line, column });
        }

        return err;
    };

    defer {
        for (schema_document) |schema_declaration| {
            destroySchemaDeclaration(schema_declaration, std.testing.allocator);
        }
        std.testing.allocator.free(schema_document);
    }

    try std.testing.expectEqual(38, schema_document.len);
}

pub fn parseSchemaDocument(parser: *Parser, allocator: Allocator) Error![]ast.SchemaDeclaration {
    var schema_declarations: ArrayList(ast.SchemaDeclaration) = .empty;
    errdefer {
        for (schema_declarations.items) |schema_declaration| destroySchemaDeclaration(schema_declaration, allocator);
        schema_declarations.deinit(allocator);
    }

    while (parser.peek_token.token_type != .eof) {
        const schema_declaration = try parseSchemaDeclaration(parser, allocator);
        try schema_declarations.append(allocator, schema_declaration);
    }

    return try schema_declarations.toOwnedSlice(allocator);
}

test "parseSchemaDeclaration" {
    {
        const input =
            \\ "description of my type"
            \\ type MyType {
            \\   foo: String! 
            \\   bar: [Int]!
            \\   baz: Boolean
            \\ }
        ;

        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const schema_declaration = try parseSchemaDeclaration(&parser, std.testing.allocator);
        defer destroySchemaDeclaration(schema_declaration, std.testing.allocator);

        const description = schema_declaration.description orelse return error.UnexpectedNull;

        try std.testing.expectEqualStrings("\"description of my type\"", description);
    }
    {
        const input = "scalar MyScalar";
        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const schema_decl = try parseSchemaDeclaration(&parser, std.testing.allocator);

        const scalar_definition = switch (schema_decl.definition) {
            .scalar_definition => |v| v,
            else => return error.ExpectedGraphType,
        };

        try std.testing.expectEqualStrings("MyScalar", scalar_definition.name);
    }

    {
        const input =
            \\directive @deprecated(
            \\  reason: String = "No longer supported"
            \\) on FIELD_DEFINITION | ARGUMENT_DEFINITION | INPUT_FIELD_DEFINITION | ENUM_VALUE
        ;
        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const schema_decl = try parseSchemaDeclaration(&parser, std.testing.allocator);
        const directive_definition = switch (schema_decl.definition) {
            .directive_definition => |v| v,
            else => return error.ExpectedDirectiveDefinition,
        };
        defer destroyDirectiveDefinition(directive_definition, std.testing.allocator);

        const arguments = directive_definition.arguments orelse return error.UnexpectedNull;
        try std.testing.expectEqual(1, arguments.len);
        const default_value = arguments[0].default orelse return error.UnexpectedNull;
        const default_value_string = switch (default_value) {
            .string_type => |s| s,
            else => return error.UnexpectedValueType,
        };
        try std.testing.expectEqualStrings("No longer supported", default_value_string);
    }

    {
        const input =
            \\type MyType {
            \\  foo: String
            \\  bar: Int!
            \\}
        ;
        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const schema_declaration = try parseSchemaDeclaration(&parser, std.testing.allocator);
        const type_definition = switch (schema_declaration.definition) {
            .type_definition => |v| v,
            else => return error.ExpectedTypeDeclaration,
        };
        defer destroyTypeDefinition(type_definition, std.testing.allocator);

        try std.testing.expectEqualStrings("MyType", type_definition.name);

        const fields = type_definition.fields orelse return error.UnexpectedNull;

        try std.testing.expectEqual(2, fields.len);
        try std.testing.expectEqualStrings("foo", fields[0].name);
        try std.testing.expectEqualStrings("bar", fields[1].name);
    }

    {
        const input =
            \\type MyType implements Foo & Bar
            \\@foo
            \\@bar(fizz: "buzz")
            \\{ one: String }
        ;
        var lexer: Lexer = .init(input);
        const parser: Parser = try .init(&lexer);

        _ = parser;
    }
}

test "parseImplements" {
    {
        const input = "implements TypeOne & TypeTwo";
        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const implement_type_refs = (try parseImplements(&parser, std.testing.allocator)) orelse return error.UnexpectedNull;
        defer std.testing.allocator.free(implement_type_refs);

        try std.testing.expectEqual(2, implement_type_refs.len);
        try std.testing.expectEqualStrings("TypeOne", implement_type_refs[0]);
        try std.testing.expectEqualStrings("TypeTwo", implement_type_refs[1]);
    }

    {
        const input = "implements TypeOne";
        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const implement_type_refs = (try parseImplements(&parser, std.testing.allocator)) orelse return error.UnexpectedNull;
        defer std.testing.allocator.free(implement_type_refs);

        try std.testing.expectEqual(1, implement_type_refs.len);
        try std.testing.expectEqualStrings("TypeOne", implement_type_refs[0]);
    }
}

fn parseImplements(parser: *Parser, allocator: Allocator) Error!?[][]const u8 {
    if (parser.current_token.token_type == .keyword_implements) {
        try parser.advance();
    } else {
        return null;
    }

    var implement_type_refs: ArrayList([]const u8) = .empty;
    errdefer implement_type_refs.deinit(allocator);

    while (true) {
        const type_ref_name = switch (parser.current_token.token_type) {
            .identifier => parser.current_token.token_text,
            else => {
                parser.error_info.wanted = "named type ref to implement";
                return error.UnexpectedToken;
            },
        };

        try implement_type_refs.append(allocator, type_ref_name);

        try parser.advance();

        if (parser.current_token.token_type == .ampersand) {
            try parser.advance();
            continue;
        }

        break;
    }

    return try implement_type_refs.toOwnedSlice(allocator);
}

test "parseArgumentDefinitions" {
    {
        const input = "(subtaskId: String!, filterDone: Boolean = false, otherIds: [Int] = [1, 2, 3])";
        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const argument_definitions = (try parseArgumentDefinitions(&parser, std.testing.allocator)) orelse
            return error.UnexpectedNull;

        defer {
            for (argument_definitions) |argument_definition| {
                destroyArgumentDefinition(argument_definition, std.testing.allocator);
            }
            std.testing.allocator.free(argument_definitions);
        }

        try std.testing.expectEqual(3, argument_definitions.len);
    }
}

fn parseArgumentDefinitions(parser: *Parser, allocator: Allocator) Error!?[]ast.ArgumentDefinition {
    if (parser.current_token.token_type == .l_paren) {
        try parser.advance();
    } else {
        return null;
    }

    var argument_definitions: ArrayList(ast.ArgumentDefinition) = .empty;
    errdefer argument_definitions.deinit(allocator);

    while (true) {
        var description: ?[]const u8 = null;
        if (parser.current_token.token_type == .string) {
            description = parser.current_token.token_text;
            try parser.advance();
        }

        const argument_name = switch (parser.current_token.token_type) {
            .identifier => parser.current_token.token_text,
            else => {
                parser.error_info.wanted = "name for argument definition";
                return error.UnexpectedToken;
            },
        };

        try parser.advance();

        if (parser.current_token.token_type == .colon) {
            try parser.advance();
        } else {
            parser.error_info.wanted = "colon : following argument name";
            return error.UnexpectedToken;
        }

        const graphql_type = try parseGraphQlType(parser, allocator);
        errdefer destroyGraphQlType(graphql_type, allocator);

        var default_value: ?ast.Value = null;
        if (parser.current_token.token_type == .equals) {
            try parser.advance();

            default_value = try parseValue(parser, allocator);
        }

        const directives: ?[]ast.Directive = try parseDirectives(parser, allocator);
        errdefer if (directives) |d| destroyDirectives(d, allocator);

        try argument_definitions.append(allocator, ast.ArgumentDefinition{
            .description = description,
            .default = default_value,
            .name = argument_name,
            .graphql_type = graphql_type,
            .directives = directives,
        });

        if (parser.current_token.token_type == .r_paren) {
            try parser.advance();
            break;
        }

        if (parser.current_token.token_type == .comma) {
            try parser.advance();
            continue;
        }

        if (parser.lexer.newline_on_last_read) {
            continue;
        }

        parser.error_info.wanted = "Either an indent or a ',' comma followed by another argument, or a closing ) paren";
        return error.UnexpectedToken;
    }

    return try argument_definitions.toOwnedSlice(allocator);
}

fn parseIdentifier(parser: *Parser, wanted: []const u8) ![]const u8 {
    if (parser.current_token.token_type == .identifier) {
        const identifier = parser.current_token.token_text;
        try parser.advance();
        return identifier;
    }
    parser.error_info.wanted = wanted;
    return error.UnexpectedToken;
}

fn parseKeyword(parser: *Parser, keyword: TokenType) !?struct {} {
    if (parser.current_token.token_type == keyword) {
        try parser.advance();
        return .{};
    }
    return null;
}

test "parseTypeDefinition" {
    {
        const input =
            \\type MyType 
            \\  implements 
            \\    One &
            \\    Two 
            \\  @deprecated(reason: "old")
            \\  @example
            \\{
            \\  foo: String
            \\  bar: [Bar!]!
            \\}
        ;
        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const type_definition =
            try parseTypeDefinition(&parser, std.testing.allocator) orelse
            return error.UnexpectedNull;

        defer destroyTypeDefinition(type_definition, std.testing.allocator);
    }
}

fn parseSchemaDefinition(parser: *Parser, allocator: Allocator) Error!?ast.SchemaDefinition {
    _ = try parseKeyword(parser, .keyword_schema) orelse return null;

    const directives_opt = try parseDirectives(parser, allocator);
    errdefer if (directives_opt) |directives| destroyDirectives(directives, allocator);

    const fields_opt = try parseFields(parser, allocator);
    errdefer if (fields_opt) |fields| destroyFields(fields, allocator);

    return ast.SchemaDefinition{
        .directives = directives_opt,
        .fields = fields_opt,
    };
}

fn parseTypeDefinition(parser: *Parser, allocator: Allocator) Error!?ast.TypeDefinition {
    _ = try parseKeyword(parser, .keyword_type) orelse return null;

    const name = try parseIdentifier(parser, "name for type");

    const implements_opt = try parseImplements(parser, allocator);
    errdefer if (implements_opt) |implements| allocator.free(implements);

    const directives_opt = try parseDirectives(parser, allocator);
    errdefer if (directives_opt) |directives| destroyDirectives(directives, allocator);

    const fields_opt = try parseFields(parser, allocator);
    errdefer if (fields_opt) |fields| destroyFields(fields, allocator);

    return ast.TypeDefinition{
        .name = name,
        .implements = implements_opt,
        .directives = directives_opt,
        .fields = fields_opt,
    };
}

fn parseScalarDefinition(parser: *Parser, allocator: Allocator) Error!?ast.ScalarDefinition {
    _ = try parseKeyword(parser, .keyword_scalar) orelse return null;

    const name = try parseIdentifier(parser, "name for scalar");

    const directives_opt = try parseDirectives(parser, allocator);
    errdefer if (directives_opt) |directives| destroyDirectives(directives, allocator);

    return ast.ScalarDefinition{
        .name = name,
        .directives = directives_opt,
    };
}

fn parseInterfaceDefinition(parser: *Parser, allocator: Allocator) Error!?ast.InterfaceDefinition {
    _ = try parseKeyword(parser, .keyword_interface) orelse return null;

    const name = try parseIdentifier(parser, "name for interface");

    const implements_opt = try parseImplements(parser, allocator);

    const directives_opt = try parseDirectives(parser, allocator);
    errdefer if (directives_opt) |directives| destroyDirectives(directives, allocator);

    const fields_opt = try parseFields(parser, allocator);
    errdefer if (fields_opt) |fields| destroyFields(fields, allocator);

    return ast.InterfaceDefinition{
        .name = name,
        .implements = implements_opt,
        .directives = directives_opt,
        .fields = fields_opt,
    };
}

fn parseInputDefinition(parser: *Parser, allocator: Allocator) Error!?ast.InputDefinition {
    _ = try parseKeyword(parser, .keyword_input) orelse return null;

    const name = try parseIdentifier(parser, "name for input type");

    const directives_opt = try parseDirectives(parser, allocator);
    errdefer if (directives_opt) |directives| destroyDirectives(directives, allocator);

    const fields_opt = try parseFields(parser, allocator);
    errdefer if (fields_opt) |fields| destroyFields(fields, allocator);

    return ast.InputDefinition{
        .name = name,
        .directives = directives_opt,
        .fields = fields_opt,
    };
}

fn parseSchemaDeclaration(parser: *Parser, allocator: Allocator) Error!ast.SchemaDeclaration {
    var description: ?[]const u8 = null;
    if (parser.current_token.token_type == .string) {
        description = parser.current_token.token_text;
        try parser.advance();
    }

    var extends: bool = false;
    if (try parseKeyword(parser, .keyword_extend)) |_| {
        extends = true;
    }

    return ast.SchemaDeclaration{
        .description = description,
        .extends = extends,
        .definition = if (try parseSchemaDefinition(parser, allocator)) |schema_definition|
            .{ .schema_definition = schema_definition }
        else if (try parseInterfaceDefinition(parser, allocator)) |interface_definition|
            .{ .interface_definition = interface_definition }
        else if (try parseInputDefinition(parser, allocator)) |input_definition|
            .{ .input_definition = input_definition }
        else if (try parseTypeDefinition(parser, allocator)) |type_definition|
            .{ .type_definition = type_definition }
        else if (try parseScalarDefinition(parser, allocator)) |scalar_definition|
            .{ .scalar_definition = scalar_definition }
        else if (try parseDirectiveDefinition(parser, allocator)) |directive_definition|
            .{ .directive_definition = directive_definition }
        else if (try parseUnionDefinition(parser, allocator)) |union_definition|
            .{ .union_definition = union_definition }
        else if (try parseEnumDefinition(parser, allocator)) |enum_definition|
            .{ .enum_definition = enum_definition }
        else {
            parser.error_info.wanted = "type, schema, scalar, interface, input, enum, union, or directive definition";
            return error.UnexpectedToken;
        },
    };
}

test "parseEnumDefinition" {
    const input =
        \\ enum MyEnum @deprecated(reason: "old")
        \\ {
        \\   "description for one"
        \\   One @onEnum @deprecated(reason: "old")
        \\
        \\   """
        \\   description for two
        \\   """
        \\   Two @onEnum
        \\ }
    ;

    var lexer: Lexer = .init(input);
    var parser: Parser = try .init(&lexer);

    const enum_definition =
        try parseEnumDefinition(&parser, std.testing.allocator) orelse
        return error.UnexpectedNull;
    defer destroyEnumDefinition(enum_definition, std.testing.allocator);

    try std.testing.expectEqualStrings("MyEnum", enum_definition.name);
}

fn parseEnumDefinition(parser: *Parser, allocator: Allocator) Error!?ast.EnumDefinition {
    _ = try parseKeyword(parser, .keyword_enum) orelse return null;

    const name = try parseIdentifier(parser, "name for enum type");

    const directives_opt = try parseDirectives(parser, allocator);
    errdefer if (directives_opt) |directives| destroyDirectives(directives, allocator);

    const entries_opt = try parseEnumEntries(parser, allocator);
    errdefer if (entries_opt) |entries| destroyEnumEntries(entries, allocator);

    return ast.EnumDefinition{
        .directives = directives_opt,
        .entries = entries_opt,
        .name = name,
    };
}

fn parseEnumEntries(parser: *Parser, allocator: Allocator) Error!?[]ast.EnumEntryDefinition {
    _ = try parseKeyword(parser, .l_brace) orelse return null;

    var entries: ArrayList(ast.EnumEntryDefinition) = .empty;

    while (try parseKeyword(parser, .r_brace) == null) {
        var description: ?[]const u8 = null;
        if (parser.current_token.token_type == .string) {
            description = parser.current_token.token_text;
            try parser.advance();
        }

        const name = try parseIdentifier(parser, "name for enum entry");

        const directives_opt = try parseDirectives(parser, allocator);
        errdefer if (directives_opt) |directives| destroyDirectives(directives, allocator);

        try entries.append(allocator, ast.EnumEntryDefinition{
            .name = name,
            .directives = directives_opt,
            .description = description,
        });
    }

    return try entries.toOwnedSlice(allocator);
}

test "parseUnionDefinition" {
    {
        const input =
            \\ union MyUnion
            \\   = Foo 
            \\   | Bar 
        ;

        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const union_definition =
            try parseUnionDefinition(&parser, std.testing.allocator) orelse
            return error.UnexpectedNull;

        defer destroyUnionDefinition(union_definition, std.testing.allocator);

        try std.testing.expectEqualStrings("MyUnion", union_definition.name);
        try std.testing.expectEqual(null, union_definition.directives);

        const entries =
            union_definition.entries orelse
            return error.UnexpectedNull;

        try std.testing.expectEqual(2, entries.len);
        try std.testing.expectEqualStrings("Foo", entries[0]);
        try std.testing.expectEqualStrings("Bar", entries[1]);
    }
}

fn parseUnionDefinition(parser: *Parser, allocator: Allocator) Error!?ast.UnionDefinition {
    _ = try parseKeyword(parser, .keyword_union) orelse return null;

    const name = try parseIdentifier(parser, "name for union");

    const directives_opt = try parseDirectives(parser, allocator);
    errdefer if (directives_opt) |directives| destroyDirectives(directives, allocator);

    const entries: ?[][]const u8 = try parseUnionEntries(parser, allocator);

    if (try parseKeyword(parser, .equals)) |_| {}

    return ast.UnionDefinition{
        .directives = directives_opt,
        .entries = entries,
        .name = name,
    };
}

fn parseUnionEntries(parser: *Parser, allocator: Allocator) Error!?[][]const u8 {
    _ = try parseKeyword(parser, .equals) orelse return null;

    _ = try parseKeyword(parser, .pipe);

    var entries: ArrayList([]const u8) = .empty;
    errdefer entries.deinit(allocator);
    while (true) {
        const entry = try parseIdentifier(parser, "identifier for union entry");
        try entries.append(allocator, entry);

        if (try parseKeyword(parser, .pipe)) |_| continue;

        break;
    }

    return try entries.toOwnedSlice(allocator);
}

test "parseDirectiveDefinition" {
    {
        const input =
            \\directive @my_directive(
            \\  arg_one: Boolean
            \\  arg_two: String, arg_three: [Boolean!]
            \\) repeatable on FIELD_DEFINITION | 
            \\     ARGUMENT_DEFINITION | 
            \\     INPUT_FIELD_DEFINITION | ENUM_VALUE
        ;

        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const directive_definition =
            try parseDirectiveDefinition(&parser, std.testing.allocator) orelse
            return error.UnexpectedNull;

        defer destroyDirectiveDefinition(directive_definition, std.testing.allocator);

        const argument_definitions = directive_definition.arguments orelse return error.UnexpectedNull;

        try std.testing.expectEqual(3, argument_definitions.len);
        try std.testing.expectEqual(4, directive_definition.targets.len);
        try std.testing.expectEqualStrings("my_directive", directive_definition.name);
    }
}

fn parseDirectiveDefinition(
    parser: *Parser,
    allocator: Allocator,
) Error!?ast.DirectiveDefinition {
    _ = try parseKeyword(parser, .keyword_directive) orelse return null;

    _ = try parseKeyword(parser, .at_sign) orelse {
        parser.error_info.wanted = "an '@' symbol prior to the directive's name";
        return error.UnexpectedToken;
    };

    const name = try parseIdentifier(parser, "name for directive declaration");

    const argument_definitions: ?[]ast.ArgumentDefinition = try parseArgumentDefinitions(parser, allocator);
    errdefer if (argument_definitions) |defs| destroyArgumentDefinitions(defs, allocator);

    var repeatable: bool = false;
    if (try parseKeyword(parser, .keyword_repeatable)) |_| {
        repeatable = true;
    }

    return ast.DirectiveDefinition{
        .repeatable = repeatable,
        .name = name,
        .arguments = argument_definitions,
        .targets = try parseDirectiveTargets(parser, allocator),
    };
}

fn parseDirectiveTargets(parser: *Parser, allocator: Allocator) Error![]ast.DirectiveTarget {
    _ = try parseKeyword(parser, .keyword_on) orelse {
        parser.error_info.wanted = "expecting keyword 'on' followed by valid directive locations";
        return error.UnexpectedToken;
    };

    var directive_targets: ArrayList(ast.DirectiveTarget) = .empty;
    errdefer directive_targets.deinit(allocator);

    _ = try parseKeyword(parser, .pipe);

    while (true) {
        const directive_target_name = try parseIdentifier(parser, "Expecting identifier for graphql directive location");

        const directive_location = ast.DirectiveTarget.fromString(directive_target_name) orelse {
            parser.error_info.wanted = "Matching valid identifier for graphql directive location";
            return error.UnexpectedToken;
        };

        try directive_targets.append(allocator, directive_location);

        if (try parseKeyword(parser, .pipe)) |_| continue;

        break;
    }

    return try directive_targets.toOwnedSlice(allocator);
}

test "parseFields" {
    {
        const input =
            \\{
            \\ a: Int!
            \\ b: [String]
            \\}
        ;
        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const fields = (try parseFields(&parser, std.testing.allocator)) orelse
            return error.UnexpectedNull;

        defer destroyFields(fields, std.testing.allocator);

        try std.testing.expectEqual(2, fields.len);
        try std.testing.expectEqualStrings("a", fields[0].name);
        try std.testing.expectEqualStrings("b", fields[1].name);
    }
}

test "parseValue" {
    {
        var lexer: Lexer = .init("123.0");
        var parser: Parser = try .init(&lexer);

        const value = try parseValue(&parser, std.testing.allocator);

        const float_value = switch (value) {
            .float_type => |v| v,
            else => return error.ExpectedFloatValue,
        };

        const expected: f64 = 123.0;
        try std.testing.expectEqual(expected, float_value);
    }

    {
        var lexer: Lexer = .init("-123");
        var parser: Parser = try .init(&lexer);

        const value = try parseValue(&parser, std.testing.allocator);

        const int_value = switch (value) {
            .int_type => |v| v,
            else => return error.ExpectedIntValue,
        };

        const expected: i64 = -123;
        try std.testing.expectEqual(expected, int_value);
    }

    {
        var lexer: Lexer = .init("\"hello\"");
        var parser: Parser = try .init(&lexer);

        const value = try parseValue(&parser, std.testing.allocator);

        const string_value = switch (value) {
            .string_type => |v| v,
            else => return error.ExpectedStringValue,
        };

        const expected: []const u8 = "hello";
        try std.testing.expectEqualStrings(expected, string_value);
    }

    {
        var lexer: Lexer = .init("{ hello: \"World\" }");
        var parser: Parser = try .init(&lexer);

        const value = try parseValue(&parser, std.testing.allocator);

        const object_value = switch (value) {
            .object_type => |v| v,
            else => return error.ExpectedObjectValue,
        };

        try std.testing.expectEqual(1, object_value.len);

        std.testing.allocator.free(object_value);
    }

    {
        var lexer: Lexer = .init("[1, 2, 3]");
        var parser: Parser = try .init(&lexer);

        const value = try parseValue(&parser, std.testing.allocator);

        const list_value = switch (value) {
            .list_type => |v| v,
            else => return error.ExpectedListValue,
        };

        try std.testing.expectEqual(3, list_value.len);
        destroyValue(value, std.testing.allocator);
    }
}

fn parseValue(parser: *Parser, allocator: Allocator) Error!ast.Value {
    if (parser.current_token.token_type == .number) {
        if (std.mem.containsAtLeastScalar(u8, parser.current_token.token_text, 1, '.')) {
            const float_value = std.fmt.parseFloat(f64, parser.current_token.token_text) catch {
                parser.error_info.wanted = "floating point number";
                return error.UnexpectedToken;
            };

            try parser.advance();

            return ast.Value{
                .float_type = float_value,
            };
        }

        const int_value = std.fmt.parseInt(i64, parser.current_token.token_text, 10) catch {
            parser.error_info.wanted = "integer number";
            return error.UnexpectedToken;
        };

        try parser.advance();

        return ast.Value{
            .int_type = int_value,
        };
    }

    if (parser.current_token.token_type == .string) {
        const string_value = parser.current_token.token_text[1 .. parser.current_token.token_text.len - 1];

        try parser.advance();

        return ast.Value{
            .string_type = string_value,
        };
    }

    if (try parseListValue(parser, allocator)) |list_value| return ast.Value{ .list_type = list_value };

    if (try parseValuePairs(parser, allocator)) |value_pairs| return ast.Value{ .object_type = value_pairs };

    if (try parseKeyword(parser, .keyword_true)) |_| return .{ .boolean_type = true };

    if (try parseKeyword(parser, .keyword_false)) |_| return .{ .boolean_type = false };

    if (try parseKeyword(parser, .keyword_null)) |_| return .null_type;

    parser.error_info.wanted = "float, bool, null, string, list or object value";
    return error.UnexpectedToken;
}

fn parseListValue(parser: *Parser, allocator: Allocator) Error!?[]ast.Value {
    _ = try parseKeyword(parser, .l_bracket) orelse return null;

    var values: ArrayList(ast.Value) = .empty;
    errdefer values.deinit(allocator);

    while (parser.current_token.token_type != .r_bracket) {
        const value = try parseValue(parser, allocator);
        errdefer destroyValue(value, allocator);

        try values.append(allocator, value);

        if (try parseKeyword(parser, .comma)) |_| continue;

        if (try parseKeyword(parser, .r_bracket)) |_| break;

        parser.error_info.wanted = "expected either comma ',' followed by another item, or ending bracket ]";
        return error.UnexpectedToken;
    }

    return try values.toOwnedSlice(allocator);
}

fn parseValuePairs(parser: *Parser, allocator: Allocator) Error!?[]ast.ValuePair {
    _ = try parseKeyword(parser, .l_brace) orelse return null;

    var pairs: ArrayList(ast.ValuePair) = .empty;
    errdefer pairs.deinit(allocator);

    while (parser.current_token.token_type != .r_brace) {
        const key_identifier, const value = try parseValuePair(parser, allocator);

        try pairs.append(allocator, .{ .key = key_identifier, .value = value });

        if (try parseKeyword(parser, .comma)) |_| continue;

        if (try parseKeyword(parser, .r_brace)) |_| break;

        parser.error_info.wanted = "either comma , followed by another pair, or ending } brace";
        return error.UnexpectedToken;
    }

    return try pairs.toOwnedSlice(allocator);
}

fn parseValuePair(parser: *Parser, allocator: Allocator) Error!struct { []const u8, ast.Value } {
    const key_identifier = switch (parser.current_token.token_type) {
        .identifier => parser.current_token.token_text,
        else => {
            parser.error_info.wanted = "identifier key for object key/value pair";
            return error.UnexpectedToken;
        },
    };

    try parser.advance();

    if (parser.current_token.token_type == .colon) {
        try parser.advance();
    } else {
        parser.error_info.wanted = "colon seperator for object key/value pair";
        return error.UnexpectedToken;
    }

    const value = try parseValue(parser, allocator);

    return .{ key_identifier, value };
}

test "parseField" {
    {
        const input = "hello: [World]";

        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const field = try parser.parseField(std.testing.allocator);

        defer destroyField(field, std.testing.allocator);

        try std.testing.expectEqual(true, field.graphql_type.is_list);
        const child = field.graphql_type.child orelse return error.UnexpectedNull;

        const child_name = child.named_type orelse return error.UnexpectedNull;
        try std.testing.expectEqualStrings("World", child_name);
    }

    {
        const input = "hello(argA: String): [World]";

        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const field = try parser.parseField(std.testing.allocator);
        defer destroyField(field, std.testing.allocator);

        try std.testing.expectEqual(true, field.graphql_type.is_list);
        const child = field.graphql_type.child orelse return error.UnexpectedNull;

        const child_name = child.named_type orelse return error.UnexpectedNull;
        try std.testing.expectEqualStrings("World", child_name);
    }

    {
        const input =
            \\hello(argA: String): [World]
            \\@directive_one(foo: "bar")
            \\@directive_two
        ;

        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const field = try parser.parseField(std.testing.allocator);
        defer destroyField(field, std.testing.allocator);

        try std.testing.expectEqual(true, field.graphql_type.is_list);
        const child = field.graphql_type.child orelse return error.UnexpectedNull;

        const child_name = child.named_type orelse return error.UnexpectedNull;
        try std.testing.expectEqualStrings("World", child_name);

        const directives = field.directives orelse return error.UnexpectedNull;
        try std.testing.expectEqual(2, directives.len);
    }
}

fn parseFields(parser: *Parser, allocator: Allocator) Error!?[]ast.Field {
    if (parser.current_token.token_type == .l_brace) {
        try parser.advance();
    } else {
        return null;
    }

    var fields: ArrayList(ast.Field) = .empty;
    errdefer fields.deinit(allocator);

    while (parser.current_token.token_type != .r_brace) {
        const field = try parseField(parser, allocator);

        try fields.append(allocator, field);
    }

    try parser.advance();

    if (fields.items.len == 0) {
        parser.error_info.wanted = "at least one field within the field selection braces";
        return error.UnexpectedToken;
    }

    return try fields.toOwnedSlice(allocator);
}

fn parseField(parser: *Parser, allocator: Allocator) Error!ast.Field {
    var description: ?[]const u8 = null;
    if (parser.current_token.token_type == .string) {
        description = parser.current_token.token_text;
        try parser.advance();
    }

    const field_name = switch (parser.current_token.token_type) {
        .identifier => parser.current_token.token_text,
        .keyword_mutation => parser.current_token.token_text,
        .keyword_subscription => parser.current_token.token_text,
        .keyword_query => parser.current_token.token_text,
        else => {
            parser.error_info.wanted = "field name identifier";
            return error.UnexpectedToken;
        },
    };

    try parser.advance();

    const argument_definitions: ?[]ast.ArgumentDefinition = try parseArgumentDefinitions(parser, allocator);
    errdefer if (argument_definitions) |defs| destroyArgumentDefinitions(defs, allocator);

    if (parser.current_token.token_type == .colon) {
        try parser.advance();
    } else {
        parser.error_info.wanted = "colon following field name identifier";
        return error.UnexpectedToken;
    }

    const graphql_type = try parseGraphQlType(parser, allocator);
    errdefer destroyGraphQlType(graphql_type, allocator);

    var default_value_opt: ?ast.Value = null;
    errdefer if (default_value_opt) |value| destroyValue(value, allocator);
    if (parser.current_token.token_type == .equals) {
        try parser.advance();
        default_value_opt = try parseValue(parser, allocator);
    }

    const directives: ?[]ast.Directive = try parseDirectives(parser, allocator);

    return ast.Field{
        .description = description,
        .name = field_name,
        .graphql_type = graphql_type,
        .arguments = argument_definitions,
        .directives = directives,
        .default_value = default_value_opt,
    };
}

test "parseDirective" {
    {
        const input =
            \\ @my_directive(one: 33)
        ;

        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const directive = (try parseDirective(&parser, std.testing.allocator)) orelse
            return error.UnexpectedNull;

        defer destroyDirective(directive, std.testing.allocator);

        try std.testing.expectEqualStrings("my_directive", directive.name);
        const arguments = directive.arguments orelse return error.ExpectedNotNull;

        try std.testing.expectEqual(1, arguments.len);
        try std.testing.expectEqualStrings("one", arguments[0].key);
        try std.testing.expectEqual(@as(i64, 33), arguments[0].value.int_type);
    }

    {
        const input = "@directive_no_arguments";
        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const directive = (try parseDirective(&parser, std.testing.allocator)) orelse
            return error.UnexpectedNull;

        try std.testing.expectEqualStrings("directive_no_arguments", directive.name);
    }
}

fn parseDirectives(parser: *Parser, allocator: Allocator) Error!?[]ast.Directive {
    if (parser.current_token.token_type != .at_sign) {
        return null;
    }

    var directives: ArrayList(ast.Directive) = .empty;
    errdefer directives.deinit(allocator);

    while (try parseDirective(parser, allocator)) |directive| {
        try directives.append(allocator, directive);
    }

    return try directives.toOwnedSlice(allocator);
}

fn parseDirective(parser: *Parser, allocator: Allocator) Error!?ast.Directive {
    if (parser.current_token.token_type == .at_sign) {
        try parser.advance();
    } else {
        return null;
    }

    const directive_ident = switch (parser.current_token.token_type) {
        .identifier => parser.current_token.token_text,
        else => {
            parser.error_info.wanted = "identifier for directive following the @ symbol";
            return error.UnexpectedToken;
        },
    };

    try parser.advance();

    var arguments: ?[]ast.ValuePair = null;
    var arguments_list: ArrayList(ast.ValuePair) = .empty;
    errdefer arguments_list.deinit(allocator);

    if (parser.current_token.token_type == .l_paren) {
        try parser.advance();

        while (true) {
            const value_ident, const value = try parseValuePair(parser, allocator);
            try arguments_list.append(allocator, ast.ValuePair{
                .key = value_ident,
                .value = value,
            });

            if (parser.current_token.token_type == .comma) {
                try parser.advance();
                continue;
            }

            if (parser.current_token.token_type == .r_paren) {
                try parser.advance();
                break;
            }

            parser.error_info.wanted = "Either a , before the next item or a ) to close the argument list";
            return error.UnexpectedToken;
        }

        arguments = try arguments_list.toOwnedSlice(allocator);
    }

    return ast.Directive{
        .arguments = arguments,
        .name = directive_ident,
    };
}

fn parseGraphQlType(parser: *Parser, allocator: Allocator) Error!ast.GraphQlType {
    if (parser.current_token.token_type == .l_bracket) {
        try parser.advance();

        const child = try allocator.create(ast.GraphQlType);
        child.* = try parseGraphQlType(parser, allocator);
        errdefer allocator.destroy(child);

        if (parser.current_token.token_type == .r_bracket) {
            try parser.advance();
        } else {
            parser.error_info.wanted = "closing delimiter for [";
            return error.UnexpectedToken;
        }

        var nullable = true;
        if (parser.current_token.token_type == .ex_mark) {
            nullable = false;
            try parser.advance();
        }

        return ast.GraphQlType{
            .child = child,
            .is_list = true,
            .is_nullable = nullable,
            .named_type = null,
        };
    }

    const named_type = switch (parser.current_token.token_type) {
        .identifier => parser.current_token.token_text,
        else => {
            parser.error_info.wanted = "identifier for field type";
            return error.UnexpectedToken;
        },
    };

    try parser.advance();

    var nullable = true;

    if (parser.current_token.token_type == .ex_mark) {
        nullable = false;
        try parser.advance();
    }

    return ast.GraphQlType{
        .child = null,
        .is_list = false,
        .is_nullable = nullable,
        .named_type = named_type,
    };
}

fn destroyDirectives(directives: []ast.Directive, allocator: Allocator) void {
    for (directives) |directive| destroyDirective(directive, allocator);
    allocator.free(directives);
}

fn destroyDirective(directive: ast.Directive, allocator: Allocator) void {
    if (directive.arguments) |arguments| {
        for (arguments) |argument| destroyValue(argument.value, allocator);
        allocator.free(arguments);
    }
}

fn destroyGraphQlType(graphQlType: ast.GraphQlType, allocator: Allocator) void {
    if (graphQlType.child) |child| {
        destroyGraphQlType(child.*, allocator);
        allocator.destroy(child);
    }
}

fn destroyValue(value: ast.Value, allocator: Allocator) void {
    switch (value) {
        .list_type => |list| {
            for (list) |sub_value| destroyValue(sub_value, allocator);
            allocator.free(list);
        },
        .object_type => |obj| {
            for (obj) |pair| destroyValue(pair.value, allocator);
            allocator.free(obj);
        },
        else => {},
    }
}

fn destroyArgumentDefinitions(argument_definitions: []ast.ArgumentDefinition, allocator: Allocator) void {
    for (argument_definitions) |argument_definition| destroyArgumentDefinition(argument_definition, allocator);
    allocator.free(argument_definitions);
}

fn destroyArgumentDefinition(argument_definition: ast.ArgumentDefinition, allocator: Allocator) void {
    if (argument_definition.default) |default_value| destroyValue(default_value, allocator);
    if (argument_definition.directives) |directives| destroyDirectives(directives, allocator);
    destroyGraphQlType(argument_definition.graphql_type, allocator);
}

fn destroyFields(fields: []ast.Field, allocator: Allocator) void {
    for (fields) |field| destroyField(field, allocator);
    allocator.free(fields);
}

fn destroyField(field: ast.Field, allocator: Allocator) void {
    if (field.arguments) |argument_definitions| destroyArgumentDefinitions(argument_definitions, allocator);
    if (field.directives) |directives| destroyDirectives(directives, allocator);
    destroyGraphQlType(field.graphql_type, allocator);
}

fn destroyDirectiveDefinition(directive_definition: ast.DirectiveDefinition, allocator: Allocator) void {
    if (directive_definition.arguments) |argument_definitions| destroyArgumentDefinitions(argument_definitions, allocator);
    allocator.free(directive_definition.targets);
}

fn destroyTypeDefinition(type_definition: ast.TypeDefinition, allocator: Allocator) void {
    if (type_definition.directives) |directives| destroyDirectives(directives, allocator);
    if (type_definition.fields) |fields| destroyFields(fields, allocator);
    if (type_definition.implements) |implements| allocator.free(implements);
}

fn destroyUnionDefinition(union_definition: ast.UnionDefinition, allocator: Allocator) void {
    if (union_definition.directives) |directives| destroyDirectives(directives, allocator);
    if (union_definition.entries) |entries| allocator.free(entries);
}

fn destroyEnumEntry(enum_entry: ast.EnumEntryDefinition, allocator: Allocator) void {
    if (enum_entry.directives) |directives| destroyDirectives(directives, allocator);
}

fn destroyEnumEntries(enum_entries: []ast.EnumEntryDefinition, allocator: Allocator) void {
    for (enum_entries) |entry| destroyEnumEntry(entry, allocator);
    allocator.free(enum_entries);
}

fn destroyEnumDefinition(enum_definition: ast.EnumDefinition, allocator: Allocator) void {
    if (enum_definition.directives) |directives| destroyDirectives(directives, allocator);
    if (enum_definition.entries) |entries| destroyEnumEntries(entries, allocator);
}

fn destroyInputDefinition(input_definition: ast.InputDefinition, allocator: Allocator) void {
    if (input_definition.directives) |directives| destroyDirectives(directives, allocator);
    if (input_definition.fields) |fields| destroyFields(fields, allocator);
}

fn destroySchemaDefinition(schema_definition: ast.SchemaDefinition, allocator: Allocator) void {
    if (schema_definition.directives) |directives| destroyDirectives(directives, allocator);
    if (schema_definition.fields) |fields| destroyFields(fields, allocator);
}

fn destroyInterfaceDefinition(interface_definition: ast.InterfaceDefinition, allocator: Allocator) void {
    if (interface_definition.directives) |directives| destroyDirectives(directives, allocator);
    if (interface_definition.fields) |fields| destroyFields(fields, allocator);
    if (interface_definition.implements) |implements| allocator.free(implements);
}

fn destroyScalarDefinition(scalar_definition: ast.ScalarDefinition, allocator: Allocator) void {
    if (scalar_definition.directives) |directives| destroyDirectives(directives, allocator);
}

fn destroySchemaDeclaration(schema_declaration: ast.SchemaDeclaration, allocator: Allocator) void {
    switch (schema_declaration.definition) {
        .directive_definition => |definition| destroyDirectiveDefinition(definition, allocator),
        .enum_definition => |definition| destroyEnumDefinition(definition, allocator),
        .input_definition => |definition| destroyInputDefinition(definition, allocator),
        .schema_definition => |definition| destroySchemaDefinition(definition, allocator),
        .interface_definition => |definition| destroyInterfaceDefinition(definition, allocator),
        .type_definition => |definition| destroyTypeDefinition(definition, allocator),
        .union_definition => |definition| destroyUnionDefinition(definition, allocator),
        .scalar_definition => |definition| destroyScalarDefinition(definition, allocator),
    }
}
