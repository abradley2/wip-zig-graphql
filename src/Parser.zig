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

fn parseSchemaDocument(parser: *Parser, allocator: Allocator) Error![]ast.SchemaDocument {
    var schema_declarations: ArrayList(ast.SchemaDeclaration) = .empty;

    while (parser.peek_token.token_type != .eof) {
        const schema_declaration = try parseSchemaDeclaration(parser, allocator);
        try schema_declarations.append(allocator, schema_declaration);
    }

    return try schema_declarations.toOwnedSlice(allocator);
}

test "Parse Schema Declaration" {
    {
        const input = "scalar MyScalar";
        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const schema_decl = (parseSchemaDeclaration(&parser, std.testing.allocator) catch |err| {
            if (err == Parser.ParserError.UnexpectedToken) {
                std.debug.print("Parser error, wanted: {s}\n", .{parser.error_info.wanted});
                std.debug.print("But got: {s}\n", .{parser.current_token.token_text});
            }
            return err;
        });

        const graph_type = switch (schema_decl) {
            .type_delcaration => |v| v,
            else => return error.ExpectedGraphType,
        };

        const is_scalar = switch (graph_type.graphql_type) {
            .scalar_type => true,
            else => false,
        };

        try std.testing.expectEqual(true, is_scalar);
        try std.testing.expectEqualStrings("MyScalar", graph_type.type_ref);
    }
}

test "Parse Implements" {
    {
        const input = "TypeOne & TypeTwo";
        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const implement_type_refs = try parseImplements(&parser, std.testing.allocator);
        defer std.testing.allocator.free(implement_type_refs);

        try std.testing.expectEqual(2, implement_type_refs.len);
        try std.testing.expectEqualStrings("TypeOne", implement_type_refs[0]);
        try std.testing.expectEqualStrings("TypeTwo", implement_type_refs[1]);
    }

    {
        const input = "TypeOne";
        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const implement_type_refs = try parseImplements(&parser, std.testing.allocator);
        defer std.testing.allocator.free(implement_type_refs);

        try std.testing.expectEqual(1, implement_type_refs.len);
        try std.testing.expectEqualStrings("TypeOne", implement_type_refs[0]);
    }
}

fn parseImplements(parser: *Parser, allocator: Allocator) Error![]ast.NamedTypeRef {
    var implement_type_refs: ArrayList(ast.NamedTypeRef) = .empty;
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

test "Parse Argument Definitions" {
    {
        const input = "subtaskId: String!, filterDone: Boolean = false, otherIds: [Int] = [1, 2, 3]";
        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const argument_definitions = try parseArgumentDefinitions(&parser, std.testing.allocator);
        defer {
            for (argument_definitions) |argument_definition| {
                destroyArgumentDefinition(argument_definition, std.testing.allocator);
            }
            std.testing.allocator.free(argument_definitions);
        }

        try std.testing.expectEqual(3, argument_definitions.len);
    }
}

pub fn destroyArgumentDefinition(argument_definition: ast.ArgumentDefinition, allocator: Allocator) void {
    if (argument_definition.default) |value| {
        destroyValue(value, allocator);
    }
    destroyNamedType(argument_definition.named_type, allocator);
}

fn parseArgumentDefinitions(parser: *Parser, allocator: Allocator) Error![]ast.ArgumentDefinition {
    var argument_definitions: ArrayList(ast.ArgumentDefinition) = .empty;
    errdefer argument_definitions.deinit(allocator);

    while (true) {
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

        const named_type = try parseNamedType(parser, allocator);
        errdefer destroyNamedType(named_type, allocator);

        var default_value: ?ast.Value = null;
        if (parser.current_token.token_type == .equals) {
            try parser.advance();

            default_value = try parseValue(parser, allocator);
        }

        try argument_definitions.append(allocator, ast.ArgumentDefinition{
            .default = default_value,
            .name = argument_name,
            .named_type = named_type,
        });

        if (parser.current_token.token_type == .comma) {
            try parser.advance();
            continue;
        }

        break;
    }

    return try argument_definitions.toOwnedSlice(allocator);
}

fn parseSchemaDeclaration(parser: *Parser, allocator: Allocator) Error!ast.SchemaDeclaration {
    const declaration_token = parser.current_token;

    return switch (declaration_token.token_type) {
        .keyword_type, .keyword_input, .keyword_interface => {
            try parser.advance();

            var object_kind: ast.ObjectKind = .default_type;
            if (declaration_token.token_type == .keyword_input) object_kind = .input_type;
            if (declaration_token.token_type == .keyword_interface) object_kind = .interface_type;

            var implements: ?[]ast.NamedTypeRef = null;
            if (object_kind == .default_type and parser.peek_token.token_type == .keyword_implements) {
                implements = try parseImplements(parser, allocator);
            }

            const object = try parseObject(parser, allocator, object_kind);
            _ = object;
            return error.NotImplemented;
        },
        .keyword_scalar => {
            try parser.advance();
            const identifier = switch (parser.current_token.token_type) {
                .identifier => parser.current_token.token_text,
                else => {
                    parser.error_info.wanted = "identifier for scalar";
                    return error.UnexpectedToken;
                },
            };

            return ast.SchemaDeclaration{
                .type_delcaration = ast.TypeDeclaration{
                    .type_ref = identifier,
                    .graphql_type = .scalar_type,
                    .implements = &[_]ast.NamedTypeRef{},
                    .directives = &[_]ast.Directive{},
                },
            };
        },
        .keyword_enum => {
            try parser.advance();
            return error.NotImplemented;
        },
        .keyword_directive => {
            try parser.advance();
            return error.NotImplemented;
        },
        else => {
            parser.error_info.wanted = "type, input, interface, scalar, enum, or directive keyword";
            return error.UnexpectedToken;
        },
    };
}

test "Parse Object" {
    {
        const input =
            \\{
            \\ a: Int!
            \\ b: [String]
            \\}
        ;
        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const object = parseObject(&parser, std.testing.allocator, .default_type) catch |err| {
            if (err == error.UnexpectedToken) {
                std.debug.print("Wanted: {s}\n", .{parser.error_info.wanted});
                std.debug.print("Got: {s}\n", .{parser.current_token.token_text});
                std.debug.print("At: {d}\n", .{parser.lexer.position});
            }
            return err;
        };

        try std.testing.expectEqual(2, object.fields.len);
        try std.testing.expectEqualStrings("a", object.fields[0].name);
        try std.testing.expectEqualStrings("b", object.fields[1].name);

        for (object.fields) |field| {
            if (field.field_type.child) |c| {
                std.testing.allocator.destroy(c);
            }
        }

        std.testing.allocator.free(object.fields);
    }
}

fn parseObject(parser: *Parser, allocator: Allocator, object_kind: ast.ObjectKind) Error!ast.Object {
    if (parser.current_token.token_type == .l_brace) {
        try parser.advance();
    } else {
        parser.error_info.wanted = "{ for object selection";
        return error.UnexpectedToken;
    }

    var fields: ArrayList(ast.Field) = .empty;
    errdefer {
        if (fields.items.len > 0) fields.deinit(allocator);
    }

    while (parser.current_token.token_type != .r_brace) {
        if (parser.current_token.token_type == .eof) {
            parser.error_info.wanted = "either next field for object or ending } brace";
            return error.UnexpectedToken;
        }

        try fields.append(allocator, try parseField(parser, allocator));
    }

    try parser.advance();

    return ast.Object{
        .fields = try fields.toOwnedSlice(allocator),
        .kind = object_kind,
    };
}

test "Parse Value" {
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

fn destroyValue(value: ast.Value, allocator: Allocator) void {
    switch (value) {
        .list_type => |list| {
            for (list) |sub_value| {
                destroyValue(sub_value, allocator);
            }
            allocator.free(list);
        },
        .object_type => |obj| {
            for (obj) |pair| {
                destroyValue(pair.value, allocator);
            }
            allocator.free(obj);
        },
        else => {},
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

    if (parser.current_token.token_type == .l_bracket) {
        try parser.advance();

        var values: ArrayList(ast.Value) = .empty;
        errdefer values.deinit(allocator);

        while (parser.current_token.token_type != .r_bracket) {
            const value = try parseValue(parser, allocator);
            errdefer destroyValue(value, allocator);

            try values.append(allocator, value);

            if (parser.current_token.token_type == .comma) {
                try parser.advance();
                continue;
            }

            if (parser.current_token.token_type == .r_bracket) {
                try parser.advance();
                break;
            }

            parser.error_info.wanted = "expected either comma ',' followed by another item, or ending bracket ]";
            return error.UnexpectedToken;
        }

        return ast.Value{
            .list_type = try values.toOwnedSlice(allocator),
        };
    }

    if (parser.current_token.token_type == .l_brace) {
        try parser.advance();

        var pairs: ArrayList(ast.ValuePair) = .empty;

        while (parser.current_token.token_type != .r_brace) {
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

            try pairs.append(allocator, .{ .key = key_identifier, .value = value });

            if (parser.current_token.token_type == .comma) {
                try parser.advance();
                continue;
            }

            if (parser.current_token.token_type == .r_brace) {
                try parser.advance();
                break;
            }

            parser.error_info.wanted = "either comma , followed by another pair, or ending } brace";
            return error.UnexpectedToken;
        }

        return ast.Value{
            .object_type = try pairs.toOwnedSlice(allocator),
        };
    }

    if (parser.current_token.token_type == .keyword_true) {
        try parser.advance();

        return ast.Value{
            .boolean_type = true,
        };
    }

    if (parser.current_token.token_type == .keyword_false) {
        try parser.advance();

        return ast.Value{
            .boolean_type = false,
        };
    }

    if (parser.current_token.token_type == .keyword_null) {
        try parser.advance();

        return .null_type;
    }

    parser.error_info.wanted = "float, bool, null, string, list or object value";
    return error.UnexpectedToken;
}

test "Parse Field" {
    {
        const input = "hello(argA: String): [World]";

        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const field = parser.parseField(std.testing.allocator) catch |err| {
            if (err == Parser.ParserError.UnexpectedToken) {
                std.debug.print("Error: wanted {s}\n", .{parser.error_info.wanted});
                std.debug.print("Got: {s}\n", .{parser.current_token.token_text});
                std.debug.print("At: {d}\n", .{parser.lexer.read_position});
            }
            return err;
        };

        defer std.testing.allocator.free(field.arguments);

        defer for (field.arguments) |argument_definition| {
            destroyArgumentDefinition(argument_definition, std.testing.allocator);
        };

        try std.testing.expectEqual(true, field.field_type.is_list);
        const child = field.field_type.child orelse return error.ExpectedChild;
        defer std.testing.allocator.destroy(child);

        const child_name = child.type_ref orelse return error.ExpectedNamedChild;
        try std.testing.expectEqualStrings("World", child_name);
    }
}

fn parseField(parser: *Parser, allocator: Allocator) Error!ast.Field {
    const field_name = switch (parser.current_token.token_type) {
        .identifier => parser.current_token.token_text,
        else => {
            parser.error_info.wanted = "field name identifier";
            return error.UnexpectedToken;
        },
    };

    try parser.advance();

    var argument_definitions: []ast.ArgumentDefinition = undefined;
    errdefer {
        for (argument_definitions) |argument_definition| {
            destroyArgumentDefinition(argument_definition, allocator);
        }
        if (argument_definitions.len > 0) allocator.free(argument_definitions);
    }

    if (parser.current_token.token_type == .l_paren) {
        try parser.advance();
        // handle arguments
        argument_definitions = try parseArgumentDefinitions(parser, allocator);

        if (parser.current_token.token_type == .r_paren) {
            try parser.advance();
        } else {
            parser.error_info.wanted = "Closing ) for field arguments";
            return error.UnexpectedToken;
        }
    }

    if (parser.current_token.token_type == .colon) {
        try parser.advance();
    } else {
        parser.error_info.wanted = "colon following field name identifier";
        return error.UnexpectedToken;
    }

    const named_type = try parseNamedType(parser, allocator);

    while (parser.peek_token.token_type == .at_sign) {
        // handle directives
    }

    return ast.Field{
        .name = field_name,
        .field_type = named_type,
        .arguments = argument_definitions,
        .directives = &[_]ast.Directive{},
    };
}

fn parseNamedType(parser: *Parser, allocator: Allocator) Error!ast.NamedType {
    if (parser.current_token.token_type == .l_bracket) {
        try parser.advance();

        const child = try allocator.create(ast.NamedType);
        child.* = try parseNamedType(parser, allocator);
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

        return ast.NamedType{
            .child = child,
            .is_list = true,
            .is_nullable = nullable,
            .type_ref = null,
        };
    }

    const type_ref = switch (parser.current_token.token_type) {
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

    return ast.NamedType{
        .child = null,
        .is_list = false,
        .is_nullable = nullable,
        .type_ref = type_ref,
    };
}

pub fn destroyNamedType(named_type: ast.NamedType, allocator: Allocator) void {
    if (named_type.child) |child| {
        destroyNamedType(child.*, allocator);
        allocator.destroy(child);
    }
}
