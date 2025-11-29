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

fn parseSchemaDocument(parser: *Parser, allocator: Allocator) Error![]ast.SchemaDocument {
    var schema_declarations: ArrayList(ast.SchemaDeclaration) = .empty;

    while (parser.peek_token.token_type != .eof) {
        const schema_declaration = try parseSchemaDeclaration(parser, allocator);
        try schema_declarations.append(allocator, schema_declaration);
    }

    return try schema_declarations.toOwnedSlice(allocator);
}

test "parseSchemaDeclaration" {
    {
        const input = "scalar MyScalar";
        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const schema_decl = try parseSchemaDeclaration(&parser, std.testing.allocator);

        const type_declaration = switch (schema_decl) {
            .type_declaration => |v| v,
            else => return error.ExpectedGraphType,
        };

        const is_scalar = switch (type_declaration.definition) {
            .scalar_definition => true,
            else => false,
        };

        try std.testing.expectEqual(true, is_scalar);
        try std.testing.expectEqualStrings("MyScalar", type_declaration.name);
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
        const directive_declaration = switch (schema_decl) {
            .directive_declaration => |v| v,
            else => return error.ExpectedDirectiveDeclaration,
        };
        defer destroyDirectiveDeclaration(directive_declaration, std.testing.allocator);

        const arguments = directive_declaration.arguments orelse return error.UnexpectedNull;
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
        const type_declaration = switch (schema_declaration) {
            .type_declaration => |v| v,
            else => return error.ExpectedTypeDeclaration,
        };
        defer destroyTypeDeclaration(type_declaration, std.testing.allocator);

        try std.testing.expectEqualStrings("MyType", type_declaration.name);

        const fields = switch (type_declaration.definition) {
            .type_definition => |v| v orelse return error.UnexpectedNull,
            else => return error.ExpectedObject,
        };

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

fn parseImplements(parser: *Parser, allocator: Allocator) Error!?[]ast.NamedType {
    if (parser.current_token.token_type == .keyword_implements) {
        try parser.advance();
    } else {
        return null;
    }

    var implement_type_refs: ArrayList(ast.NamedType) = .empty;
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

fn parseSchemaDeclaration(parser: *Parser, allocator: Allocator) Error!ast.SchemaDeclaration {
    var description: ?[]const u8 = null;
    if (parser.current_token.token_type == .string) {
        description = parser.current_token.token_text;
        try parser.advance();
    }

    var declaration_token = parser.current_token;

    var extends: bool = false;
    if (declaration_token.token_type == .keyword_extend) {
        extends = true;
        try parser.advance();
        declaration_token = parser.current_token;
    }

    return switch (declaration_token.token_type) {
        .keyword_type, .keyword_input, .keyword_interface, .keyword_schema => {
            try parser.advance();

            const type_identifier = switch (parser.current_token.token_type) {
                .identifier => parser.current_token.token_text,
                else => if (declaration_token.token_type == .keyword_schema)
                    declaration_token.token_text
                else {
                    parser.error_info.wanted = "name for type, input, or interface";
                    return error.UnexpectedToken;
                },
            };

            if (declaration_token.token_type != .keyword_schema) {
                try parser.advance();
            }

            const implements: ?[]ast.NamedType = try parseImplements(parser, allocator);

            const directives: ?[]ast.Directive = try parseDirectives(parser, allocator);

            const fields = try parseFields(parser, allocator);

            return ast.SchemaDeclaration{
                .type_declaration = ast.TypeDeclaration{
                    .description = description,
                    .extends = extends,
                    .name = type_identifier,
                    .definition = switch (declaration_token.token_type) {
                        .keyword_schema => .{ .schema_definition = fields },
                        .keyword_type => .{ .type_definition = fields },
                        .keyword_interface => .{ .interface_definition = fields },
                        .keyword_input => .{ .input_definition = fields orelse return error.EmptyInputDefinition },
                        else => @panic("Invalid keyword for type definition"),
                    },
                    .implements = implements,
                    .directives = directives,
                },
            };
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
                .type_declaration = ast.TypeDeclaration{
                    .description = description,
                    .extends = extends,
                    .name = identifier,
                    .definition = .scalar_definition,
                    .implements = null,
                    .directives = null,
                },
            };
        },
        .keyword_enum => {
            try parser.advance();
            return error.NotImplemented;
        },
        .keyword_directive => {
            if (extends) {
                return Error.UnexpectedToken;
            }
            try parser.advance();
            var directive_declaration = try parseDirectiveDeclaration(parser, allocator);
            directive_declaration.description = description;
            return ast.SchemaDeclaration{
                .directive_declaration = directive_declaration,
            };
        },
        else => {
            parser.error_info.wanted = "type, input, interface, scalar, enum, or directive keyword";
            return error.UnexpectedToken;
        },
    };
}

test "parseDirectiveDeclaration" {
    {
        const input =
            \\@my_directive(
            \\  arg_one: Boolean
            \\  arg_two: String, arg_three: [Boolean!]
            \\) repeatable on FIELD_DEFINITION | 
            \\     ARGUMENT_DEFINITION | 
            \\     INPUT_FIELD_DEFINITION | ENUM_VALUE
        ;

        var lexer: Lexer = .init(input);
        var parser: Parser = try .init(&lexer);

        const directive_declaration = try parseDirectiveDeclaration(&parser, std.testing.allocator);
        defer destroyDirectiveDeclaration(directive_declaration, std.testing.allocator);

        const argument_definitions = directive_declaration.arguments orelse return error.UnexpectedNull;
        try std.testing.expectEqual(3, argument_definitions.len);

        try std.testing.expectEqual(4, directive_declaration.targets.len);

        try std.testing.expectEqualStrings("my_directive", directive_declaration.name);
    }
}

fn parseDirectiveDeclaration(
    parser: *Parser,
    allocator: Allocator,
) Error!ast.DirectiveDeclaration {
    if (parser.current_token.token_type == .at_sign) {
        try parser.advance();
    } else {
        parser.error_info.wanted = "an '@' symbol prior to the directive's name";
        return error.UnexpectedToken;
    }

    const directive_ident = switch (parser.current_token.token_type) {
        .identifier => parser.current_token.token_text,
        else => {
            parser.error_info.wanted = "Identifier for directive declaration";
            return error.UnexpectedToken;
        },
    };

    try parser.advance();

    const argument_definitions: ?[]ast.ArgumentDefinition = try parseArgumentDefinitions(parser, allocator);
    errdefer if (argument_definitions) |defs| destroyArgumentDefinitions(defs, allocator);

    var repeatable: bool = false;
    if (parser.current_token.token_type == .keyword_repeatable) {
        repeatable = true;
        try parser.advance();
    }

    if (parser.current_token.token_type == .keyword_on) {
        try parser.advance();
    } else {
        parser.error_info.wanted = "expecting keyword 'on' followed by valid directive locations";
        return error.UnexpectedToken;
    }

    var directive_locations: ArrayList(ast.DirectiveLocation) = .empty;
    errdefer directive_locations.deinit(allocator);

    while (true) {
        const directive_location_ident = switch (parser.current_token.token_type) {
            .identifier => parser.current_token.token_text,
            else => {
                parser.error_info.wanted = "Expecting identifier for graphql directive location";
                return error.UnexpectedToken;
            },
        };

        try parser.advance();

        const directive_location = ast.DirectiveLocation.fromString(directive_location_ident) orelse {
            parser.error_info.wanted = "Matching valid identifier for graphql directive location";
            return error.UnexpectedToken;
        };

        try directive_locations.append(allocator, directive_location);

        if (parser.current_token.token_type == .pipe) {
            try parser.advance();
            continue;
        }

        break;
    }

    return ast.DirectiveDeclaration{
        .repeatable = repeatable,
        .name = directive_ident,
        .arguments = argument_definitions,
        .targets = try directive_locations.toOwnedSlice(allocator),
    };
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

    var pairs: ArrayList(ast.ValuePair) = .empty;
    errdefer pairs.deinit(allocator);

    if (parser.current_token.token_type == .l_brace) {
        try parser.advance();

        while (parser.current_token.token_type != .r_brace) {
            const key_identifier, const value = try parseValuePair(parser, allocator);

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

pub fn parseValuePair(parser: *Parser, allocator: Allocator) Error!struct { []const u8, ast.Value } {
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

    const directives: ?[]ast.Directive = try parseDirectives(parser, allocator);

    return ast.Field{
        .description = description,
        .name = field_name,
        .graphql_type = graphql_type,
        .arguments = argument_definitions,
        .directives = directives,
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
        for (arguments) |argument| {
            destroyValue(argument.value, allocator);
        }
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

fn destroyArgumentDefinitions(argument_definitions: []ast.ArgumentDefinition, allocator: Allocator) void {
    for (argument_definitions) |argument_definition| destroyArgumentDefinition(argument_definition, allocator);
    allocator.free(argument_definitions);
}

fn destroyArgumentDefinition(argument: ast.ArgumentDefinition, allocator: Allocator) void {
    if (argument.default) |default_value| {
        destroyValue(default_value, allocator);
    }
    destroyGraphQlType(argument.graphql_type, allocator);

    if (argument.directives) |directives| {
        for (directives) |directive| {
            destroyDirective(directive, allocator);
        }
        allocator.free(directives);
    }
}

fn destroyFields(fields: []ast.Field, allocator: Allocator) void {
    for (fields) |field| destroyField(field, allocator);
    allocator.free(fields);
}

fn destroyField(field: ast.Field, allocator: Allocator) void {
    if (field.arguments) |arguments| {
        for (arguments) |argument| {
            destroyArgumentDefinition(argument, allocator);
        }
        allocator.free(arguments);
    }
    destroyGraphQlType(field.graphql_type, allocator);
    if (field.directives) |directives| {
        for (directives) |directive| {
            destroyDirective(directive, allocator);
        }
        allocator.free(directives);
    }
}

fn destroyDirectiveDeclaration(directive_declaration: ast.DirectiveDeclaration, allocator: Allocator) void {
    if (directive_declaration.arguments) |arguments| {
        for (arguments) |argument| {
            destroyArgumentDefinition(argument, allocator);
        }
        allocator.free(arguments);
    }

    allocator.free(directive_declaration.targets);
}

fn destroyTypeDeclaration(type_declaration: ast.TypeDeclaration, allocator: Allocator) void {
    if (type_declaration.directives) |directives| {
        for (directives) |directive| {
            destroyDirective(directive, allocator);
        }
        allocator.free(directives);
    }
    if (type_declaration.implements) |implements| {
        allocator.free(implements);
    }
    switch (type_declaration.definition) {
        .scalar_definition => {},
        .type_definition,
        .interface_definition,
        .schema_definition,
        => |fields_opt| if (fields_opt) |fields| destroyFields(fields, allocator),
        .input_definition => |fields| destroyFields(fields, allocator),
        else => {},
    }
}
