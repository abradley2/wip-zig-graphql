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

    return schema_declarations.items;
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

fn parseSchemaDeclaration(parser: *Parser, allocator: Allocator) Error!ast.SchemaDeclaration {
    const declaration_token = parser.current_token;

    return switch (declaration_token.token_type) {
        .keyword_type, .keyword_input, .keyword_interface => {
            try parser.advance();

            var object_kind: ast.ObjectKind = .default_type;
            if (declaration_token.token_type == .keyword_input) object_kind = .input_type;
            if (declaration_token.token_type == .keyword_interface) object_kind = .interface_type;

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

fn parseObject(parser: *Parser, allocator: Allocator, object_kind: ast.ObjectKind) Error!ast.Object {
    if (parser.current_token.token_type == .keyword_implements) {
        // todo handle implements
    }

    if (parser.current_token.token_type == .at_sign and
        parser.peek_token.token_type == .identifier)
    {
        // todo handle directives
    }

    if (parser.current_token.token_type == .l_brace) {
        try parser.advance();
    } else {
        parser.error_info.wanted = "{ for object selection";
        return error.UnexpectedToken;
    }

    var fields: ArrayList(ast.Field) = .empty;
    errdefer {
        if (fields.items.len > 0) allocator.free(fields.items);
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
        .directives = &[_]ast.Directive{},
        .implements = &[_][]const u8{},
        .fields = fields.items,
        .kind = object_kind,
    };
}

test "Parse Field" {
    {
        const input = "hello: [World]";

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

    if (parser.current_token.token_type == .l_paren) {
        // handle arguments
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
        .arguments = &[_]ast.Argument{},
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
