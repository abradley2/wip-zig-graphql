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

        const is_scalar = switch (graph_type.data) {
            .scalar_type => true,
            else => false,
        };

        try std.testing.expectEqual(true, is_scalar);
        try std.testing.expectEqualStrings("MyScalar", graph_type.type_ref);
    }
}

fn parseSchemaDeclaration(parser: *Parser, allocator: Allocator) Error!ast.SchemaDeclaration {
    const declaration_token = parser.current_token;
    _ = allocator;

    return switch (declaration_token.token_type) {
        .keyword_type, .keyword_input, .keyword_interface => {
            try parser.advance();
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
                    .data = .scalar_type,
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

fn parseObject(parser: *Parser, allocator: Allocator) Error!ast.Object {
    _ = parser;
    _ = allocator;
    return error.NotImplemented;
}

fn parseField(parser: *Parser, allocator: Allocator) Error!ast.Field {
    const field_name = switch (parser.current_token.token_type) {
        .identifier => parser.current_token.token_text,
        else => {
            parser.error_info.wanted = "field name identifier";
            return error.UnexpectedToken;
        },
    };

    parser.advance();

    if (parser.current_token.token_type == .colon) {
        parser.advance();
    } else {
        parser.error_info.wanted = "colon following field name identifier";
        return error.UnexpectedToken;
    }

    const named_type = try parseNamedType(parser, allocator);

    return ast.Field{
        .name = field_name,
        .field_type = named_type,
    };
}

fn parseNamedType(parser: *Parser, allocator: Allocator) Error!ast.NamedType {
    if (parser.current_token.token_type == .l_bracket) {
        parser.advance();

        const child = try allocator.create(ast.NamedType);
        child.* = try parseNamedType(parser, allocator);

        var nullable = true;
        if (parser.peek_token.token_type == .ex_mark) {
            nullable = false;
            parser.advance();
        }

        if (parser.current_token.token_type == .r_bracket) {
            parser.advance();
            return ast.NamedType{
                .child = child,
                .is_list = true,
                .is_nullable = nullable,
                .type_ref = null,
            };
        } else {
            parser.error_info.wanted = "closing delimiter for [";
            return error.UnexpectedToken;
        }
    }

    const type_ref = switch (parser.current_token.token_type) {
        .identifier => parser.current_token.token_text,
        else => {
            parser.error_info.wanted = "identifier for field type";
            return error.UnexpectedToken;
        },
    };

    var nullable = true;

    if (parser.peek_token.token_type == .ex_mark) {
        nullable = false;
        parser.advance();
    }

    return ast.NamedType{
        .child = null,
        .is_list = false,
        .is_nullable = nullable,
        .type_ref = type_ref,
    };
}
