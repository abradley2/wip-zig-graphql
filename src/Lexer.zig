const std = @import("std");
const Token = @import("./Token.zig");
const TokenType = Token.TokenType;

const Lexer = @This();

pub const Error: type = error{
    UnknownToken,
    UnterminatedString,
    UnexpectedEof,
};

input: []const u8,

position: usize = 0,
read_position: usize = 0,

current_char: u8 = 0x00,

indent_on_last_read: bool = false,
newline_on_last_read: bool = false,

test "Lexer Basics" {
    const input =
        \\(hello #some comment
        \\  # another comment
        \\ ,world "again"}a
    ;

    var lexer: Lexer = .init(input);

    var token_count: usize = 0;

    while (lexer.current_char != 0x00) : (lexer.read()) {
        const token = try lexer.nextToken();
        const peek_token = try lexer.peekToken();

        switch (token_count) {
            0 => {
                try std.testing.expectEqual(TokenType.l_paren, token.token_type);
                try std.testing.expectEqualStrings("(", token.token_text);

                try std.testing.expectEqual(TokenType.identifier, peek_token.token_type);
                try std.testing.expectEqualStrings("hello", peek_token.token_text);
            },
            1 => {
                try std.testing.expectEqual(TokenType.identifier, token.token_type);
                try std.testing.expectEqualStrings("hello", token.token_text);
            },
            2 => {
                try std.testing.expectEqual(TokenType.comma, token.token_type);
                try std.testing.expectEqualStrings(",", token.token_text);
            },
            3 => {
                try std.testing.expectEqual(TokenType.identifier, token.token_type);
                try std.testing.expectEqualStrings("world", token.token_text);
            },
            4 => {
                try std.testing.expectEqual(TokenType.string, token.token_type);
                try std.testing.expectEqualStrings("\"again\"", token.token_text);
            },
            5 => {
                try std.testing.expectEqual(TokenType.r_brace, token.token_type);
                try std.testing.expectEqualStrings("}", token.token_text);
            },
            6 => {
                try std.testing.expectEqual(TokenType.identifier, token.token_type);
                try std.testing.expectEqualStrings("a", token.token_text);
            },
            else => return error.ExpectedEof,
        }

        token_count += 1;
    }
}

test "Lexer String Termination and Escaping" {
    {
        const input = "\"hello";

        var lexer: Lexer = .init(input);

        try std.testing.expectError(Error.UnterminatedString, lexer.nextToken());
    }

    {
        const input = "\"hello\"";

        var lexer: Lexer = .init(input);

        const token = try lexer.nextToken();

        try std.testing.expectEqual(TokenType.string, token.token_type);
        try std.testing.expectEqualStrings("\"hello\"", token.token_text);
    }

    {
        const input = try std.fmt.allocPrint(std.testing.allocator, "\"{s}\"", .{"bob says \\\"hello\\\" to you"});
        defer std.testing.allocator.free(input);

        var lexer: Lexer = .init(input);

        const token = try lexer.nextToken();

        try std.testing.expectEqual(TokenType.string, token.token_type);
        try std.testing.expectEqualStrings(input, token.token_text);
    }

    {
        const input =
            \\"bob says \"hello\" to you"
        ;

        var lexer: Lexer = .init(input);

        const token = try lexer.nextToken();

        try std.testing.expectEqual(TokenType.string, token.token_type);
        try std.testing.expectEqualStrings(input, token.token_text);
    }

    {
        const input =
            \\"hello \ there"
        ;

        var lexer: Lexer = .init(input);

        const token = try lexer.nextToken();

        try std.testing.expectEqual(TokenType.string, token.token_type);
        try std.testing.expectEqualStrings("\"hello \\ there\"", token.token_text);
    }
}

test "Keywords" {
    {
        const input: []const u8 = "type MyType {";

        var lexer: Lexer = .init(input);
        var token = try lexer.nextToken();

        try std.testing.expectEqual(TokenType.keyword_type, token.token_type);
        try std.testing.expectEqualStrings("type", token.token_text);

        lexer.read();
        token = try lexer.nextToken();

        try std.testing.expectEqual(TokenType.identifier, token.token_type);
        try std.testing.expectEqualStrings("MyType", token.token_text);

        lexer.read();
        token = try lexer.nextToken();

        try std.testing.expectEqual(TokenType.l_brace, token.token_type);
        try std.testing.expectEqualStrings("{", token.token_text);
    }
}

fn isIdentifier(ascii_char: u8, ascii_char_position: usize) bool {
    if (ascii_char_position == 0) {
        return std.ascii.isAlphabetic(ascii_char);
    }
    return switch (ascii_char) {
        '_' => true,
        else => std.ascii.isAlphanumeric(ascii_char),
    };
}

pub fn init(input: []const u8) Lexer {
    var lexer: Lexer = .{ .input = input };
    lexer.read();

    return lexer;
}

pub fn peekToken(lexer: *Lexer) Error!Token {
    var peek_lexer: Lexer = lexer.*;

    read(&peek_lexer);

    if (peek_lexer.current_char == 0x00) {
        return .{ .token_type = .eof };
    }

    return try nextToken(&peek_lexer);
}

pub fn nextToken(lexer: *Lexer) Error!Token {
    var token: Token = .{ .token_type = .unknown };

    while (lexer.current_char == '#') {
        lexer.readComment();
        lexer.advance();
        lexer.read();
    }

    token.token_type = switch (lexer.current_char) {
        '{' => .l_brace,
        '}' => .r_brace,
        '[' => .l_bracket,
        ']' => .r_bracket,
        '(' => .l_paren,
        ')' => .r_paren,
        '.' => .dot,
        ':' => .colon,
        ';' => .semicolon,
        ',' => .comma,
        '@' => .at_sign,
        '"' => .string,
        '|' => .pipe,
        '!' => .ex_mark,
        '&' => .ampersand,
        '=' => .equals,
        0x00 => .eof,
        else => if (isIdentifier(lexer.current_char, 0))
            .identifier
        else if (std.ascii.isDigit(lexer.current_char) or lexer.current_char == '-')
            .number
        else
            return Error.UnknownToken,
    };

    if (token.token_type == .eof) {
        token.token_text = "<EOF>";
        return token;
    }

    token.token_text = switch (token.token_type) {
        .string => try readString(lexer),
        .number => readNumber(lexer),
        .identifier => readIdentifier(lexer),
        else => lexer.input[lexer.position..lexer.read_position],
    };

    if (token.token_type == .identifier) {
        const keyword_map: std.StaticStringMap(TokenType) = .initComptime([_]struct { []const u8, TokenType }{
            .{ "type", TokenType.keyword_type },
            .{ "interface", TokenType.keyword_interface },
            .{ "input", TokenType.keyword_input },
            .{ "enum", TokenType.keyword_enum },
            .{ "scalar", TokenType.keyword_scalar },
            .{ "directive", TokenType.keyword_directive },
            .{ "query", TokenType.keyword_query },
            .{ "mutation", TokenType.keyword_mutation },
            .{ "subscription", TokenType.keyword_subscription },
            .{ "on", TokenType.keyword_on },
            .{ "implements", TokenType.keyword_implements },
            .{ "true", TokenType.keyword_true },
            .{ "false", TokenType.keyword_false },
            .{ "null", TokenType.keyword_null },
            .{ "schema", TokenType.keyword_schema },
        });

        if (keyword_map.get(token.token_text)) |keyword_token_type| {
            token.token_type = keyword_token_type;
        }
    }

    return token;
}

fn readComment(lexer: *Lexer) void {
    var end = lexer.read_position;
    while (end < lexer.input.len - 1 and
        lexer.input[end] != '\n')
    {
        end += 1;
    }

    lexer.read_position = end + 1;
}

fn readString(lexer: *Lexer) error{UnterminatedString}![]const u8 {
    var end = lexer.read_position;

    var escapeNext = false;
    while (end < lexer.input.len - 1 and
        (lexer.input[end] != '"' or escapeNext))
    {
        end += 1;
        escapeNext = !escapeNext and (lexer.input[end] == 0x1B or lexer.input[end] == '\\');
        if (escapeNext) {
            end += 1;
        }
    }

    if (end >= lexer.input.len or lexer.input[end] != '"') {
        return error.UnterminatedString;
    }

    lexer.read_position = end + 1;
    return lexer.input[lexer.position .. end + 1];
}

fn readNumber(lexer: *Lexer) []const u8 {
    var end = lexer.read_position;

    while (end < lexer.input.len and
        (std.ascii.isDigit(lexer.input[end]) or
            lexer.input[end] == '-' or
            lexer.input[end] == '.'))
    {
        end += 1;
    }

    lexer.read_position = end;
    return lexer.input[lexer.position..end];
}

fn readIdentifier(lexer: *Lexer) []const u8 {
    var end = lexer.read_position;

    while (end < lexer.input.len and
        isIdentifier(lexer.input[end], end - lexer.read_position))
    {
        end += 1;
    }

    lexer.read_position = end;
    return lexer.input[lexer.position..end];
}

pub fn peekChar(lexer: *Lexer) u8 {
    if (lexer.read_position < lexer.input.len) {
        return lexer.input[lexer.read_position];
    }
    return 0x00;
}

fn advance(lexer: *Lexer) void {
    lexer.position = lexer.read_position;
    lexer.read_position += 1;
}

pub fn read(lexer: *Lexer) void {
    lexer.indent_on_last_read = false;
    lexer.newline_on_last_read = false;

    eat_whitespace: while (true) {
        switch (peekChar(lexer)) {
            '\n' => {
                advance(lexer);
                lexer.indent_on_last_read = true;
                lexer.newline_on_last_read = true;
                continue :eat_whitespace;
            },
            ' ', '\t', '\r' => {
                advance(lexer);
                lexer.indent_on_last_read = true;
                continue :eat_whitespace;
            },
            else => break :eat_whitespace,
        }
    }

    if (lexer.read_position >= lexer.input.len) {
        lexer.current_char = 0x00;
    } else {
        lexer.current_char = lexer.input[lexer.read_position];
    }

    advance(lexer);
}
