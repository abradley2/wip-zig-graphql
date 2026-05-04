const std = @import("std");
const StaticStringMap = std.StaticStringMap;

const LexerOptions: type = struct {
    single_line_comment_delimiter: ?[]const u8 = null,
    multi_line_comment_delimiter: ?[]const u8 = null,
    multi_line_string_delimiter: ?[]const u8 = null,
};

pub fn MakeLexer(
    comptime keyword_map: StaticStringMap(u8),
    comptime token_map: std.StaticStringMap(u8),
    comptime isIdentifier: fn (u8, usize) bool,
    comptime options: LexerOptions,
) void {
    comptime var enum_fields: [token_map.values().len]std.builtin.Type.EnumField = undefined;

    comptime var enum_value: u8 = 0;
    inline for (token_map.keys()) |token_name| {
        const token_byte = token_map.get(token_name) orelse unreachable;

        enum_fields[enum_value] = std.builtin.Type.EnumField{
            .value = enum_value,
            .name = token_name,
        };

        enum_value = enum_value + 1;
    }

    const tokens: std.builtin.Type.Enum = .{
        .tag_type = u8,
        .fields = enum_fields,
        .decls = &.{},
        .is_exhaustive = false,
    };

    const Tokens: type = @TypeOf(tokens);

    return struct {
        const Lexer = @This();

        pub fn readToken() void {}

        pub fn readNumber(lexer: *Lexer) []const u8 {
            var end = lexer.read_position;

            var number_position: usize = 0;
            while (end < lexer.input.len and
                (std.ascii.isDigit(lexer.input[end]) or
                    lexer.input[end] == '-' or
                    lexer.input[end] == '.' or
                    (number_position > 0 and lexer.input[end] == 'e') or
                    (number_position > 0 and lexer.input[end] == 'E')))
            {
                end += 1;
                number_position += 1;
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
    };
}
