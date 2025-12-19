const std = @import("std");
const StaticStringMap = std.StaticStringMap;

pub fn Lexer(
    comptime KeywordEnum: type,
    comptime keyword_map: StaticStringMap(KeywordEnum),
    comptime isIdentifierFn: fn (ascii_char: u8, pos: usize) bool,
) type {
    return .{};
}
