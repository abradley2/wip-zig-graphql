const std = @import("std");

pub const TokenType: type = enum(u8) {
    // containers
    l_paren,
    r_paren,
    l_brace,
    r_brace,
    l_bracket,
    r_bracket,

    // delimiters
    comma,
    colon,
    semicolon,
    dot,
    at_sign,
    pipe,
    ex_mark,

    // literals and identifiers
    identifier,
    string,
    number,

    // keywords
    keyword_type,
    keyword_interface,
    keyword_input,
    keyword_enum,
    keyword_scalar,
    keyword_directive,
    keyword_query,
    keyword_mutation,
    keyword_subscription,
    keyword_on,
    keyword_implements,

    // special
    unknown,
    eof,
};

token_type: TokenType,
token_text: []const u8 = "",
