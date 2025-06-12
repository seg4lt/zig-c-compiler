src: []const u8,
tokens: std.ArrayList(Token),
line: usize = 1,
start: usize = 0,
current: usize = 0,

const Self = @This();

const LexerOptions = struct {
    print_tokens: bool = false,
};

pub fn parseTokens(allocator: Allocator, src: []const u8, opt: LexerOptions) std.ArrayList(Token) {
    var lexer = Self{
        .src = src,
        .tokens = std.ArrayList(Token).init(allocator),
    };
    lexer.scan();
    if (opt.print_tokens) printTokens(lexer.tokens, std.io.getStdOut().writer().any());
    return lexer.tokens;
}

fn printTokens(tokens: std.ArrayList(Token), writer: std.io.AnyWriter) void {
    writer.print("-- Lexer Print --\n", .{}) catch unreachable;
    writer.print("{s:>20}\t{s:>10}\t{s}\n", .{ "TokenType", "Lexeme", "Location" }) catch unreachable;
    for (tokens.items) |item| {
        writer.print("{s:>20}\t{s:>10}\t{d}:{d}\n", .{ @tagName(item.type), item.lexeme, item.line, item.start }) catch unreachable;
    }
}

fn scan(s: *Self) void {
    while (!s.isAtEnd(0)) {
        s.start = s.current;
        const c = s.consumeAny();
        if (std.ascii.isWhitespace(c)) continue;

        if (std.ascii.isAlphabetic(c) or c == '_') {
            s.identOrKeyword();
            continue;
        }
        if (std.ascii.isDigit(c)) {
            s.number();
            continue;
        }
        switch (c) {
            '(' => s.addToken("(", .lparen),
            ')' => s.addToken(")", .rparen),
            '{' => s.addToken("{", .lcurly),
            '}' => s.addToken("}", .rcurly),
            ';' => s.addToken(";", .semicolon),
            else => {
                // TODO: Error Reporter needed!!!
                log.debug("Unknown character: {c}", .{c});
            },
        }
    }
}

fn number(s: *Self) void {
    var found_alphabet = false;
    while (!found_alphabet and !s.isAtEnd(0) and std.ascii.isDigit(s.peek())) {
        if (std.ascii.isAlphabetic(s.peek())) found_alphabet = true;
        _ = s.consumeAny();
    }
    if (found_alphabet) {
        // TODO
        // Error Reporter needed!!!
    }
    const lexeme = s.src[s.start..s.current];
    s.addToken(lexeme, .int_literal);
}

fn consumeAny(s: *Self) u8 {
    defer s.current += 1;
    const c = s.peek();
    if (c == '\n') s.line += 1;
    return c;
}

fn identOrKeyword(s: *Self) void {
    while (!s.isAtEnd(0) and (std.ascii.isAlphabetic(s.peek()) or s.peek() == '_')) {
        _ = s.consumeAny();
    }
    const lexeme = s.src[s.start..s.current];
    const token_type = getTokenType(lexeme);
    s.addToken(lexeme, token_type);
}

fn addToken(s: *Self, lexeme: []const u8, token_type: TokenType) void {
    s.tokens.append(Token{
        .lexeme = lexeme,
        .line = s.line,
        .start = s.start,
        .type = token_type,
    }) catch unreachable;
}

fn getTokenType(lexeme: []const u8) TokenType {
    if (std.mem.eql(u8, "int", lexeme)) return .int;
    if (std.mem.eql(u8, "void", lexeme)) return .void;
    if (std.mem.eql(u8, "return", lexeme)) return .@"return";
    return .ident;
}

fn peekOffset(s: *const Self, offset: usize) u8 {
    if (s.isAtEnd(offset)) return 0;
    return s.src[s.current + offset];
}

fn peek(s: *const Self) u8 {
    return s.src[s.current];
}

fn isAtEnd(s: *const Self, offset: usize) bool {
    return s.current + offset >= s.src.len;
}

pub const LexerError = error{};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: usize,
    start: usize,
};

pub const TokenType = enum {
    // single-character tokens
    lparen,
    rparen,
    lcurly,
    rcurly,
    semicolon,

    //
    ident,
    int_literal,

    // keyword
    int,
    @"return",
    void,
};

const std = @import("std");
const log = std.log;
const Allocator = std.mem.Allocator;
