src: []const u8,
tokens: std.ArrayList(Token),
line: usize = 1,
start: usize = 0,
current: usize = 0,

print_tokens: bool = false,
error_reporter: *ErrorReporter,

arena: Allocator,
scratch_arena: Allocator,

const Self = @This();

const LexerOptions = struct {
    arena: Allocator,
    scratch_arena: Allocator,
    src: []const u8,
    error_reporter: *ErrorReporter,
    print_tokens: bool = false,
};

pub fn parseTokens(opt: LexerOptions) LexerError![]const Token {
    const tokens = std.ArrayList(Token).init(opt.arena);
    var l = Self{
        .src = opt.src,
        .tokens = tokens,
        .arena = opt.arena,
        .scratch_arena = opt.scratch_arena,
        .error_reporter = opt.error_reporter,
        .print_tokens = opt.print_tokens,
    };

    l.scan();

    if (l.error_reporter.error_items.items.len > 0) {
        l.error_reporter.printError(std.io.getStdErr().writer().any());
        return LexerError.LexerFailed;
    }

    if (l.print_tokens) {
        printTokens(l.tokens, std.io.getStdErr().writer().any());
    }
    return l.tokens.items;
}

fn appendError(s: *Self, comptime fmt: []const u8, args: anytype) void {
    s.error_reporter.addError(s.line, s.start, fmt, args);

    // If error occurs, go to next line and start parsing again
    // maybe not full proof, but maybe good enough to find as much error as possible
    while (!s.isAtEnd(0) and s.peek() != '\n') {
        s.current += 1;
    }
    s.line += 1;
    s.current += 1;
}

pub fn printTokens(tokens: std.ArrayList(Token), writer: std.io.AnyWriter) void {
    writer.print("-- Lexer Print --\n", .{}) catch unreachable;
    writer.print("{s:>20}{s:>10}{s:>10}{s:>10}{s}\n", .{ "TokenType", "", "Lexeme", "", "Location" }) catch unreachable;
    for (tokens.items) |item| {
        writer.print(
            "{s:>20}{s:>10}{s:>10}{s:>10}{d}:{d}\n",
            .{
                @tagName(item.type),
                "",
                item.lexeme,
                "",
                item.line,
                item.start,
            },
        ) catch unreachable;
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
            '(' => s.addToken(.LParen),
            ')' => s.addToken(.RParen),
            '{' => s.addToken(.LCurly),
            '}' => s.addToken(.RCurly),
            ';' => s.addToken(.Semicolon),
            '~' => s.addToken(.BitNot),
            '!' => {
                if (s.peek() == '=') {
                    _ = s.consumeAny();
                    s.addToken(.NotEqual);
                    continue;
                }
                s.addToken(.Not);
            },
            '=' => {
                if (s.peek() == '=') {
                    _ = s.consumeAny();
                    s.addToken(.EqualEqual);
                    continue;
                }
                s.addToken(.Assign);
            },
            '&' => {
                if (s.peek() == '&') {
                    _ = s.consumeAny();
                    s.addToken(.And);
                    continue;
                }
                s.addToken(.BitAnd);
            },
            '|' => {
                if (s.peek() == '|') {
                    _ = s.consumeAny();
                    s.addToken(.Or);
                    continue;
                }
                s.addToken(.BitOr);
            },
            '^' => s.addToken(.BitXor),
            '>' => {
                if (s.peek() == '>') {
                    _ = s.consumeAny();
                    s.addToken(.RightShift);
                    continue;
                }
                if (s.peek() == '=') {
                    _ = s.consumeAny();
                    s.addToken(.GreaterThanEqual);
                    continue;
                }
                s.addToken(.GreaterThan);
            },
            '<' => {
                if (s.peek() == '<') {
                    _ = s.consumeAny();
                    s.addToken(.LeftShift);
                    continue;
                }
                if (s.peek() == '=') {
                    _ = s.consumeAny();
                    s.addToken(.LessThanEqual);
                    continue;
                }
                s.addToken(.LessThan);
            },
            '+' => s.addToken(.Plus),
            '-' => {
                if (s.peek() == '-') {
                    @panic("TODO: -- is is not implemented yet");
                    // _ = s.consumeAny();
                    // s.addToken("--", .MinusMinus);
                    // continue;
                }
                s.addToken(.Minus);
            },
            '/' => switch (s.peek()) {
                '/' => s.comment(),
                '*' => s.comment(),
                else => s.addToken(.Divide),
            },
            '*' => s.addToken(.Multiply),
            '%' => s.addToken(.Mod),
            else => {
                s.appendError("Unknown character: {c}\n", .{c});
            },
        }
    }
}

fn comment(s: *Self) void {
    switch (s.peek()) {
        '/' => {
            while (!s.isAtEnd(0) and s.peek() != '\n') {
                _ = s.consumeAny();
            }
            s.line += 1;
        },
        '*' => {
            _ = s.consumeAny();
            while (!s.isAtEnd(0)) {
                const c = s.consumeAny();
                if (c == '\n') {
                    s.line += 1;
                }
                if (c == '*' and s.peek() == '/') break;
            }
            _ = s.consume('/'); // consume closing /
        },
        else => s.appendError("error parsing comment\n", .{}),
    }
}

fn number(s: *Self) void {
    var found_alphabet = false;
    while (!found_alphabet and !s.isAtEnd(0) and (std.ascii.isDigit(s.peek()) or std.ascii.isAlphabetic(s.peek()) or s.peek() == '_')) {
        if (std.ascii.isAlphabetic(s.peek())) found_alphabet = true;
        _ = s.consumeAny();
    }
    if (found_alphabet) {
        s.appendError("invalid number\n", .{});
        return; // Don't add a token when we find an error
    }
    s.addToken(.IntLiteral);
}

fn consume(s: *Self, char: u8) u8 {
    defer s.current += 1;
    const c = s.peek();
    if (c != char) {
        s.appendError("Unexpected character: {c}\n", .{c});
        return 0;
    }
    if (c == '\n') s.line += 1;
    return c;
}

fn consumeAny(s: *Self) u8 {
    defer s.current += 1;
    const c = s.peek();
    if (c == '\n') s.line += 1;
    return c;
}

fn identOrKeyword(s: *Self) void {
    while (!s.isAtEnd(0) and (std.ascii.isAlphabetic(s.peek()) or std.ascii.isDigit(s.peek()) or s.peek() == '_')) {
        _ = s.consumeAny();
    }
    const lexeme = s.src[s.start..s.current];
    const token_type = getTokenType(lexeme);
    s.addToken(token_type);
}

fn addToken(s: *Self, token_type: TokenType) void {
    const lexeme = s.src[s.start..s.current];
    s.tokens.append(Token{
        .lexeme = lexeme,
        .line = s.line,
        .start = s.start,
        .type = token_type,
    }) catch unreachable;
}

fn getTokenType(lexeme: []const u8) TokenType {
    if (std.mem.eql(u8, "int", lexeme)) return .Int;
    if (std.mem.eql(u8, "void", lexeme)) return .Void;
    if (std.mem.eql(u8, "return", lexeme)) return .Return;
    return .Ident;
}

fn peekOffset(s: *const Self, offset: usize) u8 {
    if (s.isAtEnd(offset)) return 0;
    return s.src[s.current + offset];
}

fn peek(s: *const Self) u8 {
    if (s.isAtEnd(0)) return 0;
    return s.src[s.current];
}

fn isAtEnd(s: *const Self, offset: usize) bool {
    return s.current + offset >= s.src.len;
}

const LexerError = error{LexerFailed};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: usize,
    start: usize,
};

pub const TokenType = enum {
    // single-character tokens
    LParen,
    RParen,
    LCurly,
    RCurly,
    Semicolon,

    // operators
    BitNot,
    BitAnd,
    BitOr,
    BitXor,
    LeftShift,
    RightShift,
    Plus,
    Minus,
    Multiply,
    Divide,
    Mod,
    Not,
    And,
    Or,
    EqualEqual,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Assign,
    MinusMinus,

    //
    Ident,
    IntLiteral,

    // keyword
    Int,
    Return,
    Void,
};

const std = @import("std");
const log = std.log;
const Allocator = std.mem.Allocator;
const ErrorReporter = @import("ErrorReporter.zig");

test {
    const tests = @import("tests/lexer_test.zig");
    @import("std").testing.refAllDeclsRecursive(tests);
}
