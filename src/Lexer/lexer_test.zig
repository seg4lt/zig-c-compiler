const std = @import("std");
const Lexer = @import("Lexer.zig");
const ErrorReporter = @import("../ErrorReporter.zig");

fn verifyToken(src: []const u8, expected_output: []const u8) !void {
    const testing = std.testing;
    const allocator = testing.allocator;

    var err_reporter = ErrorReporter.init(allocator, src, "test.zig");
    defer err_reporter.deinit();

    var tokens = try Lexer.parseTokens(allocator, src, &err_reporter, .{});
    defer tokens.deinit();

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    Lexer.printTokens(tokens, buffer.writer().any());
    try testing.expectEqualStrings(expected_output, buffer.items);
}

test "basic program" {
    const src =
        \\int main(void) {
        \\  return 0;
        \\}
    ;

    const expected_output =
        \\-- Lexer Print --
        \\           TokenType              Lexeme          Location
        \\                 int                 int          1:0
        \\               ident                main          1:4
        \\              lparen                   (          1:8
        \\                void                void          1:9
        \\              rparen                   )          1:13
        \\              lcurly                   {          1:15
        \\              return              return          2:19
        \\         int_literal                   0          2:26
        \\           semicolon                   ;          2:27
        \\              rcurly                   }          3:29
        \\
    ;

    try verifyToken(src, expected_output);
}

test "test return 123" {
    const src = "return 123;";

    const expected_output =
        \\-- Lexer Print --
        \\           TokenType              Lexeme          Location
        \\              return              return          1:0
        \\         int_literal                 123          1:7
        \\           semicolon                   ;          1:10
        \\
    ;
    try verifyToken(src, expected_output);
}

test "invalid identifiers" {
    // Instead of trying to verify tokens, we'll directly test the lexer functionality
    // as these cases should produce errors
    const testing = std.testing;
    const allocator = testing.allocator;

    {
        var err_reporter = ErrorReporter.init(allocator, "1foo", "test.zig");
        defer err_reporter.deinit();

        try testing.expectError(error.LexerFailed, Lexer.parseTokens(allocator, "1foo", &err_reporter, .{}));
    }

    {
        var err_reporter = ErrorReporter.init(allocator, "@foo", "test.zig");
        defer err_reporter.deinit();

        try testing.expectError(error.LexerFailed, Lexer.parseTokens(allocator, "@foo", &err_reporter, .{}));
    }
}

test "invalid tokens" {
    const testing = std.testing;
    const allocator = testing.allocator;
    const data = [_][]const u8{
        "\\",
        "`",
    };

    for (data) |invalid_token| {
        var err_reporter = ErrorReporter.init(allocator, invalid_token, "test.zig");
        defer err_reporter.deinit();

        try testing.expectError(error.LexerFailed, Lexer.parseTokens(allocator, invalid_token, &err_reporter, .{}));
    }
}
