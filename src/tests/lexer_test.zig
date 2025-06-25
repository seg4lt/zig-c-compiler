const std = @import("std");
const Lexer = @import("../Lexer.zig");
const ErrorReporter = @import("../ErrorReporter.zig");

fn verifyToken(src: []const u8, expected_output: []const u8) !void {
    const testing = std.testing;
    const allocator = testing.allocator;

    var err_buffer = std.ArrayList(u8).init(allocator);

    var err_reporter = ErrorReporter.initWithWriter(allocator, src, "test.zig", err_buffer.writer().any());
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

test "invalid tokens" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var err_buffer = std.ArrayList(u8).init(allocator);
    defer err_buffer.deinit();

    const idents = [_][]const u8{ "1foo", "@foo", "\\", "`" };
    for (idents) |ident| {
        var err_reporter = ErrorReporter.initWithWriter(
            allocator,
            ident,
            "test.zig",
            err_buffer.writer().any(),
        );
        defer err_reporter.deinit();

        try testing.expectError(
            error.LexerFailed,
            Lexer.parseTokens(allocator, ident, &err_reporter, .{}),
        );
    }
}
