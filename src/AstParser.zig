tokens: []const Token,
current: usize = 0,

arena: Allocator,
scratch_arena: Allocator,
error_reporter: *ErrorReporter,

const Self = @This();

const Options = struct {
    tokens: []const Token,
    arena: Allocator,
    scratch_arena: Allocator,
    error_reporter: *ErrorReporter,
    print_ast: bool = false,
};

pub fn parse(opt: Options) ParseError!*Ast.Program {
    var self: Self = .{
        .tokens = opt.tokens,
        .arena = opt.arena,
        .scratch_arena = opt.scratch_arena,
        .error_reporter = opt.error_reporter,
    };

    const pg = self.parseProgram() catch |e| {
        self.error_reporter.printError();
        return e;
    };

    if (opt.print_ast) AstPrinter.print(std.io.getStdOut().writer().any(), pg);

    return pg;
}

fn parseProgram(p: *Self) ParseError!*Ast.Program {
    const fn_decl: *Ast.FnDecl = try p.parseFnDecl();
    return .init(p.arena, fn_decl);
}

fn parseFnDecl(p: *Self) ParseError!*Ast.FnDecl {
    _ = try p.consume(.Int);
    const ident = try p.consume(.Ident);

    try p.parseFnParams();

    _ = try p.consume(.LCurly);
    const body = try p.parseStmt();
    _ = try p.consume(.RCurly);
    return .init(p.arena, ident.lexeme, body, ident.line, ident.start);
}

fn parseFnParams(p: *Self) ParseError!void {
    _ = try p.consume(.LParen);
    if (p.peek()) |token| {
        if (token.type == .Void) _ = try p.consume(.Void);
    }
    _ = try p.consume(.RParen);
}

fn parseStmt(p: *Self) ParseError!*Ast.Stmt {
    const token = p.peek() orelse {
        try p.parseError(ParseError.StatementExpected, "Unexpected end of input while parsing statement", .{});
    };
    return switch (token.type) {
        .Return => return try p.parseReturnStmt(),
        else => try p.parseError(ParseError.StatementExpected, "Unexpected token {s}", .{token.lexeme}),
    };
}

fn parseReturnStmt(p: *Self) ParseError!*Ast.Stmt {
    const return_token = try p.consume(.Return);
    const expr = try p.parseExpr();
    _ = try p.consume(.Semicolon);
    return .returnStmt(p.arena, expr, return_token.line, return_token.start);
}

fn parseExpr(p: *Self) ParseError!*Ast.Expr {
    const token = p.peek() orelse {
        try p.parseError(ParseError.StatementExpected, "Unexpected end of input while parsing expression", .{});
    };
    switch (token.type) {
        .IntLiteral => {
            const int_literal = try p.consume(.IntLiteral);
            const value = std.fmt.parseInt(i64, int_literal.lexeme, 10) catch {
                try p.parseError(ParseError.IntExpected, "Expected int found {s}", .{int_literal.lexeme});
            };
            return .constantExpr(p.arena, value, int_literal.line, int_literal.start);
        },
        else => try p.parseError(ParseError.UnexpectedToken, "Unexpected token {s}", .{token.lexeme}),
    }
}

fn parseError(p: *Self, e: ParseError, comptime fmt: []const u8, args: anytype) ParseError!noreturn {
    const token = p.peek().?;
    p.error_reporter.addError(token.line, token.start, fmt, args);
    return e;
}

fn consumeAny(p: *Self) *const Token {
    defer p.current += 1;
    return p.peek(0);
}

fn consume(p: *Self, expected: TokenType) ParseError!*const Token {
    const tok = p.peek() orelse {
        try p.parseError(ParseError.UnexpectedToken, "Unexpected end of input while consuming token", .{});
    };
    if (tok.type != expected) {
        try p.parseError(ParseError.UnexpectedToken, "Expected {s} found {s}", .{ @tagName(expected), @tagName(tok.type) });
    }
    p.current += 1;
    return tok;
}

fn peek(p: *Self) ?*const Token {
    return p.peekOffset(0);
}
fn peekOffset(p: *Self, offset: i8) ?*const Token {
    if (p.isAtEnd()) return null;
    // const offset_ = @as(usize, @intCast(offset));
    return &p.tokens[p.current + @as(usize, @intCast(offset))];
}

fn isAtEnd(p: *Self) bool {
    return p.isAtEndOffset(0);
}
fn isAtEndOffset(p: *Self, offset: usize) bool {
    return (p.current + offset) >= p.tokens.len;
}

const Ast = struct {
    const Program = struct {
        @"fn": *FnDecl,

        pub fn init(allocator: Allocator, @"fn": *FnDecl) *Program {
            const pg = allocator.create(Program) catch unreachable;
            pg.* = .{ .@"fn" = @"fn" };
            return pg;
        }
    };
    const FnDecl = struct {
        name: []const u8,
        body: *Stmt,
        loc: SourceLocation,

        pub fn init(allocator: Allocator, name: []const u8, body: *Stmt, line: usize, start: usize) *FnDecl {
            const fn_decl = allocator.create(FnDecl) catch unreachable;
            fn_decl.* = .{ .name = name, .body = body, .loc = .{ .line = line, .start = start } };
            return fn_decl;
        }
    };
    const Stmt = union(enum) {
        Return: struct { expr: *Expr, loc: SourceLocation },

        pub fn returnStmt(allocator: Allocator, expr: *Expr, line: usize, start: usize) *Stmt {
            const stmt = allocator.create(Stmt) catch unreachable;
            stmt.* = .{ .Return = .{ .expr = expr, .loc = .{ .line = line, .start = start } } };
            return stmt;
        }
    };
    const Expr = union(enum) {
        Constant: struct { value: i64, loc: SourceLocation },

        pub fn constantExpr(allocator: Allocator, value: i64, line: usize, start: usize) *Expr {
            const expr = allocator.create(Expr) catch unreachable;
            expr.* = .{ .Constant = .{ .value = value, .loc = .{ .line = line, .start = start } } };
            return expr;
        }
    };
    const SourceLocation = struct { line: usize, start: usize };
};

const ParseError = error{ UnexpectedToken, StatementExpected, IntExpected };

const AstPrinter = struct {
    pub fn print(writer: AnyWriter, pg: *const Ast.Program) void {
        write(writer, "-- AST --\n");
        write(writer, "program\n");
        printFnDecl(writer, pg.@"fn", 1);
    }
    fn printFnDecl(writer: AnyWriter, fn_decl: *const Ast.FnDecl, depth: usize) void {
        printSpace(writer, depth);
        write_fmt(writer, "fn {s}\n", .{fn_decl.name});
        printStmt(writer, fn_decl.body, depth + 1);
    }
    fn printStmt(writer: AnyWriter, stmt: *const Ast.Stmt, depth: usize) void {
        printSpace(writer, depth);
        switch (stmt.*) {
            .Return => |return_stmt| {
                write(writer, "return");
                write(writer, " ");
                printExpr(writer, return_stmt.expr);
            },
        }
        write(writer, ";\n");
    }
    fn printExpr(writer: AnyWriter, expr: *const Ast.Expr) void {
        switch (expr.*) {
            .Constant => |constant_expr| {
                write_fmt(writer, "{d}", .{constant_expr.value});
            },
        }
    }

    fn printSpace(writer: AnyWriter, depth: usize) void {
        for (0..depth) |_| write(writer, "  ");
    }

    fn write(w: AnyWriter, comptime bytes: []const u8) void {
        _ = w.write(bytes) catch unreachable;
    }
    fn write_fmt(w: AnyWriter, comptime fmt: []const u8, args: anytype) void {
        _ = w.print(fmt, args) catch unreachable;
    }
};

const std = @import("std");
const ErrorReporter = @import("ErrorReporter.zig");
const Lexer = @import("Lexer.zig");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const TokenType = Lexer.TokenType;
const Token = Lexer.Token;
const AnyWriter = std.io.AnyWriter;
