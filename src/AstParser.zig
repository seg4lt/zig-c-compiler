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
        // std.log.debug("Parsing failed...", .{});
        self.error_reporter.printError(std.io.getStdErr().writer().any());
        return e;
    };

    if (self.current < self.tokens.len) {
        const last_token = self.peek() orelse unreachable;
        self.parseError(
            ParseError.UnexpectedToken,
            "Unexpected token `{s}` << {s} >> at end of input",
            .{ last_token.lexeme, @tagName(last_token.type) },
        ) catch |e| {
            self.error_reporter.printError(std.io.getStdErr().writer().any());
            return e;
        };
    }

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
        try p.parseError(
            ParseError.StatementExpected,
            "Unexpected end of input while parsing statement",
            .{},
        );
    };
    return switch (token.type) {
        .Return => return try p.parseReturnStmt(),
        else => try p.parseError(
            ParseError.StatementExpected,
            "Unexpected token while parsing statement `{s}` << {s} >>",
            .{ token.lexeme, @tagName(token.type) },
        ),
    };
}

fn parseReturnStmt(p: *Self) ParseError!*Ast.Stmt {
    const return_token = try p.consume(.Return);
    const expr = try p.parseExpr(0);
    _ = try p.consume(.Semicolon);
    return .returnStmt(p.arena, expr, return_token.line, return_token.start);
}

fn parseFactor(p: *Self) ParseError!*Ast.Expr {
    const token = p.peek() orelse {
        try p.parseError(
            ParseError.UnexpectedEndOfToken,
            "Unexpected end of input while parsing factor",
            .{},
        );
    };
    switch (token.type) {
        .IntLiteral => {
            const int_literal = try p.consume(.IntLiteral);
            const value = std.fmt.parseInt(i64, int_literal.lexeme, 10) catch {
                try p.parseError(
                    ParseError.IntExpected,
                    "Expected int found `{s}` << {s} >>",
                    .{ int_literal.lexeme, @tagName(token.type) },
                );
            };
            return .constantExpr(p.arena, value, int_literal.line, int_literal.start);
        },
        .BitNot, .Minus => {
            const op_token = try p.consumeAny();
            const operator = try p.parseUnaryOperator(op_token.type);
            const inner_expr = try p.parseFactor();
            return .unaryExpr(p.arena, operator, inner_expr, op_token.line, op_token.start);
        },
        .LParen => {
            _ = try p.consume(.LParen);
            const inner_expr = try p.parseExpr(0);
            _ = try p.consume(.RParen);
            return inner_expr;
        },
        else => try p.parseError(
            ParseError.UnexpectedToken,
            "Unexpected token while parsing factor`{s}` << {s} >>",
            .{ token.lexeme, @tagName(token.type) },
        ),
    }
}

fn parseExpr(p: *Self, min_prec: usize) ParseError!*Ast.Expr {
    var left = try p.parseFactor();

    while (true) {
        const next_token = p.peek() orelse {
            try p.parseError(
                ParseError.StatementExpected,
                "Unexpected end of input while parsing expression",
                .{},
            );
        };
        const continue_expr_parse = isBinaryOperator(next_token.type) and try p.getPrecedence(next_token.type) >= min_prec;

        if (!continue_expr_parse) {
            break;
        }

        const op_token = try p.consumeAny();
        const op = try p.parseBinaryOperator(op_token.type);
        const right = try p.parseExpr(try p.getPrecedence(next_token.type) + 1);
        const binary: *Ast.Expr = .binaryExpr(p.arena, op, left, right, op_token.line, op_token.start);
        left = binary;
    }
    return left;
}

fn isBinaryOperator(token_type: TokenType) bool {
    return switch (token_type) {
        .Plus,
        .Minus,
        .Multiply,
        .Divide,
        .Mod,
        .BitNot,
        .BitAnd,
        .BitOr,
        .BitXor,
        .LeftShift,
        .RightShift,
        => true,
        else => false,
    };
}

fn parseBinaryOperator(p: Self, token_type: TokenType) ParseError!Ast.BinaryOperator {
    return switch (token_type) {
        .Plus => .Add,
        .Minus => .Subtract,
        .Multiply => .Multiply,
        .Divide => .Divide,
        .Mod => .Mod,
        .BitAnd => .BitAnd,
        .BitOr => .BitOr,
        .BitXor => .BitXor,
        .LeftShift => .LeftShift,
        .RightShift => .RightShift,
        else => try p.parseError(ParseError.CompilerBug, "** Compiler Bug ** parseBinaryOperator called on non binary token", .{}),
    };
}

fn getPrecedence(p: Self, token_type: TokenType) ParseError!usize {
    return switch (token_type) {
        .BitNot => 70,
        .Divide, .Multiply, .Mod => 50,
        .Minus, .Plus => 45,
        .LeftShift, .RightShift => 40,
        .BitAnd => 25,
        .BitOr => 24,
        .BitXor => 23,
        else => try p.parseError(ParseError.CompilerBug, "** Compiler Bug ** precedence level asked for something that doesn't support precendence", .{}),
    };
}

fn parseUnaryOperator(p: Self, token_type: TokenType) ParseError!Ast.UnaryOperator {
    return switch (token_type) {
        .Minus => .Negate,
        .BitNot => .BitNot,
        else => try p.parseError(ParseError.CompilerBug, "** Compiler Bug ** parseUnaryOperator called on non unary token", .{}),
    };
}

fn parseError(p: Self, e: ParseError, comptime fmt: []const u8, args: anytype) ParseError!noreturn {
    const token = p.peek() orelse {
        // If we reach here, it means we are at the end of input.
        // We can report the error without a token.
        p.error_reporter.addError(0, 0, fmt, args);
        return e;
    };
    p.error_reporter.addError(token.line, token.start, fmt, args);
    return e;
}

fn consumeAny(p: *Self) ParseError!*const Token {
    defer p.current += 1;
    return p.peek() orelse {
        try p.parseError(
            ParseError.UnexpectedToken,
            "Unexpected end of input while consuming any token",
            .{},
        );
    };
}

fn consume(p: *Self, expected: TokenType) ParseError!*const Token {
    const tok = p.peek() orelse {
        try p.parseError(
            ParseError.UnexpectedToken,
            "Unexpected end of input while consuming token",
            .{},
        );
    };
    if (tok.type != expected) {
        try p.parseError(
            ParseError.UnexpectedToken,
            "Expected << {s} >> found << {s} >>",
            .{ @tagName(expected), @tagName(tok.type) },
        );
    }
    p.current += 1;
    return tok;
}

fn peek(p: Self) ?*const Token {
    return p.peekOffset(0);
}
fn peekOffset(p: Self, offset: i8) ?*const Token {
    if (p.isAtEnd()) return null;
    return &p.tokens[p.current + @as(usize, @intCast(offset))];
}

fn isAtEnd(p: Self) bool {
    return p.isAtEndOffset(0);
}
fn isAtEndOffset(p: Self, offset: usize) bool {
    return (p.current + offset) >= p.tokens.len;
}

pub const Ast = struct {
    pub const Program = struct {
        @"fn": *FnDecl,

        pub fn init(allocator: Allocator, @"fn": *FnDecl) *Program {
            const pg = allocator.create(Program) catch unreachable;
            pg.* = .{ .@"fn" = @"fn" };
            return pg;
        }
    };
    pub const FnDecl = struct {
        name: []const u8,
        body: *Stmt,
        loc: SourceLocation,

        pub fn init(allocator: Allocator, name: []const u8, body: *Stmt, line: usize, start: usize) *FnDecl {
            const fn_decl = allocator.create(FnDecl) catch unreachable;
            fn_decl.* = .{
                .name = name,
                .body = body,
                .loc = .{ .line = line, .start = start },
            };
            return fn_decl;
        }
    };
    pub const Stmt = union(enum) {
        Return: struct { expr: *Expr, loc: SourceLocation },

        pub fn returnStmt(allocator: Allocator, expr: *Expr, line: usize, start: usize) *Stmt {
            const stmt = allocator.create(Stmt) catch unreachable;
            stmt.* = .{
                .Return = .{
                    .expr = expr,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return stmt;
        }
    };
    pub const Expr = union(enum) {
        Constant: struct { value: i64, loc: SourceLocation },
        Unary: struct { operator: UnaryOperator, expr: *Expr, loc: SourceLocation },
        Binary: struct { operator: BinaryOperator, left: *Expr, right: *Expr, loc: SourceLocation },

        pub fn binaryExpr(allocator: Allocator, operator: BinaryOperator, left: *Expr, right: *Expr, line: usize, start: usize) *Expr {
            const expr = allocator.create(Expr) catch unreachable;
            expr.* = .{
                .Binary = .{
                    .operator = operator,
                    .left = left,
                    .right = right,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return expr;
        }

        pub fn unaryExpr(allocator: Allocator, operator: UnaryOperator, inner_expr: *Expr, line: usize, start: usize) *Expr {
            const expr = allocator.create(Expr) catch unreachable;
            expr.* = .{
                .Unary = .{
                    .operator = operator,
                    .expr = inner_expr,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return expr;
        }

        pub fn constantExpr(allocator: Allocator, value: i64, line: usize, start: usize) *Expr {
            const expr = allocator.create(Expr) catch unreachable;
            expr.* = .{
                .Constant = .{
                    .value = value,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return expr;
        }
    };
    const UnaryOperator = enum {
        Negate,
        BitNot,
    };
    const BinaryOperator = enum {
        Add,
        Subtract,
        Multiply,
        Divide,
        Mod,
        BitAnd,
        BitOr,
        BitXor,
        LeftShift,
        RightShift,
    };

    const SourceLocation = struct {
        line: usize,
        start: usize,
    };
};

const ParseError = error{
    UnexpectedToken,
    StatementExpected,
    IntExpected,
    UnexpectedEndOfToken,
} || CompilerError;

const AstPrinter = struct {
    pub fn print(writer: AnyWriter, pg: *const Ast.Program) void {
        write(writer, "-- AST --\n");
        write(writer, "program\n");
        printFnDecl(writer, pg.@"fn", 1);
        write(writer, "\n");
    }
    fn printFnDecl(writer: AnyWriter, fn_decl: *const Ast.FnDecl, depth: usize) void {
        printSpace(writer, depth);

        writeFmt(writer, "int {s}(void) {{\n", .{fn_decl.name});
        printStmt(writer, fn_decl.body, depth + 1);

        printSpace(writer, depth);
        write(writer, "}");
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
                writeFmt(writer, "{d}", .{constant_expr.value});
            },
            .Unary => |unary_expr| {
                write(writer, "(");
                writeFmt(writer, "{s}", .{
                    switch (unary_expr.operator) {
                        .Negate => "-",
                        .BitNot => "~",
                    },
                });
                printExpr(writer, unary_expr.expr);
                write(writer, ")");
            },
            .Binary => |binary_expr| {
                write(writer, "(");
                printExpr(writer, binary_expr.left);
                writeFmt(writer, " {s} ", .{
                    switch (binary_expr.operator) {
                        .Add => "+",
                        .Subtract => "-",
                        .Multiply => "*",
                        .Divide => "/",
                        .Mod => "%",
                        .BitAnd => "&",
                        .BitOr => "|",
                        .BitXor => "^",
                        .LeftShift => "<<",
                        .RightShift => ">>",
                    },
                });
                printExpr(writer, binary_expr.right);
                write(writer, ")");
            },
        }
    }

    fn printSpace(writer: AnyWriter, depth: usize) void {
        for (0..depth) |_| write(writer, "  ");
    }

    fn write(w: AnyWriter, bytes: []const u8) void {
        _ = w.write(bytes) catch unreachable;
    }

    fn writeFmt(w: AnyWriter, comptime fmt: []const u8, args: anytype) void {
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
const CompilerError = @import("util.zig").CompilerError;
