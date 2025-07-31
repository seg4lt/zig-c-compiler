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

    if (opt.print_ast) print(self.arena, pg);

    return pg;
}

fn print(arena: Allocator, program: *const Ast.Program) void {
    var buffer = ArrayList(u8).init(arena);
    var stdErr = std.io.getStdErr().writer();
    AstPrinter.print(buffer.writer().any(), program);
    // all of this to check if I have extra space.. Ouch...
    for (buffer.items) |c| {
        _ = (if (c == ' ') stdErr.write("﹍") else stdErr.write(&[_]u8{c})) catch unreachable;
    }
}

fn parseProgram(p: *Self) ParseError!*Ast.Program {
    const fn_decl: *Ast.FnDecl = try p.parseFnDecl();
    return .init(p.arena, fn_decl);
}

fn parseFnDecl(p: *Self) ParseError!*Ast.FnDecl {
    _ = try p.consume(.Int);
    const ident = try p.consume(.Ident);

    try p.parseFnParams();

    const body = try p.parseBlock();

    return .init(p.arena, ident.lexeme, body, ident.line, ident.start);
}

fn parseBlock(p: *Self) ParseError!*Ast.Block {
    const start_token = try p.consume(.LCurly);
    var body = ArrayList(*Ast.BlockItem).init(p.arena);

    while (p.peek()) |token| {
        if (token.type == .RCurly) break;
        const item = try p.parseBlockItem();
        body.append(item) catch unreachable;
    }
    _ = try p.consume(.RCurly);
    return .init(p.arena, body, start_token.line, start_token.start);
}

fn parseBlockItem(p: *Self) ParseError!*Ast.BlockItem {
    const peek_token = p.peek() orelse {
        try p.parseError(
            ParseError.StatementExpected,
            "Unexpected end of input while parsing block item",
            .{},
        );
    };
    return switch (peek_token.type) {
        .Int => .decl(p.arena, try p.parseDecl()),
        .Semicolon => {
            _ = try p.consume(.Semicolon);
            return .stmt(p.arena, .nullStmt(p.arena, peek_token.line, peek_token.start));
        },
        else => .stmt(p.arena, try p.parseStmt()),
    };
}

fn parseDecl(p: *Self) ParseError!*Ast.Decl {
    // We only support `int` at the moment
    const token = p.peek() orelse {
        try p.parseError(ParseError.MissingToken, "expected decl `int`", .{});
    };
    if (token.type != .Int) {
        try p.parseError(ParseError.UnexpectedToken, "expected int found `{s}` << {s} >> ", .{ token.lexeme, @tagName(token.type) });
    }
    _ = try p.consume(.Int);
    const ident_token = try p.consume(.Ident);
    const ident = ident_token.lexeme;

    if (std.ascii.isDigit(ident[0])) {
        try p.parseError(ParseError.InvalidIdent, "identifier should not start with digit. Found `{s}` << {s} >>", .{ ident, @tagName(ident_token.type) });
    }

    var var_initializer: ?*Ast.Expr = null;
    if (p.peek()) |peeked_token| {
        if (peeked_token.type == .Assign) {
            _ = try p.consumeAny();
            var_initializer = try p.parseExpr(0);
        }
    }
    _ = try p.consume(.Semicolon);
    return .variableDecl(p.arena, ident, var_initializer, ident_token.line, ident_token.start);
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
        .Return => try p.parseReturnStmt(),
        .If => try p.parseIfStmt(),
        .LCurly => {
            const body = try p.parseBlock();
            return .compoundStmt(p.arena, body, token.line, token.start);
        },
        else => {
            const peeked_token = p.peek() orelse {
                try p.parseError(
                    ParseError.StatementExpected,
                    "Unexpected token while parsing statement `{s}` << {s} >>",
                    .{ token.lexeme, @tagName(token.type) },
                );
            };
            const expr = try p.parseExpr(0);
            _ = try p.consume(.Semicolon);
            return .exprStmt(p.arena, expr, peeked_token.line, peeked_token.start);
        },
    };
}

fn parseIfStmt(p: *Self) ParseError!*Ast.Stmt {
    const if_token = try p.consume(.If);
    _ = try p.consume(.LParen);
    const condition = try p.parseExpr(0);
    _ = try p.consume(.RParen);

    const then_block = try p.parseStmt();

    var else_block: ?*Ast.Stmt = null;
    if (p.peek()) |token| {
        if (token.type == .Else) {
            _ = try p.consume(.Else);
            else_block = try p.parseStmt();
        }
    }
    return .ifStmt(p.arena, condition, then_block, else_block, if_token.line, if_token.start);
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
        .BitNot, .Minus, .Not => {
            const op_token = try p.consumeAny();
            const operator = try p.parseUnaryOperator(op_token.type);
            const inner_expr = try p.parseFactor();
            return .unaryExpr(p.arena, operator, inner_expr, op_token.line, op_token.start);
        },
        .MinusMinus, .PlusPlus => {
            const op_token = try p.consumeAny();
            var var_expr = try p.parseFactor();
            var_expr = recurseGetGroupInnerExpr(var_expr);

            if (var_expr.* != .Var) {
                try p.parseErrorOnToken(
                    ParseError.InvalidPrefixOperand,
                    op_token,
                    "invalid operand found for prefix operator `{s}`",
                    .{@tagName(var_expr.*)},
                );
            }
            const inner = var_expr.Var;
            const new_var: *Ast.Expr = .varExpr(p.arena, inner.ident, inner.loc.line, inner.loc.start);
            // clone because sema will modify this value to be unique - using same entity will double resolve same ident
            const new_var_clone: *Ast.Expr = .varExpr(p.arena, inner.ident, inner.loc.line, inner.loc.start);

            const one: *Ast.Expr = .constantExpr(p.arena, 1, op_token.line, op_token.start);
            const prefix_op: Ast.BinaryOperator = switch (op_token.type) {
                .MinusMinus => .Subtract,
                .PlusPlus => .Add,
                else => @panic("** Compiler Bug ** only prefix operator should be evaluated"),
            };
            const binary: *Ast.Expr = .binaryExpr(p.arena, prefix_op, new_var, one, op_token.line, op_token.start);
            return .assignmentExpr(p.arena, binary, new_var_clone, op_token.line, op_token.start);
        },
        .LParen => {
            const group_token = try p.consume(.LParen);
            const inner_expr = try p.parseExpr(0);
            const group_expr: *Ast.Expr = .groupExpr(p.arena, inner_expr, group_token.line, group_token.start);
            _ = try p.consume(.RParen);
            return try p.parsePostfixExpr(group_expr);
        },
        .Ident => {
            const ident_token = try p.consumeAny();
            const var_expr: *Ast.Expr = .varExpr(p.arena, ident_token.lexeme, ident_token.line, ident_token.start);
            return p.parsePostfixExpr(var_expr);
        },
        else => try p.parseError(
            ParseError.UnexpectedToken,
            "Unexpected token while parsing factor`{s}` << {s} >>",
            .{ token.lexeme, @tagName(token.type) },
        ),
    }
}

fn parsePostfixExpr(p: *Self, expr: *Ast.Expr) ParseError!*Ast.Expr {
    const peeked_token = p.peek() orelse {
        return expr;
    };
    if (peeked_token.type != .MinusMinus and peeked_token.type != .PlusPlus) {
        return expr;
    }
    const inner_expr = recurseGetGroupInnerExpr(expr);
    if (inner_expr.* != .Var) {
        try p.parseErrorOnToken(
            ParseError.InvalidLValue,
            peeked_token,
            "invalid l value for postfix operator `{s}`",
            .{@tagName(expr.*)},
        );
    }

    const postfix_op_token = try p.consumeAny();
    const postfix_op: Ast.PostfixOperator = switch (postfix_op_token.type) {
        .MinusMinus => .Subtract,
        .PlusPlus => .Add,
        else => @panic("** Compiler Bug ** only prefix operator should be evaluated"),
    };
    const postfix_expr: *Ast.Expr = .postfixExpr(p.arena, postfix_op, expr, postfix_op_token.line, postfix_op_token.start);
    return p.parsePostfixExpr(postfix_expr);
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

        if (next_token.type == .Assign) {
            const assignment_token = try p.consume(.Assign);
            const right = try p.parseExpr(try p.getPrecedence(assignment_token.type));
            const assignment: *Ast.Expr = .assignmentExpr(p.arena, right, left, assignment_token.line, assignment_token.start);
            left = assignment;
        } else if (isCompoundAssignmentOperator(next_token.type)) {
            const op_token = try p.consumeAny();
            const op = try p.parseBinaryOperator(op_token.type);
            const right = try p.parseExpr(try p.getPrecedence(next_token.type));

            // cloning here because on sema phase we replace variable identifier
            // if we use same pointer, it will look for updated name on our map which is wrong
            const left_clone = p.arena.create(Ast.Expr) catch unreachable;
            left_clone.* = left.*;

            const binary: *Ast.Expr = .binaryExpr(p.arena, op, left_clone, right, op_token.line, op_token.start);
            const assignment: *Ast.Expr = .assignmentExpr(p.arena, binary, left, op_token.line, op_token.start);
            left = assignment;
        } else if (next_token.type == .Question) {
            const q_token = try p.consume(.Question);
            const then_block = try p.parseExpr(0);
            _ = try p.consume(.Colon);
            const else_block = try p.parseExpr(try p.getPrecedence(next_token.type));
            const conditional: *Ast.Expr = .ternaryExpr(p.arena, left, then_block, else_block, q_token.line, q_token.start);
            left = conditional;
        } else {
            const op_token = try p.consumeAny();
            const op = try p.parseBinaryOperator(op_token.type);
            const right = try p.parseExpr(try p.getPrecedence(next_token.type) + 1);
            const binary: *Ast.Expr = .binaryExpr(p.arena, op, left, right, op_token.line, op_token.start);
            left = binary;
        }
    }
    return left;
}

fn isBinaryOperator(token_type: TokenType) bool {
    return switch (token_type) {
        .And,
        .Assign,
        .BitAnd,
        .BitAndEqual,
        .BitNot,
        .BitOr,
        .BitOrEqual,
        .BitXor,
        .BitXorEqual,
        .Divide,
        .DivideEqual,
        .EqualEqual,
        .GreaterThan,
        .GreaterThanEqual,
        .LeftShift,
        .LeftShiftEqual,
        .LessThan,
        .LessThanEqual,
        .Minus,
        .MinusEqual,
        .Mod,
        .ModEqual,
        .Multiply,
        .MultiplyEqual,
        .NotEqual,
        .Or,
        .Plus,
        .PlusEqual,
        .RightShift,
        .RightShiftEqual,
        .Question, // hack to get existing system work - maybe ternary is binary :D
        => true,

        .Ident,
        .Int,
        .IntLiteral,
        .LCurly,
        .LParen,
        .MinusMinus,
        .Not,
        .PlusPlus,
        .RCurly,
        .RParen,
        .Return,
        .Semicolon,
        .Void,
        .If,
        .Else,
        .Colon,
        => false,
    };
}

fn isCompoundAssignmentOperator(token_type: TokenType) bool {
    return switch (token_type) {
        .BitAndEqual,
        .BitOrEqual,
        .BitXorEqual,
        .DivideEqual,
        .LeftShiftEqual,
        .MinusEqual,
        .ModEqual,
        .MultiplyEqual,
        .PlusEqual,
        .RightShiftEqual,
        => true,

        .And,
        .Assign,
        .BitAnd,
        .BitNot,
        .BitOr,
        .BitXor,
        .Divide,
        .EqualEqual,
        .GreaterThan,
        .GreaterThanEqual,
        .Ident,
        .Int,
        .IntLiteral,
        .LCurly,
        .LParen,
        .LeftShift,
        .LessThan,
        .LessThanEqual,
        .Minus,
        .MinusMinus,
        .Mod,
        .Multiply,
        .Not,
        .NotEqual,
        .Or,
        .Plus,
        .PlusPlus,
        .RCurly,
        .RParen,
        .Return,
        .RightShift,
        .Semicolon,
        .Void,
        .If,
        .Else,
        .Question,
        .Colon,
        => false,
    };
}

fn parseBinaryOperator(p: Self, token_type: TokenType) ParseError!Ast.BinaryOperator {
    return switch (token_type) {
        .And => .And,
        .BitAndEqual, .BitAnd => .BitAnd,
        .BitOr, .BitOrEqual => .BitOr,
        .BitXor, .BitXorEqual => .BitXor,
        .Divide, .DivideEqual => .Divide,
        .EqualEqual => .EqualEqual,
        .GreaterThan => .GreaterThan,
        .GreaterThanEqual => .GreaterThanEqual,
        .LeftShift, .LeftShiftEqual => .LeftShift,
        .LessThan => .LessThan,
        .LessThanEqual => .LessThanEqual,
        .Minus, .MinusEqual => .Subtract,
        .Mod, .ModEqual => .Mod,
        .Multiply, .MultiplyEqual => .Multiply,
        .NotEqual => .NotEqual,
        .Or => .Or,
        .Plus, .PlusEqual => .Add,
        .RightShift, .RightShiftEqual => .RightShift,

        else => try p.parseError(ParseError.CompilerBug, "** Compiler Bug ** parseBinaryOperator called on non binary token", .{}),
    };
}

fn getPrecedence(p: Self, token_type: TokenType) ParseError!usize {
    return switch (token_type) {
        .BitNot => 70,
        .Divide, .Multiply, .Mod => 50,
        .Minus, .Plus => 45,
        .LeftShift, .RightShift => 40,
        .LessThan, .LessThanEqual, .GreaterThan, .GreaterThanEqual => 35,
        .EqualEqual, .NotEqual => 30,
        .BitAnd => 25,
        .BitXor => 24,
        .BitOr => 23,
        .And => 10,
        .Or => 5,
        .Question => 3,
        .Assign,
        .PlusEqual,
        .MinusEqual,
        .MultiplyEqual,
        .DivideEqual,
        .ModEqual,
        .BitAndEqual,
        .BitOrEqual,
        .BitXorEqual,
        .LeftShiftEqual,
        .RightShiftEqual,
        => 1,

        else => try p.parseError(
            ParseError.CompilerBug,
            "** Compiler Bug ** precedence level asked for something that doesn't support precendence << {s} >>",
            .{@tagName(token_type)},
        ),
    };
}

fn parseUnaryOperator(p: Self, token_type: TokenType) ParseError!Ast.UnaryOperator {
    return switch (token_type) {
        .Minus => .Negate,
        .BitNot => .BitNot,
        .Not => .Not,
        else => try p.parseError(
            ParseError.CompilerBug,
            "** Compiler Bug ** parseUnaryOperator called on non unary token << {s} >>",
            .{@tagName(token_type)},
        ),
    };
}

fn parseError(p: Self, e: ParseError, comptime fmt: []const u8, args: anytype) ParseError!noreturn {
    const token = p.peek() orelse {
        // If we reach here, it means we are at the end of input.
        // We can report the error without a token.
        p.error_reporter.addError(0, 0, fmt, args);
        return e;
    };
    try parseErrorOnToken(p, e, token, fmt, args);
}

fn parseErrorOnToken(p: Self, e: ParseError, token: *const Token, comptime fmt: []const u8, args: anytype) ParseError!noreturn {
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

    pub const Block = struct {
        block_item: ArrayList(*BlockItem),
        loc: SourceLocation,

        pub fn init(allocator: Allocator, block_item: ArrayList(*BlockItem), line: usize, start: usize) *Block {
            const block = allocator.create(Block) catch unreachable;
            block.* = .{
                .block_item = block_item,
                .loc = .{ .line = line, .start = start },
            };
            return block;
        }
    };

    pub const BlockItem = union(enum) {
        Stmt: *Stmt,
        Decl: *Decl,

        pub fn stmt(allocator: Allocator, stmt_item: *Stmt) *BlockItem {
            const item = allocator.create(BlockItem) catch unreachable;
            item.* = .{ .Stmt = stmt_item };
            return item;
        }

        pub fn decl(allocator: Allocator, decl_item: *Decl) *BlockItem {
            const item = allocator.create(BlockItem) catch unreachable;
            item.* = .{ .Decl = decl_item };
            return item;
        }
    };
    pub const Decl = union(enum) {
        Fn: *FnDecl,
        Var: *VarDecl,

        pub fn variableDecl(allocator: Allocator, ident: []const u8, init: ?*Expr, line: usize, start: usize) *Decl {
            const item = allocator.create(Decl) catch unreachable;
            const _var_decl = allocator.create(VarDecl) catch unreachable;
            _var_decl.* = .{ .ident = ident, .init = init, .loc = .{ .line = line, .start = start } };
            item.* = .{ .Var = _var_decl };
            return item;
        }
    };

    pub const VarDecl = struct {
        ident: []const u8,
        init: ?*Expr,
        loc: SourceLocation,
    };

    pub const FnDecl = struct {
        name: []const u8,
        body: *Block,
        loc: SourceLocation,

        pub fn init(allocator: Allocator, name: []const u8, body: *Block, line: usize, start: usize) *FnDecl {
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
        Expr: struct { expr: *Expr, loc: SourceLocation },
        If: struct { condition: *Expr, then: *Stmt, @"else": ?*Stmt, loc: SourceLocation },
        Compound: struct { body: *Block, loc: SourceLocation },
        Null: SourceLocation,

        pub fn compoundStmt(allocator: Allocator, body: *Block, line: usize, start: usize) *Stmt {
            const stmt = allocator.create(Stmt) catch unreachable;
            stmt.* = .{
                .Compound = .{
                    .body = body,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return stmt;
        }

        pub fn ifStmt(allocator: Allocator, condition: *Expr, then: *Stmt, @"else": ?*Stmt, line: usize, start: usize) *Stmt {
            const stmt = allocator.create(Stmt) catch unreachable;
            stmt.* = .{
                .If = .{
                    .condition = condition,
                    .then = then,
                    .@"else" = @"else",
                    .loc = .{ .line = line, .start = start },
                },
            };
            return stmt;
        }

        pub fn nullStmt(allocator: Allocator, line: usize, start: usize) *Stmt {
            const stmt = allocator.create(Stmt) catch unreachable;
            stmt.* = .{ .Null = .{ .line = line, .start = start } };
            return stmt;
        }

        pub fn exprStmt(allocator: Allocator, expr: *Expr, line: usize, start: usize) *Stmt {
            const stmt = allocator.create(Stmt) catch unreachable;
            stmt.* = .{
                .Expr = .{
                    .expr = expr,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return stmt;
        }

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
        Var: struct { ident: []const u8, loc: SourceLocation },
        Unary: struct { operator: UnaryOperator, expr: *Expr, loc: SourceLocation },
        Binary: struct { operator: BinaryOperator, left: *Expr, right: *Expr, loc: SourceLocation },
        Assignment: struct { src: *Expr, dst: *Expr, loc: SourceLocation },
        Group: struct { expr: *Expr, loc: SourceLocation },
        Postfix: struct { operator: PostfixOperator, expr: *Expr, loc: SourceLocation },
        Ternary: struct { condition: *Expr, then: *Expr, @"else": *Expr, loc: SourceLocation },

        pub fn ternaryExpr(allocator: Allocator, condition: *Expr, then: *Expr, @"else": *Expr, line: usize, start: usize) *Expr {
            const expr = allocator.create(Expr) catch unreachable;
            expr.* = .{
                .Ternary = .{
                    .condition = condition,
                    .then = then,
                    .@"else" = @"else",
                    .loc = .{ .line = line, .start = start },
                },
            };
            return expr;
        }

        pub fn postfixExpr(allocator: Allocator, operator: PostfixOperator, expr: *Expr, line: usize, start: usize) *Expr {
            const postfix_expr = allocator.create(Expr) catch unreachable;
            postfix_expr.* = .{
                .Postfix = .{
                    .operator = operator,
                    .expr = expr,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return postfix_expr;
        }

        pub fn groupExpr(allocator: Allocator, expr: *Expr, line: usize, start: usize) *Expr {
            const group_expr = allocator.create(Expr) catch unreachable;
            group_expr.* = .{
                .Group = .{
                    .expr = expr,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return group_expr;
        }

        pub fn assignmentExpr(allocator: Allocator, src: *Expr, dst: *Expr, line: usize, start: usize) *Expr {
            const expr = allocator.create(Expr) catch unreachable;
            expr.* = .{
                .Assignment = .{
                    .src = src,
                    .dst = dst,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return expr;
        }

        pub fn varExpr(allocator: Allocator, ident: []const u8, line: usize, start: usize) *Expr {
            const expr = allocator.create(Expr) catch unreachable;
            expr.* = .{
                .Var = .{
                    .ident = ident,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return expr;
        }

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
    pub const PostfixOperator = enum { Add, Subtract };
    const UnaryOperator = enum {
        Negate,
        BitNot,
        Not,
    };
    const BinaryOperator = enum {
        Add,
        And,
        BitAnd,
        BitOr,
        BitXor,
        Divide,
        EqualEqual,
        GreaterThan,
        GreaterThanEqual,
        LeftShift,
        LessThan,
        LessThanEqual,
        Mod,
        Multiply,
        NotEqual,
        Or,
        RightShift,
        Subtract,
    };

    const SourceLocation = struct {
        line: usize,
        start: usize,
    };
};

const ParseError = error{
    InvalidLValue,
    UnexpectedToken,
    StatementExpected,
    IntExpected,
    UnexpectedEndOfToken,
    MissingToken,
    InvalidIdent,
    InvalidPrefixOperand,
} || CompilerError;

pub const AstPrinter = struct {
    pub fn print(writer: AnyWriter, pg: *const Ast.Program) void {
        write(writer, "-- AST --\n");
        write(writer, "program\n");
        printFnDecl(writer, pg.@"fn", 1);
        write(writer, "\n");
    }

    fn printFnDecl(writer: AnyWriter, fn_decl: *const Ast.FnDecl, depth: usize) void {
        printSpace(writer, depth);
        writeFmt(writer, "int {s}(void)\n", .{fn_decl.name});
        printSpace(writer, depth);
        write(writer, "{\n");

        printBlock(writer, fn_decl.body, depth + 1);

        printSpace(writer, depth);
        write(writer, "}");
    }

    fn printBlock(writer: AnyWriter, block: *const Ast.Block, depth: usize) void {
        for (block.block_item.items) |item| {
            printBlockItem(writer, item, depth);
        }
    }

    fn printBlockItem(writer: AnyWriter, block_item: *const Ast.BlockItem, depth: usize) void {
        switch (block_item.*) {
            .Stmt => |stmt| printStmt(writer, stmt, depth),
            .Decl => |decl| printDecl(writer, decl, depth),
        }
    }

    fn printDecl(writer: AnyWriter, decl: *const Ast.Decl, depth: usize) void {
        switch (decl.*) {
            .Fn => |fn_decl| printFnDecl(writer, fn_decl, depth),
            .Var => |var_decl| printVarDecl(writer, var_decl, depth),
        }
    }

    fn printVarDecl(writer: AnyWriter, var_decl: *const Ast.VarDecl, depth: usize) void {
        printSpace(writer, depth);
        write(writer, "int ");
        writeFmt(writer, "{s}", .{var_decl.ident});
        if (var_decl.init) |initializer| {
            write(writer, " = ");
            printExpr(writer, initializer);
        }
        write(writer, ";\n");
    }

    fn printIfStmt(writer: AnyWriter, stmt: *const Ast.Stmt, depth: usize) void {
        const if_stmt = switch (stmt.*) {
            .If => |if_stmt| if_stmt,
            else => @panic("** Compiler Bug ** printIfStmt called on non If statement"),
        };
        write(writer, "if (");
        printExpr(writer, if_stmt.condition);
        write(writer, ")\n");
        printStmt(writer, if_stmt.then, depth);
        if (if_stmt.@"else") |else_stmt| {
            printSpace(writer, depth);
            write(writer, "else");

            if (else_stmt.* == .If) {
                write(writer, " ");
                printIfStmt(writer, else_stmt, depth);
            } else {
                write(writer, "\n");
                printStmt(writer, else_stmt, depth);
            }
        }
    }

    fn printStmt(writer: AnyWriter, stmt: *const Ast.Stmt, depth: usize) void {
        printSpace(writer, depth);
        switch (stmt.*) {
            .If => {
                printIfStmt(writer, stmt, depth);
            },
            .Compound => |compound_stmt| {
                write(writer, "{\n");
                printSpace(writer, depth);
                printBlock(writer, compound_stmt.body, depth);
                printSpace(writer, depth);
                write(writer, "}\n");
            },
            .Return => |return_stmt| {
                write(writer, "return");
                write(writer, " ");
                printExpr(writer, return_stmt.expr);
                write(writer, ";\n");
            },
            .Expr => |expr| {
                printExpr(writer, expr.expr);
                write(writer, ";\n");
            },
            .Null => write(writer, ";\n"),
        }
    }
    fn printExpr(writer: AnyWriter, expr: *const Ast.Expr) void {
        switch (expr.*) {
            .Ternary => |ternary_expr| {
                printExpr(writer, ternary_expr.condition);
                write(writer, " ? ");
                printExpr(writer, ternary_expr.then);
                write(writer, " : ");
                printExpr(writer, ternary_expr.@"else");
            },
            .Var => |var_expr| {
                writeFmt(writer, "{s}", .{var_expr.ident});
            },
            .Postfix => |postfix_expr| {
                printExpr(writer, postfix_expr.expr);
                switch (postfix_expr.operator) {
                    .Add => write(writer, "++"),
                    .Subtract => write(writer, "--"),
                }
            },
            .Group => |group_expr| {
                write(writer, "(");
                printExpr(writer, group_expr.expr);
                write(writer, ")");
            },
            .Assignment => |assignment_expr| {
                printExpr(writer, assignment_expr.dst);
                write(writer, " = ");
                printExpr(writer, assignment_expr.src);
            },
            .Constant => |constant_expr| {
                writeFmt(writer, "{d}", .{constant_expr.value});
            },
            .Unary => |unary_expr| {
                write(writer, "(");
                writeFmt(writer, "{s}", .{
                    switch (unary_expr.operator) {
                        .Negate => "-",
                        .BitNot => "~",
                        .Not => "!",
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
                        .And => "&&",
                        .Or => "||",
                        .EqualEqual => "==",
                        .NotEqual => "!=",
                        .LessThan => "<",
                        .LessThanEqual => "<=",
                        .GreaterThan => ">",
                        .GreaterThanEqual => ">=",
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

pub fn recurseGetGroupInnerExpr(expr: *Ast.Expr) *Ast.Expr {
    if (expr.* != .Group) return expr;
    return recurseGetGroupInnerExpr(expr.Group.expr);
}

const std = @import("std");
const ErrorReporter = @import("ErrorReporter.zig");
const Lexer = @import("Lexer.zig");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const TokenType = Lexer.TokenType;
const Token = Lexer.Token;
const AnyWriter = std.io.AnyWriter;
const CompilerError = @import("util.zig").CompilerError;
