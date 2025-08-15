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
        const last_token = self.peek();
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
    var fns = ArrayList(*Ast.FnDecl).init(p.arena);
    while (!p.isAtEnd()) {
        const fn_decl: *Ast.FnDecl = try p.parseFnDecl(.{});
        fns.append(fn_decl);
    }
    return .init(p.arena, fns);
}

const ParseFnDecl = struct {
    only_decl: bool = false,
};

fn parseFnDecl(p: *Self, opt: ParseFnDecl) ParseError!*Ast.FnDecl {
    _ = try p.consume(.Int);
    const ident = try p.consume(.Ident);

    const params = try p.parseFnParams();

    if (p.peek().type == .Semicolon) {
        _ = try p.consume(.Semicolon);
        return .init(p.arena, ident.lexeme, params, null, ident.line, ident.start);
    }
    if (opt.only_decl) {
        try p.parseError(
            ParseError.ExpectedOnlyFnDefinition,
            "Expected only function declaration, found function definition",
            .{},
        );
    }
    const body = try p.parseBlock();
    return .init(p.arena, ident.lexeme, params, body, ident.line, ident.start);
}

fn parseBlock(p: *Self) ParseError!*Ast.Block {
    const start_token = try p.consume(.LCurly);
    var body = ArrayList(*Ast.BlockItem).init(p.arena);

    while (p.peek().type != .RCurly) {
        const item = try p.parseBlockItem();
        body.append(item);
    }
    _ = try p.consume(.RCurly);
    return .init(p.arena, body, start_token.line, start_token.start);
}

fn parseBlockItem(p: *Self) ParseError!*Ast.BlockItem {
    const peek_token = p.peek();
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
    const token = p.peek();
    // We only support `int` at the moment
    if (token.type != .Int) {
        try p.parseError(ParseError.UnexpectedToken, "expected int found `{s}` << {s} >> ", .{ token.lexeme, @tagName(token.type) });
    }

    if (p.isFnDecl()) {
        const fn_decl = try p.parseFnDecl(.{ .only_decl = true });
        return .fnDecl(p.arena, fn_decl);
    }

    _ = try p.consume(.Int);
    const ident_token = try p.consume(.Ident);
    const ident = ident_token.lexeme;

    if (std.ascii.isDigit(ident[0])) {
        try p.parseError(ParseError.InvalidIdent, "identifier should not start with digit. Found `{s}` << {s} >>", .{ ident, @tagName(ident_token.type) });
    }

    var var_initializer: ?*Ast.Expr = null;
    if (p.peek().type == .Assign) {
        _ = try p.consumeAny();
        var_initializer = try p.parseExpr(0);
    }
    _ = try p.consume(.Semicolon);
    return .variableDecl(p.arena, ident, var_initializer, ident_token.line, ident_token.start);
}

fn isFnDecl(p: Self) bool {
    return p.peek().type == .Int and p.peekOffset(1).type == .Ident and p.peekOffset(2).type == .LParen;
}

fn parseFnParams(p: *Self) ParseError!ArrayList(*Ast.FnParam) {
    _ = try p.consume(.LParen);
    var params = ArrayList(*Ast.FnParam).init(p.arena);

    if (p.peek().type == .Void) {
        _ = try p.consume(.Void);
        _ = try p.consume(.RParen);
        return params;
    }

    while (p.peek().type != .RParen) {
        // we only support int type for now
        _ = try p.consume(.Int);
        const ident = try p.consume(.Ident);

        const param: *Ast.FnParam = .fnParam(p.arena, ident.lexeme, ident.line, ident.start);
        params.append(param);

        // maybe we need to break if we can't find comma?
        if (p.peek().type == .Comma) {
            _ = try p.consume(.Comma);
            if (p.peek().type == .RParen) {
                try p.parseError(
                    ParseError.UnexpectedToken,
                    "trailing comma in parameter list is not allowed",
                    .{},
                );
            }
        }
    }
    _ = try p.consume(.RParen);
    return params;
}

fn parseStmt(p: *Self) ParseError!*Ast.Stmt {
    const token = p.peek();
    return switch (token.type) {
        .Return => try p.parseReturnStmt(),
        .If => try p.parseIfStmt(),
        .LCurly => {
            const body = try p.parseBlock();
            return .compoundStmt(p.arena, body, token.line, token.start);
        },
        .Goto => {
            _ = try p.consumeAny();
            const goto_ident = try p.consume(.Ident);
            _ = try p.consume(.Semicolon);
            return .gotoStmt(p.arena, goto_ident.lexeme, token.line, token.start);
        },
        .Break => {
            _ = try p.consume(.Break);
            const break_stmt: *Ast.Stmt = .breakStmt(p.arena, null, token.line, token.start);
            _ = try p.consume(.Semicolon);
            return break_stmt;
        },
        .Continue => {
            _ = try p.consume(.Continue);
            _ = try p.consume(.Semicolon);
            return .continueStmt(p.arena, null, token.line, token.start);
        },
        .Default => {
            _ = try p.consume(.Default);
            _ = try p.consume(.Colon);
            return .defaultStmt(p.arena, token.line, token.start);
        },
        .Do => try p.parseDoWhileStmt(),
        .While => try p.parseWhileStmt(),
        .For => try p.parseForStmt(),
        .Semicolon => {
            _ = try p.consume(.Semicolon);
            return .nullStmt(p.arena, token.line, token.start);
        },
        .Case => {
            _ = try p.consume(.Case);
            const case_value_token = try p.consume(.IntLiteral);
            _ = try p.consume(.Colon);
            return .caseStmt(p.arena, case_value_token.lexeme, case_value_token.line, case_value_token.start);
        },
        .Switch => try p.parseSwitchStmt(),
        else => {
            const peeked_token = p.peek();
            const next_token = p.peekOffset(1);
            if (peeked_token.type == .Ident and next_token.type == .Colon) {
                // .Label
                const ident_token = try p.consume(.Ident);
                _ = try p.consume(.Colon);
                const label_stmt = try p.parseStmt();
                return .labelStmt(p.arena, ident_token.lexeme, label_stmt, ident_token.line, ident_token.start);
            }

            const expr = try p.parseExpr(0);
            _ = try p.consume(.Semicolon);
            return .exprStmt(p.arena, expr, peeked_token.line, peeked_token.start);
        },
    };
}

fn parseSwitchStmt(p: *Self) ParseError!*Ast.Stmt {
    const switch_token = try p.consume(.Switch);
    _ = try p.consume(.LParen);

    const condition = try p.parseExpr(0);
    _ = try p.consume(.RParen);

    var body = ArrayList(*Ast.BlockItem).init(p.arena);

    if (p.peek().type == .LCurly) {
        _ = try p.consume(.LCurly);

        var case_found = false;
        while (p.peek().type != .RCurly) {
            const peeked_token = p.peek();
            const item = try p.parseBlockItem();
            if (item.* == .Stmt and (item.Stmt.* == .Case or item.Stmt.* == .Default)) {
                case_found = true;
            }
            if (case_found and item.* == .Decl) {
                try p.parseErrorOnToken(
                    ParseError.UnexpectedToken,
                    peeked_token,
                    "switch case should not have decl. found",
                    .{},
                );
            }
            body.append(item);
        }
        _ = try p.consume(.RCurly);
        return .switchStmt(p.arena, condition, body, switch_token.line, switch_token.start);
    }
    const block_item = try p.parseBlockItem();
    body.append(block_item);

    if (block_item.* == .Stmt and block_item.Stmt.* == .Case) {
        const case_stmt = try p.parseBlockItem();
        body.append(case_stmt);
    }
    return .switchStmt(p.arena, condition, body, switch_token.line, switch_token.start);
}

fn parseForStmt(p: *Self) ParseError!*Ast.Stmt {
    const for_token = try p.consume(.For);

    _ = try p.consume(.LParen);
    const for_init = try p.parseForInit();
    if (for_init.* == .Expr) _ = try p.consume(.Semicolon);
    const condition = try p.parseOptionalExpr(0);
    _ = try p.consume(.Semicolon);
    const post_expr: ?*Ast.Expr = if (p.peek().type == .RParen) null else try p.parseExpr(0);
    _ = try p.consume(.RParen);

    const body = try p.parseStmt();

    return .forStmt(p.arena, for_init, condition, post_expr, body, null, for_token.line, for_token.start);
}

fn parseForInit(p: *Self) ParseError!*Ast.ForInit {
    const peeked_token = p.peek();
    if (peeked_token.type == .Int) {
        const decl = try p.parseDecl();
        if (decl.* != .Var) {
            try p.parseError(
                ParseError.UnexpectedToken,
                "Expected variable declaration in for loop init, found `{s}` << {any} >>",
                .{ @tagName(decl.*), decl.* },
            );
        }
        return .decl(p.arena, decl);
    }
    const expr: ?*Ast.Expr = try p.parseOptionalExpr(0);
    return .expr(p.arena, expr);
}

fn parseOptionalExpr(p: *Self, min_prec: usize) ParseError!?*Ast.Expr {
    if (p.peek().type == .Semicolon) return null;
    return p.parseExpr(min_prec);
}

fn parseWhileStmt(p: *Self) ParseError!*Ast.Stmt {
    const while_token = try p.consume(.While);

    _ = try p.consume(.LParen);
    const condition = try p.parseExpr(0);
    _ = try p.consume(.RParen);

    const body = try p.parseStmt();
    return .whileStmt(p.arena, condition, body, null, while_token.line, while_token.start);
}

fn parseDoWhileStmt(p: *Self) ParseError!*Ast.Stmt {
    const do_token = try p.consume(.Do);
    const body = try p.parseStmt();

    _ = try p.consume(.While);

    _ = try p.consume(.LParen);
    const condition = try p.parseExpr(0);
    _ = try p.consume(.RParen);

    _ = try p.consume(.Semicolon);

    return .doWhileStmt(p.arena, body, condition, null, do_token.line, do_token.start);
}

fn parseIfStmt(p: *Self) ParseError!*Ast.Stmt {
    const if_token = try p.consume(.If);
    _ = try p.consume(.LParen);
    const condition = try p.parseExpr(0);
    _ = try p.consume(.RParen);

    const then_block = try p.parseStmt();

    var else_block: ?*Ast.Stmt = null;
    if (p.peek().type == .Else) {
        _ = try p.consume(.Else);
        else_block = try p.parseStmt();
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
    const token = p.peek();
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
        // @todo - Maybe this should be handled like postfix
        // With this rewrite here, I am losing that I have prefix incr/decr operator
        // Works but code format/print doesn't look exactly same
        .MinusMinus, .PlusPlus => {
            const op_token = try p.consumeAny();
            const inner_expr = try p.parseFactor();
            const prefix_op: Ast.PrefixOperator = switch (op_token.type) {
                .MinusMinus => .Subtract,
                .PlusPlus => .Add,
                else => std.debug.panic(
                    "** Compiler Bug ** only prefix operator should be evaluated, found `{s}` << {s} >>",
                    .{ op_token.lexeme, @tagName(op_token.type) },
                ),
            };
            return .prefixExpr(p.arena, prefix_op, inner_expr, op_token.line, op_token.start);
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
            const expr = blk: {
                if (p.peek().type != .LParen) {
                    const var_expr: *Ast.Expr = .varExpr(p.arena, ident_token.lexeme, ident_token.line, ident_token.start);
                    break :blk var_expr;
                }
                const args = try p.parseFnArgs();
                break :blk Ast.Expr.fnCallExpr(p.arena, ident_token.lexeme, args, ident_token.line, ident_token.start);
            };

            return p.parsePostfixExpr(expr);
        },
        else => try p.parseError(
            ParseError.UnexpectedToken,
            "Unexpected token while parsing factor`{s}` << {s} >>",
            .{ token.lexeme, @tagName(token.type) },
        ),
    }
}

fn parseFnArgs(p: *Self) ParseError!ArrayList(*Ast.Expr) {
    var args = ArrayList(*Ast.Expr).init(p.arena);
    _ = try p.consume(.LParen);
    while (p.peek().type != .RParen) {
        const expr = try p.parseExpr(0);
        args.append(expr);
        if (p.peek().type == .Comma) {
            _ = try p.consume(.Comma);
            if (p.peek().type == .RParen) {
                try p.parseError(ParseError.TrainingComma, "trailing comma in function call is not allowed", .{});
            }
        }
    }
    _ = try p.consume(.RParen);
    return args;
}

fn parsePostfixExpr(p: *Self, expr: *Ast.Expr) ParseError!*Ast.Expr {
    const peeked_token = p.peek();

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
        else => std.debug.panic(
            "** Compiler Bug ** only prefix operator should be evaluated. found `{s}` << {s} >>",
            .{ postfix_op_token.lexeme, @tagName(postfix_op_token.type) },
        ),
    };
    const postfix_expr: *Ast.Expr = .postfixExpr(p.arena, postfix_op, expr, postfix_op_token.line, postfix_op_token.start);
    return p.parsePostfixExpr(postfix_expr);
}

fn parseExpr(p: *Self, min_prec: usize) ParseError!*Ast.Expr {
    var left = try p.parseFactor();
    var peeked_token = p.peek();

    while (isBinaryOperator(peeked_token) and getPrecedence(peeked_token) >= min_prec) {
        if (peeked_token.type == .Assign) {
            const assignment_token = try p.consume(.Assign);
            const right = try p.parseExpr(getPrecedence(assignment_token));
            const assignment: *Ast.Expr = .assignmentExpr(p.arena, right, left, assignment_token.line, assignment_token.start);
            left = assignment;
        } else if (isCompoundAssignmentOperator(peeked_token)) {
            const op_token = try p.consumeAny();
            const op = try p.parseBinaryOperator(op_token.type);
            const right = try p.parseExpr(getPrecedence(peeked_token));

            // cloning here because on sema phase we replace variable identifier
            // if we use same pointer, it will look for updated name on our map which is wrong
            const left_clone = p.arena.create(Ast.Expr) catch unreachable;
            left_clone.* = left.*;

            const binary: *Ast.Expr = .binaryExpr(p.arena, op, left_clone, right, op_token.line, op_token.start);
            const assignment: *Ast.Expr = .assignmentExpr(p.arena, binary, left, op_token.line, op_token.start);
            left = assignment;
        } else if (peeked_token.type == .Question) {
            const q_token = try p.consume(.Question);
            const then_block = try p.parseExpr(0);
            _ = try p.consume(.Colon);
            const else_block = try p.parseExpr(getPrecedence(peeked_token));
            const conditional: *Ast.Expr = .ternaryExpr(p.arena, left, then_block, else_block, q_token.line, q_token.start);
            left = conditional;
        } else {
            const op_token = try p.consumeAny();
            const op = try p.parseBinaryOperator(op_token.type);
            const right = try p.parseExpr(getPrecedence(peeked_token) + 1);
            const binary: *Ast.Expr = .binaryExpr(p.arena, op, left, right, op_token.line, op_token.start);
            left = binary;
        }
        peeked_token = p.peek();
    }
    return left;
}

fn isBinaryOperator(token: *const Token) bool {
    return switch (token.type) {
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

        .Default,
        .Eof,
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
        .Goto,
        .Do,
        .While,
        .For,
        .Break,
        .Continue,
        .Switch,
        .Case,
        .Comma,
        => false,
    };
}

fn isCompoundAssignmentOperator(token: *const Token) bool {
    return switch (token.type) {
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

        .Comma,
        .Default,
        .Eof,
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
        .Goto,
        .Do,
        .While,
        .For,
        .Break,
        .Continue,
        .Switch,
        .Case,
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

fn getPrecedence(token: *const Token) usize {
    return switch (token.type) {
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

        else => std.debug.panic(
            "** Compiler Bug ** precedence level asked for something that doesn't support precendence `{s}` << {s} >>",
            .{ token.lexeme, @tagName(token.type) },
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
    const token = p.peek();
    try parseErrorOnToken(p, e, token, fmt, args);
}

fn parseErrorOnToken(p: Self, e: ParseError, token: *const Token, comptime fmt: []const u8, args: anytype) ParseError!noreturn {
    p.error_reporter.addError(token.line, token.start, fmt, args);
    return e;
}

fn consumeAny(p: *Self) ParseError!*const Token {
    defer p.current += 1;
    const token = p.peek();
    if (token.type == .Eof) {
        try p.parseError(
            ParseError.UnexpectedToken,
            "Unexpected end of input while consuming any token",
            .{},
        );
    }
    return token;
}

fn consume(p: *Self, expected: TokenType) ParseError!*const Token {
    const tok = p.peek();
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

const EOF_TOKEN: Token = .{
    .type = .Eof,
    .lexeme = "",
    .line = 0,
    .start = 0,
};

fn peek(p: Self) *const Token {
    return p.peekOffset(0);
}

fn peekOffset(p: Self, offset: i8) *const Token {
    if (p.isAtEnd()) return &EOF_TOKEN;
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
        fns: ArrayList(*FnDecl),

        pub fn init(allocator: Allocator, fns: ArrayList(*FnDecl)) *Program {
            const pg = allocator.create(Program) catch unreachable;
            pg.* = .{ .fns = fns };
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

        pub fn fnDecl(allocator: Allocator, fn_decl: *FnDecl) *Decl {
            const item = allocator.create(Decl) catch unreachable;
            item.* = .{ .Fn = fn_decl };
            return item;
        }

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
        params: ArrayList(*FnParam),
        body: ?*Block,
        loc: SourceLocation,

        pub fn init(allocator: Allocator, name: []const u8, params: ArrayList(*FnParam), body: ?*Block, line: usize, start: usize) *FnDecl {
            const fn_decl = allocator.create(FnDecl) catch unreachable;
            fn_decl.* = .{
                .name = name,
                .params = params,
                .body = body,
                .loc = .{ .line = line, .start = start },
            };
            return fn_decl;
        }
    };

    pub const FnParam = struct {
        ident: []const u8,
        loc: SourceLocation,

        pub fn fnParam(allocator: Allocator, ident: []const u8, line: usize, start: usize) *FnParam {
            const param = allocator.create(FnParam) catch unreachable;
            param.* = .{
                .ident = ident,
                .loc = .{ .line = line, .start = start },
            };
            return param;
        }
    };

    // @note - all type need SourceLocation, instead of tagged union should I have use normal union type?
    // That way I can easily get source location in places where I need to display error with correct line on guard clause
    pub const Stmt = union(enum) {
        Return: struct { expr: *Expr, loc: SourceLocation },
        Expr: struct { expr: *Expr, loc: SourceLocation },
        If: struct { condition: *Expr, then: *Stmt, @"else": ?*Stmt, loc: SourceLocation },
        Compound: struct { body: *Block, loc: SourceLocation },
        Goto: struct { ident: []const u8, loc: SourceLocation },
        Label: struct { ident: []const u8, stmt: *Stmt, loc: SourceLocation },
        Break: struct { ident: ?[]const u8, loc: SourceLocation },
        Continue: struct { ident: ?[]const u8, loc: SourceLocation },
        DoWhile: struct { body: *Stmt, condition: *Expr, label: ?[]const u8, loc: SourceLocation },
        While: struct { condition: *Expr, body: *Stmt, label: ?[]const u8, loc: SourceLocation },
        For: struct { init: *ForInit, condition: ?*Expr, post: ?*Expr, body: *Stmt, label: ?[]const u8, loc: SourceLocation },
        Case: struct { value: []const u8, label: ?[]const u8, loc: SourceLocation },
        Switch: SwitchStmt,
        Default: struct { label: ?[]const u8, loc: SourceLocation },
        Null: SourceLocation,

        pub fn defaultStmt(allocator: Allocator, line: usize, start: usize) *Stmt {
            const stmt = allocator.create(Stmt) catch unreachable;
            stmt.* = .{
                .Default = .{ .label = null, .loc = .{ .line = line, .start = start } },
            };
            return stmt;
        }

        pub fn caseStmt(allocator: Allocator, value: []const u8, line: usize, start: usize) *Stmt {
            const stmt = allocator.create(Stmt) catch unreachable;
            stmt.* = .{
                .Case = .{
                    .value = value,
                    .label = null,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return stmt;
        }

        pub fn switchStmt(allocator: Allocator, condition: *Expr, body: ArrayList(*BlockItem), line: usize, start: usize) *Stmt {
            const stmt = allocator.create(Stmt) catch unreachable;
            stmt.* = .{
                .Switch = .{
                    .condition = condition,
                    .case_labels = null,
                    .body = body,
                    .label = null,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return stmt;
        }

        pub fn forStmt(allocator: Allocator, init: *ForInit, condition: ?*Expr, post: ?*Expr, body: *Stmt, label: ?[]const u8, line: usize, start: usize) *Stmt {
            const stmt = allocator.create(Stmt) catch unreachable;
            stmt.* = .{
                .For = .{
                    .init = init,
                    .condition = condition,
                    .post = post,
                    .body = body,
                    .label = label,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return stmt;
        }

        pub fn whileStmt(allocator: Allocator, condition: *Expr, body: *Stmt, label: ?[]const u8, line: usize, start: usize) *Stmt {
            const stmt = allocator.create(Stmt) catch unreachable;
            stmt.* = .{
                .While = .{
                    .condition = condition,
                    .body = body,
                    .label = label,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return stmt;
        }

        pub fn doWhileStmt(allocator: Allocator, body: *Stmt, condition: *Expr, label: ?[]const u8, line: usize, start: usize) *Stmt {
            const stmt = allocator.create(Stmt) catch unreachable;
            stmt.* = .{
                .DoWhile = .{
                    .body = body,
                    .condition = condition,
                    .label = label,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return stmt;
        }

        pub fn continueStmt(allocator: Allocator, ident: ?[]const u8, line: usize, start: usize) *Stmt {
            const stmt = allocator.create(Stmt) catch unreachable;
            stmt.* = .{
                .Continue = .{
                    .ident = ident,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return stmt;
        }

        pub fn breakStmt(allocator: Allocator, ident: ?[]const u8, line: usize, start: usize) *Stmt {
            const stmt = allocator.create(Stmt) catch unreachable;
            stmt.* = .{
                .Break = .{
                    .ident = ident,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return stmt;
        }

        pub fn labelStmt(allocator: Allocator, ident: []const u8, stmt: *Stmt, line: usize, start: usize) *Stmt {
            const label_stmt = allocator.create(Stmt) catch unreachable;
            label_stmt.* = .{
                .Label = .{
                    .ident = ident,
                    .stmt = stmt,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return label_stmt;
        }

        pub fn gotoStmt(allocator: Allocator, ident: []const u8, line: usize, start: usize) *Stmt {
            const stmt = allocator.create(Stmt) catch unreachable;
            stmt.* = .{
                .Goto = .{
                    .ident = ident,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return stmt;
        }

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

    pub const ForInit = union(enum) {
        Decl: *Decl,
        Expr: ?*Expr,

        pub fn decl(allocator: Allocator, decl_initializer: *Decl) *ForInit {
            const init = allocator.create(ForInit) catch unreachable;
            init.* = .{ .Decl = decl_initializer };
            return init;
        }
        pub fn expr(allocator: Allocator, expr_initializer: ?*Expr) *ForInit {
            const init = allocator.create(ForInit) catch unreachable;
            init.* = .{ .Expr = expr_initializer };
            return init;
        }
    };

    pub const SwitchStmt = struct {
        condition: *Expr,
        body: ArrayList(*BlockItem),
        label: ?[]const u8,
        // labels for all case statements - processed on sema - used on tacky IR
        case_labels: ?*ArrayList(CaseLabel),
        loc: SourceLocation,
    };

    pub const CaseLabel = struct {
        label: []const u8,
        value: []const u8,
        is_default: bool,
    };

    pub const Expr = union(enum) {
        Constant: struct { value: i64, loc: SourceLocation },
        Var: struct { ident: []const u8, loc: SourceLocation },
        Unary: struct { operator: UnaryOperator, expr: *Expr, loc: SourceLocation },
        Binary: struct { operator: BinaryOperator, left: *Expr, right: *Expr, loc: SourceLocation },
        Assignment: struct { src: *Expr, dst: *Expr, loc: SourceLocation },
        Group: struct { expr: *Expr, loc: SourceLocation },
        Prefix: struct { operator: PrefixOperator, expr: *Expr, loc: SourceLocation },
        Postfix: struct { operator: PostfixOperator, expr: *Expr, loc: SourceLocation },
        Ternary: struct { condition: *Expr, then: *Expr, @"else": *Expr, loc: SourceLocation },
        FnCall: struct { ident: []const u8, args: ArrayList(*Expr), loc: SourceLocation },

        pub fn fnCallExpr(allocator: Allocator, ident: []const u8, args: ArrayList(*Expr), line: usize, start: usize) *Expr {
            const expr = allocator.create(Expr) catch unreachable;
            expr.* = .{
                .FnCall = .{
                    .ident = ident,
                    .args = args,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return expr;
        }

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

        pub fn prefixExpr(allocator: Allocator, operator: PrefixOperator, expr: *Expr, line: usize, start: usize) *Expr {
            const prefix_expr = allocator.create(Expr) catch unreachable;
            prefix_expr.* = .{
                .Prefix = .{
                    .operator = operator,
                    .expr = expr,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return prefix_expr;
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
    pub const PrefixOperator = enum { Add, Subtract };
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
    TrainingComma,
    ExpectedOnlyFnDefinition,
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
        for (pg.fns.items) |fn_decl| {
            printFnDecl(writer, fn_decl, 1);
            write(writer, "\n");
        }
        write(writer, "\n");
    }

    fn printFnDecl(writer: AnyWriter, fn_decl: *const Ast.FnDecl, depth: usize) void {
        if (fn_decl.body != null) printSpace(writer, depth);
        writeFmt(writer, "int {s}( ", .{fn_decl.name});
        for (fn_decl.params.items, 0..) |param, i| {
            if (fn_decl.body != null) write(writer, "int ");
            writeFmt(writer, "{s}", .{param.ident});
            if (i < fn_decl.params.items.len - 1) {
                write(writer, ", ");
            }
        }
        write(writer, " )");

        if (fn_decl.body) |body| {
            write(writer, "\n");
            printSpace(writer, depth);
            write(writer, "{\n");
            printBlock(writer, body, depth + 1, true);
            printSpace(writer, depth);
            write(writer, "}");
        } else {
            write(writer, ";\n");
        }
    }

    fn printBlock(writer: AnyWriter, block: *const Ast.Block, depth: usize, new_line: bool) void {
        for (block.block_item.items) |item| {
            printSpace(writer, depth);
            printBlockItem(writer, item, depth, new_line);
        }
    }

    fn printBlockItem(writer: AnyWriter, block_item: *const Ast.BlockItem, depth: usize, new_line: bool) void {
        switch (block_item.*) {
            .Stmt => |stmt| printStmt(writer, stmt, depth),
            .Decl => |decl| printDecl(writer, decl, depth, new_line),
        }
    }

    fn printDecl(writer: AnyWriter, decl: *const Ast.Decl, depth: usize, new_line: bool) void {
        switch (decl.*) {
            .Fn => |fn_decl| printFnDecl(writer, fn_decl, depth),
            .Var => |var_decl| printVarDecl(writer, var_decl, depth, new_line),
        }
    }

    fn printVarDecl(writer: AnyWriter, var_decl: *const Ast.VarDecl, depth: usize, new_line: bool) void {
        _ = depth;
        write(writer, "int ");
        writeFmt(writer, "{s}", .{var_decl.ident});
        if (var_decl.init) |initializer| {
            write(writer, " = ");
            printExpr(writer, initializer);
        }
        if (new_line) write(writer, ";\n");
    }

    fn printIfStmt(writer: AnyWriter, stmt: *const Ast.Stmt, depth: usize) void {
        const if_stmt = switch (stmt.*) {
            .If => |if_stmt| if_stmt,
            else => std.debug.panic(
                "** Compiler Bug ** printIfStmt called on non If statement",
                .{},
            ),
        };
        write(writer, "if (");
        printExpr(writer, if_stmt.condition);
        write(writer, ")\n");

        const if_depth = if (if_stmt.then.* == .Compound) depth else depth + 1;
        printSpace(writer, if_depth);
        printStmt(writer, if_stmt.then, if_depth);
        if (if_stmt.@"else") |else_stmt| {
            if (if_stmt.then.* == .Compound) {
                write(writer, "\n");
            }
            printSpace(writer, depth);
            write(writer, "else");
            if (else_stmt.* == .If) {
                write(writer, " ");
                printIfStmt(writer, else_stmt, depth);
            } else {
                write(writer, "\n");
                const else_depth = if (if_stmt.then.* == .Compound) depth else depth + 1;
                printSpace(writer, else_depth);
                printStmt(writer, else_stmt, else_depth);
            }
        }
        write(writer, "\n");
    }

    fn printDoWhileStmt(writer: AnyWriter, stmt: *const Ast.Stmt, depth: usize) void {
        const do_stmt = switch (stmt.*) {
            .DoWhile => |do_while| do_while,
            else => std.debug.panic(
                "** Compiler Bug ** printDoWhileStmt called on non doWhile statement",
                .{},
            ),
        };
        write(writer, "do\n");
        printSpace(writer, depth);
        const do_depth = if (do_stmt.body.* == .Compound) depth else depth + 1;
        printStmt(writer, do_stmt.body, do_depth);
        write(writer, " while ( ");
        printExpr(writer, do_stmt.condition);
        write(writer, " );\n");
    }

    fn printWhileStmt(writer: AnyWriter, stmt: *const Ast.Stmt, depth: usize) void {
        const while_stmt = switch (stmt.*) {
            .While => |while_stmt_| while_stmt_,
            else => std.debug.panic(
                "** Compiler Bug ** whileStmt called on non while statement",
                .{},
            ),
        };
        write(writer, "while ( ");
        printExpr(writer, while_stmt.condition);
        write(writer, " )\n");

        const while_depth = if (while_stmt.body.* == .Compound) depth else depth + 1;

        printSpace(writer, depth);
        printStmt(writer, while_stmt.body, while_depth);
        write(writer, "\n");
    }

    fn printForStmt(writer: AnyWriter, stmt: *const Ast.Stmt, depth: usize) void {
        const for_stmt = switch (stmt.*) {
            .For => |for_stmt_| for_stmt_,
            else => std.debug.panic(
                "** Compiler Bug ** printForStmt called on non for statement",
                .{},
            ),
        };
        write(writer, "for (");
        switch (for_stmt.init.*) {
            .Decl => |decl| printDecl(writer, decl, depth + 1, false),
            .Expr => |expr| if (expr) |initializer| printExpr(writer, initializer),
        }
        write(writer, " ; ");

        if (for_stmt.condition) |condition| {
            printExpr(writer, condition);
        }
        write(writer, " ; ");

        if (for_stmt.post) |post| {
            printExpr(writer, post);
        }

        write(writer, " )\n");

        printSpace(writer, depth);
        const for_depth = if (for_stmt.body.* == .Compound) depth else depth + 1;
        printStmt(writer, for_stmt.body, for_depth);
        write(writer, "\n");
    }

    fn printStmt(writer: AnyWriter, stmt: *const Ast.Stmt, depth: usize) void {
        switch (stmt.*) {
            .Switch => |switch_stmt| {
                write(writer, "switch (");
                printExpr(writer, switch_stmt.condition);
                write(writer, ")\n");
                printSpace(writer, depth);
                write(writer, "{\n");
                for (switch_stmt.body.items) |item| {
                    if (item.* == .Stmt and item.Stmt.* == .Case) {
                        printSpace(writer, depth + 1);
                    } else if (item.* == .Stmt and item.Stmt.* == .Default) {
                        printSpace(writer, depth + 1);
                    } else {
                        printSpace(writer, depth + 2);
                    }
                    printBlockItem(writer, item, depth, true);
                }
                printSpace(writer, depth);
                write(writer, "}\n");
            },
            .Case => |case_stmt| {
                write(writer, "case ");
                writeFmt(writer, "{s}:\n", .{case_stmt.value});
            },
            .Default => {
                write(writer, "default:\n");
            },
            .Break => {
                write(writer, "break");
                write(writer, ";\n");
            },
            .Continue => {
                write(writer, "continue");
                write(writer, ";\n");
            },
            .DoWhile => printDoWhileStmt(writer, stmt, depth),
            .While => printWhileStmt(writer, stmt, depth),
            .For => printForStmt(writer, stmt, depth),
            .Label => |label_stmt| {
                writeFmt(writer, "{s}:\n", .{label_stmt.ident});
                printStmt(writer, label_stmt.stmt, depth + 1);
            },
            .Goto => |goto_stmt| {
                writeFmt(writer, "goto {s};\n", .{goto_stmt.ident});
            },
            .If => {
                printIfStmt(writer, stmt, depth);
            },
            .Compound => |compound_stmt| {
                write(writer, "{\n");
                printBlock(writer, compound_stmt.body, depth + 1, true);
                printSpace(writer, depth);
                write(writer, "}");
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
            .Prefix => |prefix_expr| {
                writeFmt(writer, "{s}", .{
                    switch (prefix_expr.operator) {
                        .Add => "++",
                        .Subtract => "--",
                    },
                });
                printExpr(writer, prefix_expr.expr);
            },
            .FnCall => |fn_call_expr| {
                writeFmt(writer, "{s}(", .{fn_call_expr.ident});
                for (fn_call_expr.args.items, 0..) |arg, i| {
                    printExpr(writer, arg);
                    if (i < fn_call_expr.args.items.len - 1) {
                        write(writer, ", ");
                    }
                }
                write(writer, ")");
            },
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
// const ArrayList = std.ArrayList;
const ArrayList = @import("from_scratch/ArrayList.zig").ArrayList;
const TokenType = Lexer.TokenType;
const Token = Lexer.Token;
const AnyWriter = std.io.AnyWriter;
const CompilerError = @import("util.zig").CompilerError;
