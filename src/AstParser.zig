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
        var collector = Printer.init(opt.scratch_arena);
        self.error_reporter.printError(collector.writer());
        collector.printToStdErr(.{}) catch return ParseError.PrintFailed;
        return e;
    };

    if (self.current < self.tokens.len) {
        const last_token = self.peek();
        self.parseError(
            ParseError.UnexpectedToken,
            "Unexpected token `{s}` << {s} >> at end of input",
            .{ last_token.lexeme, @tagName(last_token.type) },
        ) catch |e| {
            var printer = Printer.init(self.scratch_arena);
            self.error_reporter.printError(printer.writer());
            printer.printToStdErr(.{}) catch return ParseError.PrintFailed;
            return e;
        };
    }

    if (opt.print_ast) print(self.arena, pg);

    return pg;
}

fn print(arena: Allocator, program: *const Ast.Program) void {
    var collector = Printer.init(arena);
    var ast_printer = AstPrinter.init(collector.writer(), false);
    ast_printer.print(program);
    collector.printToStdErr(.{ .show_whitespace = true }) catch unreachable;
}

fn parseProgram(p: *Self) ParseError!*Ast.Program {
    var decls = ArrayList(*Ast.Decl).init(p.arena);
    while (!p.isAtEnd()) {
        const decl = try p.parseDecl(.{});
        decls.append(decl);
    }
    return .init(p.arena, decls);
}

const ParseFnDecl = struct {
    only_decl: bool = false,
};

fn parseFnDecl(p: *Self, opt: ParseFnDecl) ParseError!*Ast.FnDecl {
    const return_type, const storage_class = try p.parseTypeAndStorageClass();

    const ident = try p.consume(.Ident);
    const params, const params_type = try p.parseFnParams();

    const fn_type = Ast.BuiltinType.fnType(p.arena, return_type, params_type);

    if (p.peek().type == .Semicolon) {
        _ = try p.consume(.Semicolon);
        return Ast.FnDecl.init(p.arena, ident.lexeme, fn_type, params, null, storage_class, ident.line, ident.start);
    }
    if (opt.only_decl) {
        try p.parseError(
            ParseError.ExpectedOnlyFnDefinition,
            "Expected only function declaration, found function definition",
            .{},
        );
    }
    const body = try p.parseBlock();
    return .init(p.arena, ident.lexeme, fn_type, params, body, storage_class, ident.line, ident.start);
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

fn isMaybeDecl(p: Self, offset: i8) bool {
    return switch (p.peekOffset(offset).type) {
        .Int, .Static, .Extern, .Long => true,
        else => false,
    };
}

fn parseBlockItem(p: *Self) ParseError!*Ast.BlockItem {
    if (p.isMaybeDecl(0)) {
        const decl = try p.parseDecl(.{ .only_fn_decl = true });
        return .decl(p.arena, decl);
    }

    const peek_token = p.peek();
    if (peek_token.type == .Semicolon) {
        _ = try p.consume(.Semicolon);
        return .stmt(p.arena, .nullStmt(p.arena, peek_token.line, peek_token.start));
    }

    return .stmt(p.arena, try p.parseStmt());
}

fn parseType(p: *Self, type_hints: []const []const u8) ParseError!*Ast.BuiltinType {
    if (type_hints.len == 0) try p.parseError(
        ParseError.InvalidType,
        "unable to parse type",
        .{},
    );
    switch (type_hints.len) {
        1 => {
            if (eql(u8, "long", type_hints[0])) return .longType(p.arena);
            if (eql(u8, "int", type_hints[0])) return .intType(p.arena);
            return try p.parseError(ParseError.InvalidType, "invalid type `{s}`", .{type_hints[0]});
        },
        2 => {
            if (hasItem(type_hints, "long")) return .longType(p.arena);
            if (hasItem(type_hints, "int")) return .intType(p.arena);
            try p.parseError(ParseError.InvalidType, "invalid type `{any}`", .{type_hints});
        },
        else => try p.parseError(ParseError.InvalidType, "invalid type `{any}`", .{type_hints}),
    }
}

fn hasItem(haystack: []const []const u8, needle: []const u8) bool {
    for (haystack) |item| {
        if (eql(u8, item, needle)) return true;
    }
    return false;
}

fn parseTypeAndStorageClass(p: *Self) ParseError!struct { *Ast.BuiltinType, ?Ast.StorageClass } {
    var types: ArrayList([]const u8) = .init(p.scratch_arena);
    var storage_classes: ArrayList(Ast.StorageClass) = .init(p.scratch_arena);
    while (true) {
        const peeked_token = p.peek();
        switch (peeked_token.type) {
            .Int, .Long => {
                _ = try p.consumeAny();
                types.append(peeked_token.lexeme);
            },
            .Static => {
                _ = try p.consumeAny();
                storage_classes.append(.Static);
            },
            .Extern => {
                _ = try p.consumeAny();
                storage_classes.append(.Extern);
            },
            else => break, // Don't care about other tokens
        }
    }

    const typez = try p.parseType(types.items);

    if (storage_classes.items.len > 1) {
        try p.parseError(ParseError.InvalidStorageClass, "decl must have only one storage class. Found `{any}`", .{storage_classes});
    }
    return .{
        typez,
        if (storage_classes.items.len == 0) null else storage_classes.items[0],
    };
}

const ParseDeclOption = struct { only_fn_decl: bool = false };

fn parseDecl(p: *Self, option: ParseDeclOption) ParseError!*Ast.Decl {
    if (p.isFnDecl()) {
        const fn_decl = try p.parseFnDecl(.{ .only_decl = option.only_fn_decl });
        return .fnDecl(p.arena, fn_decl);
    }
    return .variableDecl(p.arena, try p.parseVarDecl());
}

fn parseVarDecl(p: *Self) ParseError!*Ast.VarDecl {
    const var_type, const storage_class = try p.parseTypeAndStorageClass();
    const ident_token = try p.consume(.Ident);
    const ident = ident_token.lexeme;

    if (isDigit(ident[0])) {
        try p.parseError(ParseError.InvalidIdent, "identifier should not start with digit. Found `{s}` << {s} >>", .{ ident, @tagName(ident_token.type) });
    }

    var var_initializer: ?*Ast.Expr = null;
    if (p.peek().type == .Assign) {
        _ = try p.consumeAny();
        var_initializer = try p.parseExpr(0);
    }
    _ = try p.consume(.Semicolon);
    return Ast.VarDecl.init(p.arena, ident, var_type, var_initializer, storage_class, ident_token.line, ident_token.start);
}

fn isFnDecl(p: Self) bool {
    var peek_offset: i8 = 0;
    while (p.isMaybeDecl(peek_offset)) peek_offset += 1;

    if (p.peekOffset(peek_offset).type != .Ident) return false;
    peek_offset += 1;

    if (p.peekOffset(peek_offset).type != .LParen) return false;
    return true;
}

fn parseFnParams(p: *Self) ParseError!struct { ArrayList(*Ast.FnParam), ArrayList(*Ast.BuiltinType) } {
    _ = try p.consume(.LParen);
    var params = ArrayList(*Ast.FnParam).init(p.arena);
    var params_type = ArrayList(*Ast.BuiltinType).init(p.arena);

    if (p.peek().type == .Void) {
        _ = try p.consume(.Void);
        _ = try p.consume(.RParen);
        return .{ params, params_type };
    }

    while (p.peek().type != .RParen) {
        const param_type, const stclass = try p.parseTypeAndStorageClass();
        if (stclass != null) try p.parseError(ParseError.UnexpectedToken, "fn param cannot have storage class", .{});

        const ident = try p.consume(.Ident);

        const param: *Ast.FnParam = .fnParam(p.arena, ident.lexeme, ident.line, ident.start);
        params.append(param);
        params_type.append(param_type);

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
    return .{ params, params_type };
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

fn isExternOrStaticVar(decl: *const Ast.Decl) bool {
    if (decl.* != .Var) return false;
    return decl.Var.storage_class == .Extern or decl.Var.storage_class == .Static;
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
            if (case_found and item.* == .Decl and !isExternOrStaticVar(item.Decl)) {
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
    if (p.isMaybeDecl(0)) {
        const decl = try p.parseDecl(.{ .only_fn_decl = true });
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

// Most potentially I should do this on sema
// int/long are easy, but after we introduce custom types
// we will need to know which type it is, and it is on sema phase we know them
// for now let's keep it simple
fn parseOptionalTypeToken(p: *Self) ?*Ast.BuiltinType {
    return switch (p.peek().type) {
        .Int => .intType(p.arena),
        .Long => .longType(p.arena),
        else => null,
    };
}

fn parseConstant(p: *Self) ParseError!*Ast.Expr {
    const literal_value = try p.consumeAny();
    const to_parse = if (std.ascii.isAlphabetic(literal_value.lexeme[literal_value.lexeme.len - 1])) literal_value.lexeme[0 .. literal_value.lexeme.len - 1] else literal_value.lexeme;
    const value = std.fmt.parseInt(i64, to_parse, 10) catch {
        try p.parseError(
            ParseError.IntExpected,
            "expected int found `{s}` << {s} >>",
            .{ literal_value.lexeme, @tagName(literal_value.type) },
        );
    };
    return switch (literal_value.type) {
        .LongLiteral => .constantExpr(
            p.arena,
            .{ .Long = value },
            literal_value.line,
            literal_value.start,
        ),
        .IntLiteral => if (value > std.math.maxInt(i32))
            .constantExpr(p.arena, .{ .Long = value }, literal_value.line, literal_value.start)
        else
            .constantExpr(p.arena, .{ .Int = @intCast(value) }, literal_value.line, literal_value.start),
        else => try p.parseError(
            ParseError.UnexpectedToken,
            "unexpected token while parsing constant `{s}` << {s} >>",
            .{ literal_value.lexeme, @tagName(literal_value.type) },
        ),
    };
}

fn parseFactor(p: *Self) ParseError!*Ast.Expr {
    const token = p.peek();
    switch (token.type) {
        .IntLiteral, .LongLiteral => return try p.parseConstant(),
        .BitNot, .Minus, .Not => {
            const op_token = try p.consumeAny();
            const operator = try p.parseUnaryOperator(op_token.type);
            const inner_expr = try p.parseFactor();
            return .unaryExpr(p.arena, operator, inner_expr, op_token.line, op_token.start);
        },
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
            if (p.parseOptionalTypeToken()) |type_token| {
                _ = try p.consumeAny();
                _ = try p.consume(.RParen);
                // cast expr
                const expr_to_cast = try p.parseFactor();
                return .castExpr(p.arena, type_token, expr_to_cast, group_token.line, group_token.start);
            }

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
            "Unexpected token while parsing factor `{s}` << {s} >>",
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

        .Long,
        .LongLiteral,
        .Signed,
        .Unsigned,
        .Static,
        .Extern,
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

        .Long,
        .LongLiteral,
        .Static,
        .Extern,
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
        .Signed,
        .Unsigned,
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
        decls: ArrayList(*Decl),

        pub fn init(allocator: Allocator, decls: ArrayList(*Decl)) *Program {
            const pg = allocator.create(Program) catch unreachable;
            pg.* = .{ .decls = decls };
            return pg;
        }
    };

    pub const BuiltinType = union(enum) {
        Int,
        Long,
        Fn: struct {
            params: ArrayList(*BuiltinType),
            return_type: *BuiltinType,
        },

        pub fn clone(self: *const @This(), allocator: Allocator) *BuiltinType {
            const typez = allocator.create(BuiltinType) catch unreachable;
            typez.* = switch (self.*) {
                .Int => .Int,
                .Long => .Long,
                .Fn => |fn_type| blk: {
                    var params_clone = ArrayList(*BuiltinType).init(allocator);
                    for (fn_type.params.items) |param| {
                        params_clone.append(param.clone(allocator));
                    }
                    break :blk .{ .Fn = .{ .params = params_clone, .return_type = fn_type.return_type.clone(allocator) } };
                },
            };
            return typez;
        }

        pub fn intType(allocator: Allocator) *BuiltinType {
            const typez = allocator.create(BuiltinType) catch unreachable;
            typez.* = .Int;
            return typez;
        }

        pub fn longType(allocator: Allocator) *BuiltinType {
            const typez = allocator.create(BuiltinType) catch unreachable;
            typez.* = .Long;
            return typez;
        }

        pub fn fnType(
            allocator: Allocator,
            return_type: *BuiltinType,
            params: ArrayList(*BuiltinType),
        ) *BuiltinType {
            const typez = allocator.create(BuiltinType) catch unreachable;
            typez.* = .{ .Fn = .{ .return_type = return_type, .params = params } };
            return typez;
        }

        pub fn isSame(self: *const BuiltinType, other: *const BuiltinType) bool {
            return switch (self.*) {
                .Int => switch (other.*) {
                    .Int => true,
                    else => false,
                },
                .Long => switch (other.*) {
                    .Long => true,
                    else => false,
                },
                .Fn => switch (other.*) {
                    .Fn => |other_fn| {
                        if (self.params.items.len != other_fn.params.items.len) return false;
                        for (self.params.items, other_fn.params.items) |param, other_param| {
                            if (!param.isSame(other_param)) return false;
                        }
                        return true;
                    },
                    else => false,
                },
            };
        }
    };

    pub const StorageClass = enum { Static, Extern };

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

        pub fn variableDecl(allocator: Allocator, var_decl: *VarDecl) *Decl {
            const item = allocator.create(Decl) catch unreachable;
            item.* = .{ .Var = var_decl };
            return item;
        }
    };

    pub const VarDecl = struct {
        ident: []const u8,
        initializer: ?*Expr,
        storage_class: ?StorageClass,
        type: *BuiltinType,
        loc: SourceLocation,

        pub fn init(
            allocator: Allocator,
            ident: []const u8,
            typez: *BuiltinType,
            initializer: ?*Expr,
            storage_class: ?StorageClass,
            line: usize,
            start: usize,
        ) *@This() {
            const var_decl = allocator.create(VarDecl) catch unreachable;
            var_decl.* = .{
                .ident = ident,
                .initializer = initializer,
                .storage_class = storage_class,
                .type = typez,
                .loc = .{ .line = line, .start = start },
            };
            return var_decl;
        }
    };

    pub const FnDecl = struct {
        name: []const u8,
        params: ArrayList(*FnParam),
        body: ?*Block,
        type: ?*BuiltinType,
        storage_class: ?StorageClass,
        loc: SourceLocation,

        pub fn init(allocator: Allocator, name: []const u8, fn_type: *BuiltinType, params: ArrayList(*FnParam), body: ?*Block, storage_class: ?StorageClass, line: usize, start: usize) *FnDecl {
            const fn_decl = allocator.create(FnDecl) catch unreachable;
            fn_decl.* = .{
                .name = name,
                .params = params,
                .body = body,
                .type = fn_type,
                .storage_class = storage_class,
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

    // @note - all type need SourceLocation, instead of tagged union should I have use normal union type or struct hierarchy with @fieldParentPtr?
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

    pub const Constant = union(enum) {
        Int: i32,
        Long: i64,

        pub fn int(value: i32) @This() {
            return .{ .Int = value };
        }
        pub fn long(value: i64) @This() {
            return .{ .Long = value };
        }
    };

    pub const Expr = union(enum) {
        Cast: struct { target_type: *BuiltinType, expr: *Expr, type: ?*BuiltinType, loc: SourceLocation },
        Constant: struct { value: Constant, type: ?*BuiltinType, loc: SourceLocation },
        Var: struct { ident: []const u8, type: ?*BuiltinType, loc: SourceLocation },
        Unary: struct { operator: UnaryOperator, expr: *Expr, type: ?*BuiltinType, loc: SourceLocation },
        Binary: struct { operator: BinaryOperator, left: *Expr, right: *Expr, type: ?*BuiltinType, loc: SourceLocation },
        Assignment: struct { src: *Expr, dst: *Expr, type: ?*BuiltinType, loc: SourceLocation },
        Group: struct { expr: *Expr, type: ?*BuiltinType, loc: SourceLocation },
        Prefix: struct { operator: PrefixOperator, expr: *Expr, type: ?*BuiltinType, loc: SourceLocation },
        Postfix: struct { operator: PostfixOperator, expr: *Expr, type: ?*BuiltinType, loc: SourceLocation },
        Ternary: struct { condition: *Expr, then: *Expr, @"else": *Expr, type: ?*BuiltinType, loc: SourceLocation },
        FnCall: struct { ident: []const u8, args: ArrayList(*Expr), type: ?*BuiltinType, loc: SourceLocation },

        pub fn getLoc(self: *const @This()) SourceLocation {
            return switch (self.*) {
                .Cast => |cast_expr| cast_expr.loc,
                .Constant => |constant_expr| constant_expr.loc,
                .Var => |var_expr| var_expr.loc,
                .Unary => |unary_expr| unary_expr.loc,
                .Binary => |binary_expr| binary_expr.loc,
                .Assignment => |assignment_expr| assignment_expr.loc,
                .Group => |group_expr| group_expr.loc,
                .Postfix => |postfix_expr| postfix_expr.loc,
                .Ternary => |ternary_expr| ternary_expr.loc,
                .FnCall => |fn_call_expr| fn_call_expr.loc,
                .Prefix => |prefix_expr| prefix_expr.loc,
            };
        }

        pub fn getType(self: *const @This()) ?*BuiltinType {
            return switch (self.*) {
                .Cast => |cast_expr| cast_expr.type,
                .Constant => |constant_expr| constant_expr.type,
                .Var => |var_expr| var_expr.type,
                .Unary => |unary_expr| unary_expr.type,
                .Binary => |binary_expr| binary_expr.type,
                .Assignment => |assignment_expr| assignment_expr.type,
                .Group => |group_expr| group_expr.type,
                .Postfix => |postfix_expr| postfix_expr.type,
                .Ternary => |ternary_expr| ternary_expr.type,
                .FnCall => |fn_call_expr| fn_call_expr.type,
                .Prefix => |prefix_expr| prefix_expr.type,
            };
        }

        pub fn setType(self: *@This(), typez: *BuiltinType) void {
            switch (self.*) {
                .Prefix => |*prefix_expr| prefix_expr.type = typez,
                .Cast => |*cast_expr| cast_expr.type = typez,
                .Constant => |*constant_expr| constant_expr.type = typez,
                .Var => |*var_expr| var_expr.type = typez,
                .Unary => |*unary_expr| unary_expr.type = typez,
                .Binary => |*binary_expr| binary_expr.type = typez,
                .Assignment => |*assignment_expr| assignment_expr.type = typez,
                .Group => |*group_expr| group_expr.type = typez,
                .Postfix => |*postfix_expr| postfix_expr.type = typez,
                .Ternary => |*ternary_expr| ternary_expr.type = typez,
                .FnCall => |*fn_call_expr| fn_call_expr.type = typez,
            }
        }

        pub fn castExpr(allocator: Allocator, target_type: *BuiltinType, expr: *Expr, line: usize, start: usize) *Expr {
            const cast_expr = allocator.create(Expr) catch unreachable;
            cast_expr.* = .{
                .Cast = .{
                    .target_type = target_type,
                    .expr = expr,
                    .type = null,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return cast_expr;
        }

        pub fn fnCallExpr(allocator: Allocator, ident: []const u8, args: ArrayList(*Expr), line: usize, start: usize) *Expr {
            const expr = allocator.create(Expr) catch unreachable;
            expr.* = .{
                .FnCall = .{
                    .ident = ident,
                    .args = args,
                    .type = null,
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
                    .type = null,
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
                    .type = null,
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
                    .type = null,
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
                    .type = null,
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
                    .type = null,
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
                    .type = null,
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
                    .type = null,
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
                    .type = null,
                    .loc = .{ .line = line, .start = start },
                },
            };
            return expr;
        }

        pub fn constantExpr(allocator: Allocator, value: Constant, line: usize, start: usize) *Expr {
            const expr = allocator.create(Expr) catch unreachable;
            expr.* = .{
                .Constant = .{
                    .value = value,
                    .type = null,
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
    InvalidType,
    InvalidStorageClass,
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
    writer: *std.Io.Writer,
    print_expr_type: bool,
    pub fn init(writer: *std.Io.Writer, print_expr_type: bool) @This() {
        return .{ .writer = writer, .print_expr_type = print_expr_type };
    }

    pub fn print(self: *@This(), pg: *const Ast.Program) void {
        self.write("-- AST --\n");
        self.write("program\n");

        for (pg.decls.items) |decl| {
            switch (decl.*) {
                .Fn => |fn_decl| {
                    self.printFnDecl(fn_decl, 1, true);
                    self.write("\n");
                },
                .Var => |var_decl| {
                    self.printVarDecl(var_decl, 1, false);
                    self.write(";");
                },
            }
        }

        self.write("\n");
    }

    fn printFnDecl(self: *@This(), fn_decl: *const Ast.FnDecl, depth: usize, indent: bool) void {
        if (indent) self.printSpace(depth);
        self.writeFmt("int {s}( ", .{fn_decl.name});
        for (fn_decl.params.items, 0..) |param, i| {
            if (fn_decl.body != null) self.write("int ");
            self.writeFmt("{s}", .{param.ident});
            if (i < fn_decl.params.items.len - 1) {
                self.write(", ");
            }
        }
        self.write(" )");

        if (fn_decl.body) |body| {
            self.write("\n");
            self.printSpace(depth);
            self.write("{\n");
            self.printBlock(body, depth + 1, true);
            self.printSpace(depth);
            self.write("}");
        } else {
            self.write(";\n");
        }
    }

    fn printBlock(self: *@This(), block: *const Ast.Block, depth: usize, new_line: bool) void {
        for (block.block_item.items) |item| {
            self.printSpace(depth);
            self.printBlockItem(item, depth, new_line);
        }
    }

    fn printBlockItem(self: *@This(), block_item: *const Ast.BlockItem, depth: usize, new_line: bool) void {
        switch (block_item.*) {
            .Stmt => |stmt| self.printStmt(stmt, depth),
            .Decl => |decl| self.printDecl(decl, depth, new_line),
        }
    }

    fn printDecl(self: *@This(), decl: *const Ast.Decl, depth: usize, new_line: bool) void {
        switch (decl.*) {
            .Fn => |fn_decl| self.printFnDecl(fn_decl, depth, false),
            .Var => |var_decl| self.printVarDecl(var_decl, depth, new_line),
        }
    }

    fn printVarDecl(self: *@This(), var_decl: *const Ast.VarDecl, depth: usize, new_line: bool) void {
        _ = depth;
        self.write("int ");
        self.writeFmt("{s}", .{var_decl.ident});
        if (var_decl.initializer) |initializer| {
            self.write(" = ");
            self.printExpr(initializer);
        }
        if (new_line) self.write(";\n");
    }

    fn printIfStmt(self: *@This(), stmt: *const Ast.Stmt, depth: usize) void {
        const if_stmt = switch (stmt.*) {
            .If => |if_stmt| if_stmt,
            else => std.debug.panic(
                "** Compiler Bug ** printIfStmt called on non If statement",
                .{},
            ),
        };
        self.write("if (");
        self.printExpr(if_stmt.condition);
        self.write(")\n");

        const if_depth = if (if_stmt.then.* == .Compound) depth else depth + 1;
        self.printSpace(if_depth);
        self.printStmt(if_stmt.then, if_depth);
        if (if_stmt.@"else") |else_stmt| {
            if (if_stmt.then.* == .Compound) {
                self.write("\n");
            }
            self.printSpace(depth);
            self.write("else");
            if (else_stmt.* == .If) {
                self.write(" ");
                self.printIfStmt(else_stmt, depth);
            } else {
                self.write("\n");
                const else_depth = if (if_stmt.then.* == .Compound) depth else depth + 1;
                self.printSpace(else_depth);
                self.printStmt(else_stmt, else_depth);
            }
        }
        self.write("\n");
    }

    fn printDoWhileStmt(self: *@This(), stmt: *const Ast.Stmt, depth: usize) void {
        const do_stmt = switch (stmt.*) {
            .DoWhile => |do_while| do_while,
            else => std.debug.panic(
                "** Compiler Bug ** printDoWhileStmt called on non doWhile statement",
                .{},
            ),
        };
        self.write("do\n");
        self.printSpace(depth);
        const do_depth = if (do_stmt.body.* == .Compound) depth else depth + 1;
        self.printStmt(do_stmt.body, do_depth);
        self.write(" while ( ");
        self.printExpr(do_stmt.condition);
        self.write(" );\n");
    }

    fn printWhileStmt(self: *@This(), stmt: *const Ast.Stmt, depth: usize) void {
        const while_stmt = switch (stmt.*) {
            .While => |while_stmt_| while_stmt_,
            else => std.debug.panic(
                "** Compiler Bug ** whileStmt called on non while statement",
                .{},
            ),
        };
        self.write("while ( ");
        self.printExpr(while_stmt.condition);
        self.write(" )\n");

        const while_depth = if (while_stmt.body.* == .Compound) depth else depth + 1;

        self.printSpace(depth);
        self.printStmt(while_stmt.body, while_depth);
        self.write("\n");
    }

    fn printForStmt(self: *@This(), stmt: *const Ast.Stmt, depth: usize) void {
        const for_stmt = switch (stmt.*) {
            .For => |for_stmt_| for_stmt_,
            else => std.debug.panic(
                "** Compiler Bug ** printForStmt called on non for statement",
                .{},
            ),
        };
        self.write("for (");
        switch (for_stmt.init.*) {
            .Decl => |decl| self.printDecl(decl, depth + 1, false),
            .Expr => |expr| if (expr) |initializer| self.printExpr(initializer),
        }
        self.write(" ; ");

        if (for_stmt.condition) |condition| {
            self.printExpr(condition);
        }
        self.write(" ; ");

        if (for_stmt.post) |post| {
            self.printExpr(post);
        }

        self.write(" )\n");

        self.printSpace(depth);
        const for_depth = if (for_stmt.body.* == .Compound) depth else depth + 1;
        self.printStmt(for_stmt.body, for_depth);
        self.write("\n");
    }

    fn printStmt(self: *@This(), stmt: *const Ast.Stmt, depth: usize) void {
        switch (stmt.*) {
            .Switch => |switch_stmt| {
                self.write("switch (");
                self.printExpr(switch_stmt.condition);
                self.write(")\n");
                self.printSpace(depth);
                self.write("{\n");
                for (switch_stmt.body.items) |item| {
                    if (item.* == .Stmt and item.Stmt.* == .Case) {
                        self.printSpace(depth + 1);
                    } else if (item.* == .Stmt and item.Stmt.* == .Default) {
                        self.printSpace(depth + 1);
                    } else {
                        self.printSpace(depth + 2);
                    }
                    self.printBlockItem(item, depth, true);
                }
                self.printSpace(depth);
                self.write("}\n");
            },
            .Case => |case_stmt| {
                self.write("case ");
                self.writeFmt("{s}:\n", .{case_stmt.value});
            },
            .Default => {
                self.write("default:\n");
            },
            .Break => {
                self.write("break");
                self.write(";\n");
            },
            .Continue => {
                self.write("continue");
                self.write(";\n");
            },
            .DoWhile => self.printDoWhileStmt(stmt, depth),
            .While => self.printWhileStmt(stmt, depth),
            .For => self.printForStmt(stmt, depth),
            .Label => |label_stmt| {
                self.writeFmt("{s}:\n", .{label_stmt.ident});
                self.printStmt(label_stmt.stmt, depth + 1);
            },
            .Goto => |goto_stmt| {
                self.writeFmt("goto {s};\n", .{goto_stmt.ident});
            },
            .If => {
                self.printIfStmt(stmt, depth);
            },
            .Compound => |compound_stmt| {
                self.write("{\n");
                self.printBlock(compound_stmt.body, depth + 1, true);
                self.printSpace(depth);
                self.write("}");
            },
            .Return => |return_stmt| {
                self.write("return");
                self.write(" ");
                self.printExpr(return_stmt.expr);
                self.write(";\n");
            },
            .Expr => |expr| {
                self.printExpr(expr.expr);
                self.write(";\n");
            },
            .Null => self.write(";\n"),
        }
    }
    fn printBuiltinType(self: *@This(), typez: *const Ast.BuiltinType) void {
        switch (typez.*) {
            .Int => self.write("int"),
            .Long => self.write("long"),
            else => |t| std.debug.panic("** Compiler Bug ** self.printBuiltinType called on function type. {s}", .{@tagName(t)}),
        }
    }

    fn printType(self: *@This(), typez: ?*const Ast.BuiltinType) void {
        if (self.print_expr_type) {
            std.debug.assert(typez != null);
            self.write("<");
            self.printBuiltinType(typez.?);
            self.write(">");
        }
    }

    fn printExpr(self: *@This(), expr: *const Ast.Expr) void {
        self.printType(expr.getType());
        switch (expr.*) {
            .Cast => |cast_expr| {
                self.write("(");
                self.printBuiltinType(cast_expr.target_type);
                self.write(")");
                self.printExpr(cast_expr.expr);
            },
            .Prefix => |prefix_expr| {
                self.writeFmt("{s}", .{
                    switch (prefix_expr.operator) {
                        .Add => "++",
                        .Subtract => "--",
                    },
                });
                self.printExpr(prefix_expr.expr);
            },
            .FnCall => |fn_call_expr| {
                self.writeFmt("{s}(", .{fn_call_expr.ident});
                for (fn_call_expr.args.items, 0..) |arg, i| {
                    self.printExpr(arg);
                    if (i < fn_call_expr.args.items.len - 1) {
                        self.write(", ");
                    }
                }
                self.write(")");
            },
            .Ternary => |ternary_expr| {
                self.printExpr(ternary_expr.condition);
                self.write(" ? ");
                self.printExpr(ternary_expr.then);
                self.write(" : ");
                self.printExpr(ternary_expr.@"else");
            },
            .Var => |var_expr| {
                self.writeFmt("{s}", .{var_expr.ident});
            },
            .Postfix => |postfix_expr| {
                self.printExpr(postfix_expr.expr);
                switch (postfix_expr.operator) {
                    .Add => self.write("++"),
                    .Subtract => self.write("--"),
                }
            },
            .Group => |group_expr| {
                self.write("(");
                self.printExpr(group_expr.expr);
                self.write(")");
            },
            .Assignment => |assignment_expr| {
                self.printExpr(assignment_expr.dst);
                self.write(" = ");
                self.printExpr(assignment_expr.src);
            },
            .Constant => |constant_expr| {
                switch (constant_expr.value) {
                    .Int => |int_value| {
                        self.writeFmt("{d}", .{int_value});
                    },
                    .Long => |long_value| {
                        self.writeFmt("{d}", .{long_value});
                    },
                }
            },
            .Unary => |unary_expr| {
                self.write("(");
                self.writeFmt("{s}", .{
                    switch (unary_expr.operator) {
                        .Negate => "-",
                        .BitNot => "~",
                        .Not => "!",
                    },
                });
                self.printExpr(unary_expr.expr);
                self.write(")");
            },
            .Binary => |binary_expr| {
                self.write("(");
                self.printExpr(binary_expr.left);
                self.writeFmt(" {s} ", .{
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
                self.printExpr(binary_expr.right);
                self.write(")");
            },
        }
    }

    fn printSpace(self: *@This(), depth: usize) void {
        for (0..depth) |_| self.write("  ");
    }

    fn write(self: *@This(), bytes: []const u8) void {
        _ = self.writer.write(bytes) catch unreachable;
    }

    fn writeFmt(self: *@This(), comptime fmt: []const u8, args: anytype) void {
        _ = self.writer.print(fmt, args) catch unreachable;
    }
};

pub fn recurseGetGroupInnerExpr(expr: *Ast.Expr) *Ast.Expr {
    if (expr.* != .Group) return expr;
    return recurseGetGroupInnerExpr(expr.Group.expr);
}

pub fn isSameType(type1: ?*const Ast.BuiltinType, type2: ?*const Ast.BuiltinType) bool {
    if (type1 == null or type2 == null) return false;
    return switch (type1.?.*) {
        .Int => switch (type2.?.*) {
            .Int => true,
            else => false,
        },
        .Long => switch (type2.?.*) {
            .Long => true,
            else => false,
        },
        .Fn => |type1_fn| switch (type2.?.*) {
            .Fn => |other_fn| {
                if (!isSameType(type1_fn.return_type, other_fn.return_type)) return false;
                if (type1_fn.params.items.len != other_fn.params.items.len) return false;
                for (type1_fn.params.items, other_fn.params.items) |param, other_param| {
                    if (!isSameType(param, other_param)) return false;
                }
                return true;
            },
            else => false,
        },
    };
}

pub fn getCommonType(allocator: Allocator, type1: *Ast.BuiltinType, type2: *Ast.BuiltinType) struct { bool, ?*Ast.BuiltinType } {
    if (isSameType(type1, type2)) return .{ true, type1 };
    if (type1.* == .Fn or type2.* == .Fn) @panic("not implemented - fn common type checking");
    if (type1.* == .Long or type2.* == .Long) return .{ false, .longType(allocator) };
    return .{ false, .intType(allocator) };
}

pub fn maybeExplicitCast(allocator: Allocator, expr: *Ast.Expr, target_type: *Ast.BuiltinType) *Ast.Expr {
    if (isSameType(expr.getType(), target_type)) return expr;
    const loc = expr.getLoc();
    const cast_expr: *Ast.Expr = .castExpr(allocator, target_type, expr, loc.line, loc.start);
    cast_expr.setType(target_type);
    return cast_expr;
}

// const ArrayList = std.ArrayList;

const std = @import("std");
const Allocator = std.mem.Allocator;

const CompilerError = @import("util.zig").CompilerError;
const ErrorReporter = @import("ErrorReporter.zig");
const ArrayList = @import("from_scratch/ArrayList.zig").ArrayList;
const Lexer = @import("Lexer.zig");
const TokenType = Lexer.TokenType;
const Token = Lexer.Token;
const Printer = @import("util.zig").Printer;
const isDigit = std.ascii.isDigit;
const eql = std.mem.eql;
