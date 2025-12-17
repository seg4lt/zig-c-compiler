arena: Allocator,
symbol_table: *SymbolTable,
var_count: usize = 0,
label_count: usize = 0,

const Self = @This();

const Options = struct {
    arena: Allocator,
    pg: *const Ast.Program,
    symbol_table: *SymbolTable,
    print: bool = false,
};

pub fn genTacky(opt: Options) Tac.Program {
    var self = Self{ .arena = opt.arena, .symbol_table = opt.symbol_table };
    const pg = self.genPg(opt.pg);
    if (opt.print) {
        var printer = Printer.init(opt.arena);
        TackyIRPrinter.print(printer.writer(), pg);
        printer.printToStdErr(.{}) catch unreachable;
    }
    return pg;
}

fn genPg(s: *Self, pg: *const Ast.Program) Tac.Program {
    var decls = ArrayList(Tac.TopLevelDecl).init(s.arena);

    for (pg.decls.items) |decl| {
        switch (decl.*) {
            .Fn => |fn_decl| {
                if (fn_decl.body != null) {
                    const fn_tacky = s.genFnDefn(fn_decl);
                    decls.append(fn_tacky);
                }
            },
            .Var => {}, // noop
        }
    }

    var symbol_iter = s.symbol_table.inner.iterator();
    while (symbol_iter.next()) |symbol| {
        switch (symbol.value_ptr.*) {
            .Fn => {}, // noop
            .Var => |var_symbol| {
                switch (var_symbol) {
                    .Local => {}, // noop
                    .Static => |static_attr| {
                        switch (static_attr.initial_value) {
                            .no_initializer => {},
                            .tentative => {
                                decls.append(.topLevelStaticVar(symbol.key_ptr.*, static_attr.global, static_attr.type.clone(s.arena), .int(0)));
                            },
                            .initial => |initial_value| {
                                decls.append(.topLevelStaticVar(symbol.key_ptr.*, static_attr.global, static_attr.type.clone(s.arena), initial_value));
                            },
                        }
                    },
                }
            },
        }
    }

    return .{ .decls = decls };
}

fn genFnDefn(s: *Self, fn_decl: *const Ast.FnDecl) Tac.TopLevelDecl {
    var instructions = ArrayList(Tac.Instruction).init(s.arena);

    if (fn_decl.body) |body| {
        s.genBlock(body, &instructions);
        instructions.append(.ret(.constant(.int(0))));
    }
    const is_global = blk: {
        if (s.symbol_table.get(fn_decl.name)) |symbol| {
            std.debug.assert(symbol == .Fn);
            break :blk symbol.Fn.global;
        }
        std.debug.panic("** Compiler Bug ** - function symbol not found in symbol table during tacky IR generation", .{});
    };

    return .topLevelFn(s.arena.dupe(u8, fn_decl.name) catch unreachable, is_global, fn_decl.type.?.clone(s.arena), instructions);
}

fn genBlock(s: *Self, block: *const Ast.Block, instructions: *ArrayList(Tac.Instruction)) void {
    var block_item = block.block_item;
    s.genBlockItem(&block_item, instructions);
}

fn genBlockItem(s: *Self, block_item: *ArrayList(*Ast.BlockItem), instructions: *ArrayList(Tac.Instruction)) void {
    for (block_item.*.items) |item| {
        switch (item.*) {
            .Stmt => |stmt| s.genStmt(stmt, instructions),
            .Decl => |decl| s.genDecl(decl, instructions),
        }
    }
}
fn genDecl(s: *Self, decl: *Ast.Decl, instructions: *ArrayList(Tac.Instruction)) void {
    switch (decl.*) {
        .Var => |var_decl| {
            if (var_decl.initializer) |initializer| {
                if (var_decl.storage_class == .Static) return;
                const result = s.genExpr(initializer, instructions);
                const dst: Tac.Val = .variable(s.arena.dupe(u8, var_decl.ident) catch unreachable);
                instructions.append(.copy(result, dst));
            }
            // variable with just declaration can be ignored, it has served its purpose
        },
        .Fn => |fn_decl| {
            if (fn_decl.body != null) {
                std.debug.panic("** Compiler Bug ** - function declaration should not have body in this phase", .{});
            }
        },
    }
}

fn genStmt(s: *Self, stmt: *const Ast.Stmt, instructions: *ArrayList(Tac.Instruction)) void {
    switch (stmt.*) {
        .Switch => |switch_stmt| s.genSwitchStmt(switch_stmt, instructions),
        .Case => |case_stmt| {
            if (case_stmt.label == null) {
                std.debug.panic("** Compiler Bug ** case statement should have label as part of sema loop labeling phase", .{});
            }
            const owned_ident = s.arena.dupe(u8, case_stmt.label.?) catch unreachable;
            instructions.append(.label(owned_ident));
        },
        .Default => |default_stmt| {
            if (default_stmt.label == null) {
                std.debug.panic("** Compiler Bug ** default statement should have label as part of sema loop labeling phase", .{});
            }
            const owned_ident = s.arena.dupe(u8, default_stmt.label.?) catch unreachable;
            instructions.append(.label(owned_ident));
        },
        .Break => |break_stmt| {
            if (break_stmt.ident == null) {
                std.debug.panic("** Compiler Bug ** break statement should have label as part of sema loop labeling phase", .{});
            }
            // Any other way?
            // this label needs to match with once defined in the loop
            const jump_label = std.fmt.allocPrint(s.arena, "{s}_end", .{break_stmt.ident.?}) catch unreachable;
            instructions.append(.jump(jump_label));
        },
        .Continue => |continue_stmt| {
            if (continue_stmt.ident == null) {
                std.debug.panic("** Compiler Bug ** continue statement should have label as part of sema loop labeling phase", .{});
            }
            // Any other way?
            // this label needs to match with once defined in the loop
            const jump_label = std.fmt.allocPrint(s.arena, "{s}_post_expr", .{continue_stmt.ident.?}) catch unreachable;
            instructions.append(.jump(jump_label));
        },
        .DoWhile => |do_stmt| {
            if (do_stmt.label == null) {
                std.debug.panic("** Compiler Bug ** do-while statement should have label as part of sema loop labeling phase", .{});
            }
            const loop_start_label = std.fmt.allocPrint(s.arena, "{s}_start", .{do_stmt.label.?}) catch unreachable;
            const loop_end_label = std.fmt.allocPrint(s.arena, "{s}_end", .{do_stmt.label.?}) catch unreachable;

            // Refactor
            // adding post expr just because I am handling continue to go to post expr
            // this makes it easier to model for loop
            // Maybe there is a better way to do this but..... this is what we have for now
            const loop_post_expr_label = std.fmt.allocPrint(s.arena, "{s}_post_expr", .{do_stmt.label.?}) catch unreachable;

            instructions.append(.label(loop_start_label));

            s.genStmt(do_stmt.body, instructions);

            instructions.append(.label(loop_post_expr_label));
            const condition = s.genExpr(do_stmt.condition, instructions);
            instructions.append(.jumpIfNotZero(condition, loop_start_label));

            instructions.append(.label(loop_end_label));
        },
        .While => |while_stmt| {
            if (while_stmt.label == null) {
                std.debug.panic("** Compiler Bug ** while statement should have label as part of sema loop labeling phase", .{});
            }
            const loop_start_label = std.fmt.allocPrint(s.arena, "{s}_start", .{while_stmt.label.?}) catch unreachable;
            const loop_end_label = std.fmt.allocPrint(s.arena, "{s}_end", .{while_stmt.label.?}) catch unreachable;

            // Note:
            // adding post expr just because I am handling continue to go to post expr
            // this makes it easier to model for loop
            // Maybe there is a better way to do this but..... this is what we have for now
            const loop_post_expr_label = std.fmt.allocPrint(s.arena, "{s}_post_expr", .{while_stmt.label.?}) catch unreachable;

            instructions.append(.label(loop_start_label));
            instructions.append(.label(loop_post_expr_label));

            const condition = s.genExpr(while_stmt.condition, instructions);
            instructions.append(.jumpIfZero(condition, loop_end_label));

            s.genStmt(while_stmt.body, instructions);

            instructions.append(.jump(loop_post_expr_label));
            instructions.append(.label(loop_end_label));
        },
        .For => |for_stmt| {
            if (for_stmt.label == null) {
                std.debug.panic("** Compiler Bug ** for statement should have label as part of sema loop labeling phase", .{});
            }
            const loop_start_label = std.fmt.allocPrint(s.arena, "{s}_start", .{for_stmt.label.?}) catch unreachable;
            const loop_post_expr_label = std.fmt.allocPrint(s.arena, "{s}_post_expr", .{for_stmt.label.?}) catch unreachable;
            const loop_end_label = std.fmt.allocPrint(s.arena, "{s}_end", .{for_stmt.label.?}) catch unreachable;

            switch (for_stmt.init.*) {
                .Decl => |decl| s.genDecl(decl, instructions),
                .Expr => |expr| _ = if (expr) |initializer| s.genExpr(initializer, instructions),
            }

            // jump to loop start - as first time you come into this phase, you don't want to run post expr
            instructions.append(.jump(loop_start_label));

            instructions.append(.label(loop_post_expr_label));

            if (for_stmt.post) |post_expr| {
                _ = s.genExpr(post_expr, instructions);
            }

            // Loop start - includes the condition and jump to body if needed
            instructions.append(.label(loop_start_label));

            if (for_stmt.condition) |condition| {
                const condition_result = s.genExpr(condition, instructions);
                instructions.append(.jumpIfZero(condition_result, loop_end_label));
            }

            s.genStmt(for_stmt.body, instructions);

            instructions.append(.jump(loop_post_expr_label));
            instructions.append(.label(loop_end_label));
        },
        .Label => |label_stmt| {
            const owned_ident = s.arena.dupe(u8, label_stmt.ident) catch unreachable;
            const label: Tac.Instruction = .label(owned_ident);

            instructions.append(label);
            s.genStmt(label_stmt.stmt, instructions);
        },
        .Goto => |goto_stmt| {
            const owned_ident = s.arena.dupe(u8, goto_stmt.ident) catch unreachable;
            instructions.append(.jump(owned_ident));
        },
        .If => |if_stmt| {
            const if_end_label = s.makeLabel("if_end");
            const condition = s.genExpr(if_stmt.condition, instructions);

            const else_block = if_stmt.@"else" orelse {
                instructions.append(.jumpIfZero(condition, if_end_label.Label));
                s.genStmt(if_stmt.then, instructions);
                instructions.append(if_end_label);
                return;
            };
            const else_label = s.makeLabel("else");
            instructions.append(.jumpIfZero(condition, else_label.Label));

            s.genStmt(if_stmt.then, instructions);
            instructions.append(.jump(if_end_label.Label));

            instructions.append(else_label);
            s.genStmt(else_block, instructions);

            instructions.append(if_end_label);
        },
        .Compound => |compound_stmt| s.genBlock(compound_stmt.body, instructions),
        .Expr => |expr_stmt| {
            if (expr_stmt.expr.* == .Var) {
                if (s.symbol_table.get(expr_stmt.expr.Var.ident)) |symbol| {
                    if (symbol != .Var) {
                        std.debug.panic("** Compiler Bug ** - expression statement with variable expr has to be a variable symbol!!!", .{});
                    }
                    if (symbol.Var == .Static) {
                        // if variable is static we don't add initialization instuction
                        // this is done on top-level declaration where we loop through all symbol and find static/global decls
                        return;
                    }
                }
            }

            _ = s.genExpr(expr_stmt.expr, instructions);
        },
        .Return => |ret| {
            const ret_val = s.genExpr(ret.expr, instructions);
            instructions.append(.ret(ret_val));
        },
        .Null => {}, //noop
    }
}

fn genSwitchStmt(s: *Self, stmt: Ast.SwitchStmt, instructions: *ArrayList(Tac.Instruction)) void {
    const condition_expr = s.genExpr(stmt.condition, instructions);

    if (stmt.case_labels == null) {
        std.debug.panic("** Compiler Bug ** switch statement should have case labels as part of sema loop labeling phase", .{});
    }
    // Add jump instruction for all cases first
    // We still don't emit the instruction for each case
    // handle non-default first
    for (stmt.case_labels.?.items) |label| {
        if (label.is_default) continue;
        // we create an expression to check if matches case we have
        const case_value: Tac.Val = .constant(.int(std.fmt.parseInt(i32, label.value, 10) catch unreachable));
        const condition = s.makeVar(.intType(s.arena));
        instructions.append(.binary(.EqualEqual, condition_expr, case_value, condition));

        // we gen the instruction for the expr we created
        // if it mathces, we jump the the case label
        const owned_label = s.arena.dupe(u8, label.label) catch unreachable;
        instructions.append(.jumpIfNotZero(condition, owned_label));
    }
    // handling default case separately as this is if nothing matches
    for (stmt.case_labels.?.items) |label| {
        if (!label.is_default) continue;
        const owned_label = s.arena.dupe(u8, label.label) catch unreachable;
        instructions.append(.jump(owned_label));
    }

    // by this point we have added required jump instructions to correct cases
    // if nothing maches we need to go to end to the switch
    const switch_end_label = std.fmt.allocPrint(s.arena, "{s}_end", .{stmt.label.?}) catch unreachable;
    instructions.append(.jump(switch_end_label));

    // now add instruction for all cases
    var body = stmt.body;
    s.genBlockItem(&body, instructions);

    // this is to mark end of switch to jump if nothing matches
    instructions.append(.label(switch_end_label));
}

fn recurseGetGroupInnerExpr(expr: *Ast.Expr) *Ast.Expr {
    if (expr.* != .Group) return expr;
    return recurseGetGroupInnerExpr(expr.Group.expr);
}

fn genExpr(s: *Self, expr: *const Ast.Expr, instructions: *ArrayList(Tac.Instruction)) Tac.Val {
    return switch (expr.*) {
        .Cast => |cast_expr| {
            const result = s.genExpr(cast_expr.expr, instructions);
            const expr_type = expr.getType().?;
            if (isSameType(expr_type, cast_expr.expr.getType())) return result;
            const dst = s.makeVar(expr_type);
            switch (expr_type.*) {
                .Int => {
                    instructions.append(.truncate(result, dst));
                },
                .Long => instructions.append(.signExtended(result, dst)),
                else => std.debug.panic("** Compiler Bug ** - cast to unknown type: {any}", .{expr_type}),
            }
            return dst;
        },
        .Prefix => |prefix_expr| {
            const inner_expr_result = s.genExpr(prefix_expr.expr, instructions);
            const one: Tac.Val = .constant(.int(1));
            const dst = s.makeVar(expr.getType().?);
            switch (prefix_expr.operator) {
                .Add => {
                    instructions.append(.binary(.Add, inner_expr_result, one, dst));
                    instructions.append(.copy(dst, inner_expr_result));
                },
                .Subtract => {
                    instructions.append(.binary(.Subtract, inner_expr_result, one, dst));
                    instructions.append(.copy(dst, inner_expr_result));
                },
            }
            return inner_expr_result;
        },
        .FnCall => |fn_call| {
            var args = ArrayList(Tac.Val).init(s.arena);

            for (fn_call.args.items) |arg| {
                const arg_val = s.genExpr(arg, instructions);
                args.append(arg_val);
            }
            const dst = s.makeVar(expr.getType().?);
            instructions.append(.fnCall(fn_call.ident, args, dst));
            return dst;
        },
        .Ternary => |ternary_expr| {
            const dst = s.makeVar(expr.getType().?);

            const ternary_end = s.makeLabel("ternary_end");
            const ternary_else = s.makeLabel("ternary_else");

            const condition = s.genExpr(ternary_expr.condition, instructions);

            instructions.append(.jumpIfZero(condition, ternary_else.Label));

            // true
            const then_result = s.genExpr(ternary_expr.then, instructions);
            instructions.append(.copy(then_result, dst));
            instructions.append(.jump(ternary_end.Label));

            // false
            instructions.append(ternary_else);
            const else_result = s.genExpr(ternary_expr.@"else", instructions);
            instructions.append(.copy(else_result, dst));

            instructions.append(ternary_end);
            return dst;
        },
        .Postfix => |postfix| {
            const expr_result = s.genExpr(postfix.expr, instructions);
            const prev_result_var = s.makeVar(postfix.expr.getType().?);
            const prev_value: Tac.Instruction = .copy(expr_result, prev_result_var);

            instructions.append(prev_value);

            const one: Tac.Val = .constant(.int(1));
            switch (postfix.operator) {
                .Add => {
                    const dst = s.makeVar(postfix.expr.getType().?);
                    instructions.append(.binary(.Add, expr_result, one, dst));
                    instructions.append(.copy(dst, expr_result));
                },
                .Subtract => {
                    const dst = s.makeVar(postfix.expr.getType().?);
                    instructions.append(.binary(.Subtract, expr_result, one, dst));
                    instructions.append(.copy(dst, expr_result));
                },
            }
            return prev_result_var;
        },
        .Var => |variable| .variable(s.arena.dupe(u8, variable.ident) catch unreachable),
        .Assignment => |assignment| {
            const result = s.genExpr(assignment.src, instructions);
            if (assignment.dst.* != .Var) {
                std.debug.panic("** Compiler Bug ** assignment dst has to be a var. Sema phase should have caught this!!!", .{});
            }
            const dst: Tac.Val = .variable(s.arena.dupe(u8, assignment.dst.Var.ident) catch unreachable);
            instructions.append(.copy(result, dst));
            return dst;
        },
        .Constant => |constant| .constant(constant.value),
        .Unary => |unary| {
            const src = s.genExpr(unary.expr, instructions);
            const dst = s.makeVar(unary.expr.getType().?);
            const operator: Tac.UnaryOperator = switch (unary.operator) {
                .Negate => .Negate,
                .BitNot => .BitNot,
                .Not => .Not,
            };
            instructions.append(.unary(operator, src, dst));
            return dst;
        },
        .Binary => |binary| {
            switch (binary.operator) {
                .And => {
                    // @todo improvement
                    // All labels on this phase can use same index, so we can cross reference all labels in assembly
                    const false_label = s.makeLabel("and_condition_false");
                    const src1 = s.genExpr(binary.left, instructions);
                    // short-circuit evaluation for 'and'
                    instructions.append(.jumpIfZero(src1, false_label.Label));
                    const src2 = s.genExpr(binary.right, instructions);
                    instructions.append(.jumpIfZero(src2, false_label.Label));

                    // both conditions are true, dst = 1
                    const result = s.makeVar(.intType(s.arena));
                    const one: Tac.Val = .constant(.int(1));
                    instructions.append(.copy(one, result));
                    const end_label = s.makeLabel("and_condition_end");
                    instructions.append(.jump(end_label.Label));

                    // if condition is false
                    instructions.append(false_label);
                    const zero: Tac.Val = .constant(.int(0));
                    instructions.append(.copy(zero, result));

                    // end
                    instructions.append(end_label);
                    return result;
                },
                .Or => {
                    // @todo improvement
                    // all lables to use same label index
                    const true_label = s.makeLabel("or_condition_true");

                    const src1 = s.genExpr(binary.left, instructions);
                    instructions.append(.jumpIfNotZero(src1, true_label.Label));

                    const src2 = s.genExpr(binary.right, instructions);
                    instructions.append(.jumpIfNotZero(src2, true_label.Label));

                    const result = s.makeVar(.intType(s.arena));
                    const zero: Tac.Val = .constant(.int(0));
                    instructions.append(.copy(zero, result));

                    const end_label = s.makeLabel("or_condition_end");
                    instructions.append(.jump(end_label.Label));
                    instructions.append(true_label);

                    const one: Tac.Val = .constant(.int(1));
                    instructions.append(.copy(one, result));
                    instructions.append(end_label);

                    return result;
                },
                else => {
                    const left = s.genExpr(binary.left, instructions);
                    const right = s.genExpr(binary.right, instructions);
                    const dst = s.makeVar(binary.left.getType().?);
                    const op = switch (binary.operator) {
                        .Add => Tac.BinaryOperator.Add,
                        .Subtract => Tac.BinaryOperator.Subtract,
                        .Multiply => Tac.BinaryOperator.Multiply,
                        .Divide => Tac.BinaryOperator.Divide,
                        .Mod => Tac.BinaryOperator.Mod,
                        .BitAnd => Tac.BinaryOperator.BitAnd,
                        .BitOr => Tac.BinaryOperator.BitOr,
                        .BitXor => Tac.BinaryOperator.BitXor,
                        .LeftShift => Tac.BinaryOperator.LeftShift,
                        .RightShift => Tac.BinaryOperator.RightShift,
                        .EqualEqual => Tac.BinaryOperator.EqualEqual,
                        .NotEqual => Tac.BinaryOperator.NotEqual,
                        .LessThan => Tac.BinaryOperator.LessThan,
                        .LessThanEqual => Tac.BinaryOperator.LessThanEqual,
                        .GreaterThan => Tac.BinaryOperator.GreaterThan,
                        .GreaterThanEqual => Tac.BinaryOperator.GreaterThanEqual,
                        .And, .Or => std.debug.panic("** Compiler Bug ** - 'and' and 'or' should be handled differently!!!", .{}),
                    };
                    instructions.append(.binary(op, left, right, dst));
                    return dst;
                },
            }
        },
        .Group => |group| s.genExpr(group.expr, instructions),
    };
}

pub fn makeVar(s: *Self, typez: *Ast.BuiltinType) Tac.Val {
    defer s.var_count += 1;
    const var_name = std.fmt.allocPrint(s.arena, "tmp.{d}", .{s.var_count}) catch unreachable;
    s.symbol_table.put(var_name, .localVarSymbol(s.symbol_table.arena, var_name, typez.clone(s.arena)));
    return .variable(var_name);
}

pub fn makeLabel(s: *Self, label: []const u8) Tac.Instruction {
    defer s.label_count += 1;
    const label_name = std.fmt.allocPrint(s.arena, "L_{s}_{d}", .{ label, s.label_count }) catch unreachable;
    return .label(label_name);
}

pub const Tac = struct {
    pub const Program = struct {
        decls: ArrayList(TopLevelDecl),
    };

    pub const TopLevelDecl = union(enum) {
        Fn: FnDecl,
        StaticVar: StaticVarDecl,

        pub const StaticVarDecl = struct { attribute: Attribute, type: *Ast.BuiltinType, initializer: Symbol.StaticInit };
        pub const FnDecl = struct { attribute: Attribute, instructions: ArrayList(Instruction), type: *Ast.BuiltinType };

        pub const Attribute = struct {
            ident: []const u8,
            global: bool,
        };

        pub fn topLevelFn(ident: []const u8, global: bool, typez: *Ast.BuiltinType, instructions: ArrayList(Instruction)) @This() {
            return .{
                .Fn = .{
                    .attribute = .{ .ident = ident, .global = global },
                    .type = typez,
                    .instructions = instructions,
                },
            };
        }
        pub fn topLevelStaticVar(ident: []const u8, global: bool, typez: *Ast.BuiltinType, initializer: Symbol.StaticInit) @This() {
            return .{
                .StaticVar = .{
                    .attribute = .{ .ident = ident, .global = global },
                    .type = typez,
                    .initializer = initializer,
                },
            };
        }
    };

    pub const Instruction = union(enum) {
        Return: Val,
        Unary: struct { operator: UnaryOperator, src: Val, dst: Val },
        Binary: struct { operator: BinaryOperator, left: Val, right: Val, dst: Val },
        Copy: struct { src: Val, dst: Val },
        Jump: []const u8,
        JumpIfZero: struct { condition: Val, label: []const u8 },
        JumpIfNotZero: struct { condition: Val, label: []const u8 },
        Label: []const u8,
        FnCall: struct { ident: []const u8, args: ArrayList(Val), dst: Val },
        SignExtended: struct { src: Val, dst: Val },
        Truncate: struct { src: Val, dst: Val },

        pub fn signExtended(src: Val, dst: Val) Instruction {
            return .{ .SignExtended = .{ .src = src, .dst = dst } };
        }

        pub fn truncate(src: Val, dst: Val) Instruction {
            return .{ .Truncate = .{ .src = src, .dst = dst } };
        }

        pub fn fnCall(ident: []const u8, args: ArrayList(Val), dst: Val) Instruction {
            return .{
                .FnCall = .{
                    .ident = ident,
                    .args = args,
                    .dst = dst,
                },
            };
        }

        pub fn label(name: []const u8) Instruction {
            return .{ .Label = name };
        }

        pub fn jumpIfNotZero(condition: Val, label_name: []const u8) Instruction {
            return .{ .JumpIfNotZero = .{ .condition = condition, .label = label_name } };
        }

        pub fn jumpIfZero(condition: Val, label_name: []const u8) Instruction {
            return .{ .JumpIfZero = .{ .condition = condition, .label = label_name } };
        }

        pub fn jump(label_name: []const u8) Instruction {
            return .{ .Jump = label_name };
        }

        pub fn copy(src: Val, dst: Val) Instruction {
            return .{ .Copy = .{ .src = src, .dst = dst } };
        }

        pub fn binary(operator: BinaryOperator, left: Val, right: Val, dst: Val) Instruction {
            return .{
                .Binary = .{
                    .operator = operator,
                    .left = left,
                    .right = right,
                    .dst = dst,
                },
            };
        }

        pub fn ret(val: Val) Instruction {
            return .{ .Return = val };
        }

        pub fn unary(operator: UnaryOperator, src: Val, dst: Val) Instruction {
            return .{
                .Unary = .{
                    .operator = operator,
                    .src = src,
                    .dst = dst,
                },
            };
        }
    };
    pub const Val = union(enum) {
        Const: Ast.Constant,
        Var: []const u8,

        pub fn constant(value: Ast.Constant) Val {
            return .{ .Const = value };
        }

        pub fn variable(ident: []const u8) Val {
            return .{ .Var = ident };
        }
    };
    pub const UnaryOperator = enum {
        Negate,
        BitNot,
        Not,
    };
    pub const BinaryOperator = enum {
        Add,
        Subtract,
        Multiply,
        Divide,
        Mod,
        LeftShift,
        RightShift,
        BitAnd,
        BitOr,
        BitXor,
        EqualEqual,
        NotEqual,
        LessThan,
        LessThanEqual,
        GreaterThan,
        GreaterThanEqual,
    };
};

const TackyIRPrinter = struct {
    writer: *std.Io.Writer,

    pub fn print(writer: *std.Io.Writer, pg: Tac.Program) void {
        const s = @This(){ .writer = writer };
        s.printPg(pg);
    }

    fn printPg(s: @This(), pg: Tac.Program) void {
        s.write("-- Tacky IR --\n");
        for (pg.decls.items) |decl| {
            switch (decl) {
                .Fn => |fn_decl| s.printFnDecl(fn_decl),
                .StaticVar => |static_var| s.printStaticVar(static_var),
            }
        }
    }
    fn printStaticVar(s: @This(), static_var: Tac.TopLevelDecl.StaticVarDecl) void {
        s.write("StaticVar(");
        s.writeFmt("ident: {s}, global: {any}", .{ static_var.attribute.ident, static_var.attribute.global });
        switch (static_var.initializer) {
            .Int => |i| s.writeFmt(", init: {d}", .{i}),
            .Long => |l| s.writeFmt(", init: {d}", .{l}),
        }
        s.write(")\n");
    }

    fn printFnDecl(s: @This(), fn_defn: Tac.TopLevelDecl.FnDecl) void {
        s.write(fn_defn.attribute.ident);
        s.write(":\n");
        for (fn_defn.instructions.items) |inst| {
            s.printInst(inst);
        }
    }
    fn printInst(s: @This(), inst: Tac.Instruction) void {
        s.write("  ");
        switch (inst) {
            .Truncate => |truncate| {
                s.write("Truncate(");
                s.write("src: ");
                s.printVal(truncate.src);
                s.write(", dst: ");
                s.printVal(truncate.dst);
                s.write(")");
            },
            .SignExtended => |sign_extended| {
                s.write("SignExtended(");
                s.write("src: ");
                s.printVal(sign_extended.src);
                s.write(", dst: ");
                s.printVal(sign_extended.dst);
                s.write(")");
            },
            .FnCall => |fn_call| {
                s.write("FnCall(");
                s.write(fn_call.ident);
                s.write(", args: [");
                for (fn_call.args.items, 0..) |arg, i| {
                    s.printVal(arg);
                    if (i < fn_call.args.items.len - 1) s.write(", ");
                }
                s.write("], dst: ");
                s.printVal(fn_call.dst);
                s.write(")");
            },
            .Return => |ret| {
                s.write("Return(");
                s.printVal(ret);
                s.write(")");
            },
            .Unary => |unary| {
                s.write("Unary(");
                s.write("operator: ");
                switch (unary.operator) {
                    .Negate => s.write("Negate"),
                    .BitNot => s.write("BitNot"),
                    .Not => s.write("Not"),
                }
                s.write(", ");
                s.write("src: ");
                s.printVal(unary.src);
                s.write(", ");
                s.write("dst: ");
                s.printVal(unary.dst);
                s.write(")");
            },
            .Binary => |binary| {
                s.write("Binary(");
                s.write("operator: ");
                switch (binary.operator) {
                    .Add => s.write("Add"),
                    .Subtract => s.write("Subtract"),
                    .Multiply => s.write("Multiply"),
                    .Divide => s.write("Divide"),
                    .Mod => s.write("Mod"),
                    .LeftShift => s.write("LeftShift"),
                    .RightShift => s.write("RightShift"),
                    .BitAnd => s.write("BitAnd"),
                    .BitOr => s.write("BitOr"),
                    .BitXor => s.write("BitXor"),
                    .EqualEqual => s.write("EqualEqual"),
                    .NotEqual => s.write("NotEqual"),
                    .LessThan => s.write("LessThan"),
                    .LessThanEqual => s.write("LessThanEqual"),
                    .GreaterThan => s.write("GreaterThan"),
                    .GreaterThanEqual => s.write("GreaterThanEqual"),
                }
                s.write(", ");

                s.write("dst: ");
                s.printVal(binary.dst);

                s.write(", left: ");
                s.printVal(binary.left);

                s.write(", right: ");
                s.printVal(binary.right);

                s.write(")");
            },
            .Copy => |copy| {
                s.write("Copy(");
                s.write("src: ");
                s.printVal(copy.src);
                s.write(", dst: ");
                s.printVal(copy.dst);
                s.write(")");
            },
            .Jump => |label| {
                s.write("Jump(");
                s.write(label);
                s.write(")");
            },
            .JumpIfZero => |jump| {
                s.write("JumpIfZero(");
                s.write("condition: ");
                s.printVal(jump.condition);
                s.write(", label: ");
                s.write(jump.label);
                s.write(")");
            },
            .JumpIfNotZero => |jump| {
                s.write("JumpIfNotZero(");
                s.write("condition: ");
                s.printVal(jump.condition);
                s.write(", label: ");
                s.write(jump.label);
                s.write(")");
            },
            .Label => |label| {
                s.write("Label(");
                s.write(label);
                s.write(")");
            },
        }
        s.write("\n");
    }

    fn printVal(s: @This(), val: Tac.Val) void {
        s.write("Val(");
        switch (val) {
            .Const => |c| {
                switch (c) {
                    .Int => |i| s.writeFmt("{d}", .{i}),
                    .Long => |l| s.writeFmt("{d}", .{l}),
                }
            },
            .Var => |v| s.writeFmt("{s}", .{v}),
        }
        s.write(")");
    }

    fn write(s: @This(), bytes: []const u8) void {
        _ = s.writer.write(bytes) catch unreachable;
    }
    fn writeFmt(s: @This(), comptime fmt: []const u8, args: anytype) void {
        _ = s.writer.print(fmt, args) catch unreachable;
    }
};

// const ArrayList = std.ArrayList;

const std = @import("std");
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;

const ArrayList = @import("from_scratch.zig").ArrayList;
const AstParser = @import("AstParser.zig");
const Ast = AstParser.Ast;
const Printer = @import("util.zig").Printer;
const SymbolTable = @import("SymbolTable.zig");
const Symbol = SymbolTable.Symbol;
const isSameType = AstParser.isSameType;
