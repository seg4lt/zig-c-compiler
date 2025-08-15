arena: Allocator,
var_count: usize = 0,
label_count: usize = 0,

const Self = @This();

const Options = struct {
    arena: Allocator,
    pg: *const Ast.Program,
    print: bool = false,
};

pub fn genTacky(opt: Options) Tac.Program {
    var self = Self{ .arena = opt.arena };
    const pg = self.genPg(opt.pg);
    if (opt.print) TackyIRPrinter.print(std.io.getStdErr().writer().any(), pg);
    return pg;
}

fn genPg(s: *Self, pg: *const Ast.Program) Tac.Program {
    var fns = ArrayList(Tac.FnDefn).init(s.arena);

    for (pg.fns.items) |fn_decl| {
        if (fn_decl.body != null) {
            const fn_tacky = s.genFnDefn(fn_decl);
            fns.append(fn_tacky) catch unreachable;
        }
    }
    return .{ .fns = fns };
}

fn genFnDefn(s: *Self, fn_decl: *const Ast.FnDecl) Tac.FnDefn {
    var instructions = ArrayList(Tac.Instruction).init(s.arena);

    if (fn_decl.body) |body| {
        s.genBlock(body, &instructions);
        instructions.append(.ret(.constant(0))) catch unreachable;
    }
    return .{
        .name = std.fmt.allocPrint(s.arena, "{s}", .{fn_decl.name}) catch unreachable,
        .body = instructions,
    };
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
            if (var_decl.init) |initializer| {
                const result = s.genExpr(initializer, instructions);
                const dst: Tac.Val = .variable(std.fmt.allocPrint(s.arena, "{s}", .{var_decl.ident}) catch unreachable);
                instructions.append(.copy(result, dst)) catch unreachable;
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
            const owned_ident = std.fmt.allocPrint(s.arena, "{s}", .{case_stmt.label.?}) catch unreachable;
            instructions.append(.label(owned_ident)) catch unreachable;
        },
        .Default => |default_stmt| {
            if (default_stmt.label == null) {
                std.debug.panic("** Compiler Bug ** default statement should have label as part of sema loop labeling phase", .{});
            }
            const owned_ident = std.fmt.allocPrint(s.arena, "{s}", .{default_stmt.label.?}) catch unreachable;
            instructions.append(.label(owned_ident)) catch unreachable;
        },
        .Break => |break_stmt| {
            if (break_stmt.ident == null) {
                std.debug.panic("** Compiler Bug ** break statement should have label as part of sema loop labeling phase", .{});
            }
            // Any other way?
            // this label needs to match with once defined in the loop
            const jump_label = std.fmt.allocPrint(s.arena, "{s}_end", .{break_stmt.ident.?}) catch unreachable;
            instructions.append(.jump(jump_label)) catch unreachable;
        },
        .Continue => |continue_stmt| {
            if (continue_stmt.ident == null) {
                std.debug.panic("** Compiler Bug ** continue statement should have label as part of sema loop labeling phase", .{});
            }
            // Any other way?
            // this label needs to match with once defined in the loop
            const jump_label = std.fmt.allocPrint(s.arena, "{s}_post_expr", .{continue_stmt.ident.?}) catch unreachable;
            instructions.append(.jump(jump_label)) catch unreachable;
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

            instructions.append(.label(loop_start_label)) catch unreachable;

            s.genStmt(do_stmt.body, instructions);

            instructions.append(.label(loop_post_expr_label)) catch unreachable;
            const condition = s.genExpr(do_stmt.condition, instructions);
            instructions.append(.jumpIfNotZero(condition, loop_start_label)) catch unreachable;

            instructions.append(.label(loop_end_label)) catch unreachable;
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

            instructions.append(.label(loop_start_label)) catch unreachable;
            instructions.append(.label(loop_post_expr_label)) catch unreachable;

            const condition = s.genExpr(while_stmt.condition, instructions);
            instructions.append(.jumpIfZero(condition, loop_end_label)) catch unreachable;

            s.genStmt(while_stmt.body, instructions);

            instructions.append(.jump(loop_post_expr_label)) catch unreachable;
            instructions.append(.label(loop_end_label)) catch unreachable;
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
            instructions.append(.jump(loop_start_label)) catch unreachable;

            instructions.append(.label(loop_post_expr_label)) catch unreachable;

            if (for_stmt.post) |post_expr| {
                _ = s.genExpr(post_expr, instructions);
            }

            // Loop start - includes the condition and jump to body if needed
            instructions.append(.label(loop_start_label)) catch unreachable;

            if (for_stmt.condition) |condition| {
                const condition_result = s.genExpr(condition, instructions);
                instructions.append(.jumpIfZero(condition_result, loop_end_label)) catch unreachable;
            }

            s.genStmt(for_stmt.body, instructions);

            instructions.append(.jump(loop_post_expr_label)) catch unreachable;
            instructions.append(.label(loop_end_label)) catch unreachable;
        },
        .Label => |label_stmt| {
            const owned_ident = std.fmt.allocPrint(s.arena, "{s}", .{label_stmt.ident}) catch unreachable;
            const label: Tac.Instruction = .label(owned_ident);

            instructions.append(label) catch unreachable;
            s.genStmt(label_stmt.stmt, instructions);
        },
        .Goto => |goto_stmt| {
            const owned_ident = std.fmt.allocPrint(s.arena, "{s}", .{goto_stmt.ident}) catch unreachable;
            instructions.append(.jump(owned_ident)) catch unreachable;
        },
        .If => |if_stmt| {
            const if_end_label = s.makeLabel("if_end");
            const condition = s.genExpr(if_stmt.condition, instructions);

            const else_block = if_stmt.@"else" orelse {
                instructions.append(.jumpIfZero(condition, if_end_label.Label)) catch unreachable;
                s.genStmt(if_stmt.then, instructions);
                instructions.append(if_end_label) catch unreachable;
                return;
            };
            const else_label = s.makeLabel("else");
            instructions.append(.jumpIfZero(condition, else_label.Label)) catch unreachable;

            s.genStmt(if_stmt.then, instructions);
            instructions.append(.jump(if_end_label.Label)) catch unreachable;

            instructions.append(else_label) catch unreachable;
            s.genStmt(else_block, instructions);

            instructions.append(if_end_label) catch unreachable;
        },
        .Compound => |compound_stmt| s.genBlock(compound_stmt.body, instructions),
        .Expr => |expr_stmt| _ = s.genExpr(expr_stmt.expr, instructions),
        .Return => |ret| {
            const ret_val = s.genExpr(ret.expr, instructions);
            instructions.append(.ret(ret_val)) catch unreachable;
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
        const case_value: Tac.Val = .constant(std.fmt.parseInt(u32, label.value, 10) catch unreachable);
        const condition = s.makeVar();
        instructions.append(.binary(.EqualEqual, condition_expr, case_value, condition)) catch unreachable;

        // we gen the instruction for the expr we created
        // if it mathces, we jump the the case label
        const owned_label = std.fmt.allocPrint(s.arena, "{s}", .{label.label}) catch unreachable;
        instructions.append(.jumpIfNotZero(condition, owned_label)) catch unreachable;
    }
    // handling default case separately as this is if nothing matches
    for (stmt.case_labels.?.items) |label| {
        if (!label.is_default) continue;
        const owned_label = std.fmt.allocPrint(s.arena, "{s}", .{label.label}) catch unreachable;
        instructions.append(.jump(owned_label)) catch unreachable;
    }

    // by this point we have added required jump instructions to correct cases
    // if nothing maches we need to go to end to the switch
    const switch_end_label = std.fmt.allocPrint(s.arena, "{s}_end", .{stmt.label.?}) catch unreachable;
    instructions.append(.jump(switch_end_label)) catch unreachable;

    // now add instruction for all cases
    var body = stmt.body;
    s.genBlockItem(&body, instructions);

    // this is to mark end of switch to jump if nothing matches
    instructions.append(.label(switch_end_label)) catch unreachable;
}

fn recurseGetGroupInnerExpr(expr: *Ast.Expr) *Ast.Expr {
    if (expr.* != .Group) return expr;
    return recurseGetGroupInnerExpr(expr.Group.expr);
}

fn genExpr(s: *Self, expr: *const Ast.Expr, instructions: *ArrayList(Tac.Instruction)) Tac.Val {
    return switch (expr.*) {
        .Prefix => |prefix_expr| {
            const inner_expr_result = s.genExpr(prefix_expr.expr, instructions);
            const one: Tac.Val = .constant(1);
            const dst = s.makeVar();
            switch (prefix_expr.operator) {
                .Add => {
                    instructions.append(.binary(.Add, inner_expr_result, one, dst)) catch unreachable;
                    instructions.append(.copy(dst, inner_expr_result)) catch unreachable;
                },
                .Subtract => {
                    instructions.append(.binary(.Subtract, inner_expr_result, one, dst)) catch unreachable;
                    instructions.append(.copy(dst, inner_expr_result)) catch unreachable;
                },
            }
            return inner_expr_result;
        },
        .FnCall => |fn_call| {
            var args = ArrayList(Tac.Val).init(s.arena);

            for (fn_call.args.items) |arg| {
                const arg_val = s.genExpr(arg, instructions);
                args.append(arg_val) catch unreachable;
            }
            const dst = s.makeVar();
            instructions.append(.fnCall(fn_call.ident, args, dst)) catch unreachable;
            return dst;
        },
        .Ternary => |ternary_expr| {
            const dst = s.makeVar();

            const ternary_end = s.makeLabel("ternary_end");
            const ternary_else = s.makeLabel("ternary_else");

            const condition = s.genExpr(ternary_expr.condition, instructions);

            instructions.append(.jumpIfZero(condition, ternary_else.Label)) catch unreachable;

            // true
            const then_result = s.genExpr(ternary_expr.then, instructions);
            instructions.append(.copy(then_result, dst)) catch unreachable;
            instructions.append(.jump(ternary_end.Label)) catch unreachable;

            // false
            instructions.append(ternary_else) catch unreachable;
            const else_result = s.genExpr(ternary_expr.@"else", instructions);
            instructions.append(.copy(else_result, dst)) catch unreachable;

            instructions.append(ternary_end) catch unreachable;
            return dst;
        },
        .Postfix => |postfix| {
            const expr_result = s.genExpr(postfix.expr, instructions);
            const prev_result_var = s.makeVar();
            const prev_value: Tac.Instruction = .copy(expr_result, prev_result_var);

            instructions.append(prev_value) catch unreachable;

            const one: Tac.Val = .constant(1);
            switch (postfix.operator) {
                .Add => {
                    const dst = s.makeVar();
                    instructions.append(.binary(.Add, expr_result, one, dst)) catch unreachable;
                    instructions.append(.copy(dst, expr_result)) catch unreachable;
                },
                .Subtract => {
                    const dst = s.makeVar();
                    instructions.append(.binary(.Subtract, expr_result, one, dst)) catch unreachable;
                    instructions.append(.copy(dst, expr_result)) catch unreachable;
                },
            }
            return prev_result_var;
        },
        .Var => |variable| .variable(std.fmt.allocPrint(s.arena, "{s}", .{variable.ident}) catch unreachable),
        .Assignment => |assignment| {
            const result = s.genExpr(assignment.src, instructions);
            if (assignment.dst.* != .Var) {
                std.debug.panic("** Compiler Bug ** assignment dst has to be a var. Sema phase should have caught this!!!", .{});
            }
            const dst: Tac.Val = .variable(std.fmt.allocPrint(s.arena, "{s}", .{assignment.dst.Var.ident}) catch unreachable);
            instructions.append(.copy(result, dst)) catch unreachable;
            return dst;
        },
        .Constant => |constant| .constant(constant.value),
        .Unary => |unary| {
            const src = s.genExpr(unary.expr, instructions);
            const dst = s.makeVar();
            const operator: Tac.UnaryOperator = switch (unary.operator) {
                .Negate => .Negate,
                .BitNot => .BitNot,
                .Not => .Not,
            };
            instructions.append(.unary(operator, src, dst)) catch unreachable;
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
                    instructions.append(.jumpIfZero(src1, false_label.Label)) catch unreachable;
                    const src2 = s.genExpr(binary.right, instructions);
                    instructions.append(.jumpIfZero(src2, false_label.Label)) catch unreachable;

                    // both conditions are true, dst = 1
                    const result = s.makeVar();
                    const one: Tac.Val = .constant(1);
                    instructions.append(.copy(one, result)) catch unreachable;
                    const end_label = s.makeLabel("and_condition_end");
                    instructions.append(.jump(end_label.Label)) catch unreachable;

                    // if condition is false
                    instructions.append(false_label) catch unreachable;
                    const zero: Tac.Val = .constant(0);
                    instructions.append(.copy(zero, result)) catch unreachable;

                    // end
                    instructions.append(end_label) catch unreachable;
                    return result;
                },
                .Or => {
                    // @todo improvement
                    // all lables to use same label index
                    const true_label = s.makeLabel("or_condition_true");

                    const src1 = s.genExpr(binary.left, instructions);
                    instructions.append(.jumpIfNotZero(src1, true_label.Label)) catch unreachable;

                    const src2 = s.genExpr(binary.right, instructions);
                    instructions.append(.jumpIfNotZero(src2, true_label.Label)) catch unreachable;

                    const result = s.makeVar();
                    const zero: Tac.Val = .constant(0);
                    instructions.append(.copy(zero, result)) catch unreachable;

                    const end_label = s.makeLabel("or_condition_end");
                    instructions.append(.jump(end_label.Label)) catch unreachable;
                    instructions.append(true_label) catch unreachable;

                    const one: Tac.Val = .constant(1);
                    instructions.append(.copy(one, result)) catch unreachable;
                    instructions.append(end_label) catch unreachable;

                    return result;
                },
                else => {
                    const left = s.genExpr(binary.left, instructions);
                    const right = s.genExpr(binary.right, instructions);
                    const dst = s.makeVar();
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
                    instructions.append(.binary(op, left, right, dst)) catch unreachable;
                    return dst;
                },
            }
        },
        .Group => |group| s.genExpr(group.expr, instructions),
    };
}

pub fn makeVar(s: *Self) Tac.Val {
    defer s.var_count += 1;
    const var_name = std.fmt.allocPrint(s.arena, "tmp.{d}", .{s.var_count}) catch unreachable;
    return .variable(var_name);
}

pub fn makeLabel(s: *Self, label: []const u8) Tac.Instruction {
    defer s.label_count += 1;
    const label_name = std.fmt.allocPrint(s.arena, "L_{s}_{d}", .{ label, s.label_count }) catch unreachable;
    return .label(label_name);
}

pub const Tac = struct {
    pub const Program = struct {
        fns: ArrayList(FnDefn),
    };
    pub const FnDefn = struct {
        name: []const u8,
        body: ArrayList(Instruction),
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
        Const: i64,
        Var: []const u8,

        pub fn constant(value: i64) Val {
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
    writer: AnyWriter,

    pub fn print(writer: AnyWriter, pg: Tac.Program) void {
        const s = @This(){ .writer = writer };
        s.printPg(pg);
    }

    fn printPg(s: @This(), pg: Tac.Program) void {
        s.write("-- Tacky IR --\n");
        for (pg.fns.items) |fn_defn| {
            s.printFnDecl(fn_defn);
        }
    }
    fn printFnDecl(s: @This(), fn_defn: Tac.FnDefn) void {
        s.write(fn_defn.name);
        s.write(":\n");
        for (fn_defn.body.items) |inst| {
            s.printInst(inst);
        }
    }
    fn printInst(s: @This(), inst: Tac.Instruction) void {
        s.write("  ");
        switch (inst) {
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
            .Const => |c| s.writeFmt("{d}", .{c}),
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

const std = @import("std");
const Ast = @import("AstParser.zig").Ast;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;
