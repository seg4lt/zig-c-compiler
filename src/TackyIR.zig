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
    const fn_defn = s.genFnDefn(pg.@"fn");
    const pg_tac = Tac.Program{ .fn_defn = fn_defn };
    return pg_tac;
}

fn genFnDefn(s: *Self, fn_decl: *const Ast.FnDecl) Tac.FnDefn {
    var instructions = ArrayList(Tac.Instruction).init(s.arena);

    s.genBlock(fn_decl.body, &instructions);
    instructions.append(.ret(.constant(0))) catch unreachable;

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
            _ = fn_decl;
            @panic("TODO: not implemented");
            // if (fn_decl.body != null) {
            //     @panic("** Compiler Bug ** fn declaration should not have body");
            // }
        },
    }
}

fn genStmt(s: *Self, stmt: *const Ast.Stmt, instructions: *ArrayList(Tac.Instruction)) void {
    switch (stmt.*) {
        .Expr => |expr_stmt| {
            _ = s.genExpr(expr_stmt.expr, instructions);
        },
        .Return => |ret| {
            const ret_val = s.genExpr(ret.expr, instructions);
            instructions.append(.ret(ret_val)) catch unreachable;
        },
        .Null => {}, //noop
    }
}
pub fn genExpr(s: *Self, expr: *const Ast.Expr, instructions: *ArrayList(Tac.Instruction)) Tac.Val {
    return switch (expr.*) {
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
                @panic("** Compiler Bug ** assignment dst has to be a var. Sema phase should have caught this!!!");
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
                        .And, .Or => @panic("** Compiler Bug ** - 'and' and 'or' should be handled differently!!!"),
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
pub fn makeLabel(s: *Self, comptime label: []const u8) Tac.Instruction {
    defer s.label_count += 1;
    const label_name = std.fmt.allocPrint(s.arena, "L_{s}_{d}", .{ label, s.label_count }) catch unreachable;
    return .label(label_name);
}

pub const Tac = struct {
    pub const Program = struct {
        fn_defn: FnDefn,
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
        s.printFnDecl(pg.fn_defn);
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
