arena: Allocator,
var_count: usize = 0,

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

    s.genStmt(fn_decl.body, &instructions);

    instructions.append(.ret(.constant(0))) catch unreachable;
    return .{
        .name = fn_decl.name,
        .body = instructions,
    };
}
pub fn genStmt(s: *Self, stmt: *const Ast.Stmt, instructions: *ArrayList(Tac.Instruction)) void {
    switch (stmt.*) {
        .Return => |ret| {
            const ret_val = s.genExpr(ret.expr, instructions);
            instructions.append(.ret(ret_val)) catch unreachable;
        },
    }
}
pub fn genExpr(s: *Self, expr: *const Ast.Expr, instructions: *ArrayList(Tac.Instruction)) Tac.Val {
    return switch (expr.*) {
        .Constant => |constant| .constant(constant.value),
        .Unary => |unary| {
            const src = s.genExpr(unary.expr, instructions);
            const dst = s.makeVar();
            const operator: Tac.UnaryOperator = switch (unary.operator) {
                .Negate => .Negate,
                .BitNot => .BitNot,
            };
            instructions.append(.unary(operator, src, dst)) catch unreachable;
            return dst;
        },
        .Binary => |binary| {
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
            };
            instructions.append(.binary(op, left, right, dst)) catch unreachable;
            return dst;
        },
    };
}
pub fn makeVar(s: *Self) Tac.Val {
    const var_name = std.fmt.allocPrint(s.arena, "tmp.{d}", .{s.var_count}) catch unreachable;
    s.var_count += 1;
    return .variable(var_name);
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
        Unary: struct {
            operator: UnaryOperator,
            src: Val,
            dst: Val,
        },
        Binary: struct {
            operator: BinaryOperator,
            left: Val,
            right: Val,
            dst: Val,
        },

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
