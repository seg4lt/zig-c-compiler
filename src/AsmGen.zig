arena: Allocator,
scratch_arena: Allocator,
const Self = @This();

const Options = struct {
    arena: Allocator,
    scratch_arena: Allocator,
    pg: *const Ast.Program,
    print_codegen: bool = false,
};

pub fn asmGen(opt: Options) Asm.Program {
    const self = Self{ .arena = opt.arena, .scratch_arena = opt.scratch_arena };

    const pg = self.genProgram(opt.pg);
    if (opt.print_codegen) {
        const writer = std.io.getStdOut().writer().any();
        Printer.print(writer, &pg);
    }
    return pg;
}
fn genProgram(self: *const Self, pg: *const Ast.Program) Asm.Program {
    const fn_defn = self.genFn(pg.@"fn");
    return .{ .fn_defn = fn_defn };
}

fn genFn(self: *const Self, fn_defn: *const Ast.FnDecl) Asm.FnDefn {
    var instructions = Asm.Instructions.init(self.arena);
    self.genStmt(fn_defn.body, &instructions);
    return .{
        .name = fn_defn.name,
        .instructions = instructions,
    };
}

fn genStmt(self: *const Self, stmt: *const Ast.Stmt, insts: *Asm.Instructions) void {
    _ = self;
    switch (stmt.*) {
        .Return => |ret_stmt| {
            const expr = ret_stmt.expr;
            const inst: Asm.Instruction = .mov(.imm(expr.Constant.value), .Register);
            insts.append(inst) catch unreachable;
            insts.append(.Ret) catch unreachable;
        },
    }
}

pub const Asm = struct {
    pub const Program = struct {
        fn_defn: FnDefn,
    };
    pub const FnDefn = struct {
        name: []const u8,
        instructions: std.ArrayList(Instruction),
    };
    const Instructions = ArrayList(Instruction);
    pub const Instruction = union(enum) {
        Mov: struct { src: Operand, dst: Operand },
        Ret,

        pub fn mov(src: Operand, dst: Operand) Instruction {
            return .{ .Mov = .{ .src = src, .dst = dst } };
        }
    };
    pub const Operand = union(enum) {
        Imm: i64,
        Register,

        pub fn imm(value: i64) Operand {
            return .{ .Imm = value };
        }
    };
};

const Printer = struct {
    pub fn print(writer: AnyWriter, pg: *const Asm.Program) void {
        write(writer, "-- CodeGen --\n");
        printFnDecl(writer, &pg.fn_defn, 0);
        write(writer, "\n");
    }
    fn printFnDecl(writer: AnyWriter, fn_defn: *const Asm.FnDefn, depth: usize) void {
        printSpace(writer, depth);
        write_fmt(writer, "{s}:\n", .{fn_defn.name});
        for (fn_defn.instructions.items) |*inst| {
            printInst(writer, inst, depth + 1);
        }
    }
    fn printInst(writer: AnyWriter, inst: *const Asm.Instruction, depth: usize) void {
        printSpace(writer, depth);
        switch (inst.*) {
            .Mov => |mov_inst| {
                write(writer, "mov ");
                printOperand(writer, &mov_inst.src);
                write(writer, ", ");
                printOperand(writer, &mov_inst.dst);
            },
            .Ret => {
                write(writer, "ret");
            },
        }
        write(writer, "\n");
    }

    fn printOperand(writer: AnyWriter, op: *const Asm.Operand) void {
        switch (op.*) {
            .Imm => |imm| write_fmt(writer, "Imm({d})", .{imm}),
            .Register => write(writer, "Register()"),
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
const Ast = @import("AstParser.zig").Ast;
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;
const ArrayList = std.ArrayList;
