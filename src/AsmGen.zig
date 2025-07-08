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
            const inst: Asm.Instruction = .mov(.imm(expr.Constant.value), .register(.rax));
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
        Register: Register,

        pub fn imm(value: i64) Operand {
            return .{ .Imm = value };
        }
        pub fn register(reg: Register) Operand {
            return .{ .Register = reg };
        }
    };
    pub const Register = enum {
        rax,
        rdx,
    };
};

const Printer = struct {
    writer: AnyWriter,

    pub fn print(writer: AnyWriter, pg: *const Asm.Program) void {
        const s = Printer{ .writer = writer };
        s.write("-- CodeGen --\n");
        s.printFnDecl(&pg.fn_defn, 0);
        s.write("\n");
    }
    fn printFnDecl(s: *const @This(), fn_defn: *const Asm.FnDefn, depth: usize) void {
        s.printSpace(depth);
        s.write_fmt("{s}:\n", .{fn_defn.name});
        for (fn_defn.instructions.items) |*inst| {
            s.printInst(inst, depth + 1);
        }
    }
    fn printInst(s: *const @This(), inst: *const Asm.Instruction, depth: usize) void {
        s.printSpace(depth);
        switch (inst.*) {
            .Mov => |mov_inst| {
                s.write("mov ");
                s.printOperand(&mov_inst.src);
                s.write(", ");
                s.printOperand(&mov_inst.dst);
            },
            .Ret => {
                s.write("ret");
            },
        }
        s.write("\n");
    }

    fn printOperand(s: *const @This(), op: *const Asm.Operand) void {
        switch (op.*) {
            .Imm => |imm| s.write_fmt("Imm({d})", .{imm}),
            .Register => |r| s.printRegister(r),
        }
    }

    fn printRegister(s: *const @This(), reg: Asm.Register) void {
        const reg_str = switch (reg) {
            .rax => "rax",
            .rdx => "rdx",
        };
        s.write_fmt("Register({s})", .{reg_str});
    }

    fn printSpace(s: *const @This(), depth: usize) void {
        for (0..depth) |_| s.write("  ");
    }

    fn write(s: *const @This(), bytes: []const u8) void {
        _ = s.writer.write(bytes) catch unreachable;
    }
    fn write_fmt(s: *const @This(), comptime fmt: []const u8, args: anytype) void {
        _ = s.writer.print(fmt, args) catch unreachable;
    }
};

const std = @import("std");
const Ast = @import("AstParser.zig").Ast;
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;
const ArrayList = std.ArrayList;
