const Options = struct {
    arena: Allocator,
    scratch_arena: Allocator,
    pg: Tac.Program,
    print_codegen: bool = false,
};

pub fn emit(opt: Options) Asm.Program {
    var pg = Stage1.init(opt.arena).emitPg(opt.pg);
    if (opt.print_codegen) Printer.print(std.io.getStdOut().writer().any(), pg, "Stage 1");

    Stage2.init(opt.arena).processPg(&pg);
    if (opt.print_codegen) Printer.print(std.io.getStdOut().writer().any(), pg, "Stage 2");

    return pg;
}
const Stage2 = struct {
    arena: Allocator,
    variable_map: *HashMap(i64),

    pub fn init(arena: Allocator) Stage2 {
        const map = arena.create(HashMap(i64)) catch unreachable;
        map.* = HashMap(i64).init(arena);
        return .{ .arena = arena, .variable_map = map };
    }

    pub fn processPg(s: @This(), pg: *Asm.Program) void {
        s.processFn(&pg.fn_defn);
    }
    fn processFn(s: @This(), fn_defn: *Asm.FnDefn) void {
        for (fn_defn.instructions.items) |*inst| {
            s.processInst(inst);
        }
    }
    fn processInst(s: @This(), inst: *Asm.Instruction) void {
        switch (inst.*) {
            .Mov => |*mov| {
                mov.src = s.mapOperandToStack(mov.src);
                mov.dst = s.mapOperandToStack(mov.dst);
            },
            .Unary => |*unary| {
                unary.operand = s.mapOperandToStack(unary.operand);
            },
            .AllocateStack, .Ret => {},
        }
    }
    fn mapOperandToStack(s: @This(), operand: Asm.Operand) Asm.Operand {
        return switch (operand) {
            .Pseudo => |ident| {
                if (s.variable_map.getEntry(ident)) |saved_offset| {
                    return .stack(saved_offset.value_ptr.*);
                }
                const new_offset: i64 = @as(i64, @intCast(s.variable_map.count() + 1)) * -4;
                s.variable_map.put(ident, new_offset) catch unreachable;
                return .stack(new_offset);
            },
            else => return operand,
        };
    }
};

const Stage1 = struct {
    arena: Allocator,

    const Self = @This();

    fn init(arena: Allocator) Self {
        return .{
            .arena = arena,
        };
    }

    fn emitPg(s: Self, pg: Tac.Program) Asm.Program {
        const fn_defn = s.emitFnDefn(pg.fn_defn);
        return .{ .fn_defn = fn_defn };
    }

    fn emitFnDefn(s: Self, fn_defn: Tac.FnDefn) Asm.FnDefn {
        var instructions = Asm.Instructions.init(s.arena);

        for (fn_defn.body.items) |inst| {
            s.emitInst(inst, &instructions);
        }
        return .{ .name = fn_defn.name, .instructions = instructions };
    }

    fn emitInst(s: Self, inst: Tac.Instruction, instructions: *Asm.Instructions) void {
        _ = s;
        switch (inst) {
            .Return => |ret| {
                const src = valToOperand(ret);
                const dst = Asm.Operand.register(.rax);

                instructions.append(.mov(src, dst)) catch unreachable;
                instructions.append(.ret()) catch unreachable;
            },
            .Unary => |unary| {
                const src = valToOperand(unary.src);
                const dst = valToOperand(unary.dst);
                instructions.append(.mov(src, dst)) catch unreachable;
                const operator: Asm.UnaryOperator = switch (unary.operator) {
                    .Negate => .neg,
                    .Complement => .not,
                };
                instructions.append(.unary(operator, dst)) catch unreachable;
            },
        }
    }
    fn valToOperand(val: Tac.Val) Asm.Operand {
        return switch (val) {
            .Const => |constant| .imm(constant),
            .Var => |ident| .pseudo(ident),
        };
    }
};

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
        Unary: struct { operator: UnaryOperator, operand: Operand },
        AllocateStack: usize,
        Ret,

        pub fn ret() Instruction {
            return .Ret;
        }

        pub fn allocateStack(size: usize) Instruction {
            return .{ .AllocateStack = size };
        }

        pub fn unary(operator: UnaryOperator, operand: Operand) Instruction {
            return .{ .Unary = .{ .operator = operator, .operand = operand } };
        }

        pub fn mov(src: Operand, dst: Operand) Instruction {
            return .{ .Mov = .{ .src = src, .dst = dst } };
        }
    };
    pub const UnaryOperator = enum {
        neg,
        not,
    };
    pub const Operand = union(enum) {
        Imm: i64,
        Register: Register,
        Pseudo: []const u8,
        Stack: i64,

        pub fn stack(offset: i64) Operand {
            return .{ .Stack = offset };
        }

        pub fn pseudo(name: []const u8) Operand {
            return .{ .Pseudo = name };
        }

        pub fn imm(value: i64) Operand {
            return .{ .Imm = value };
        }
        pub fn register(reg: Register) Operand {
            return .{ .Register = reg };
        }
    };
    pub const Register = enum {
        // eax(lower 32 bit of rax),
        // ax(lower 16 bit of rax),
        // al(lower 8 bit of ax),
        // ah(upper 8 bit of ax)
        rax,
        rdx,
        // r10d (lower 32 bit of r10),
        // r10w (lower 16 bit of r10),
        // r10b (lower 8 bit of r10),
        r10,
    };
};

const Printer = struct {
    writer: AnyWriter,

    pub fn print(writer: AnyWriter, pg: Asm.Program, title: []const u8) void {
        const s = Printer{ .writer = writer };
        s.writeFmt("-- Codegen: {s} --\n", .{title});
        s.printFnDecl(pg.fn_defn, 0);
        s.write("\n");
    }
    fn printFnDecl(s: @This(), fn_defn: Asm.FnDefn, depth: usize) void {
        s.printSpace(depth);
        s.writeFmt("{s}:\n", .{fn_defn.name});
        for (fn_defn.instructions.items) |inst| {
            s.printInst(inst, depth + 1);
        }
    }
    fn printInst(s: @This(), inst: Asm.Instruction, depth: usize) void {
        s.printSpace(depth);
        switch (inst) {
            .Mov => |mov_inst| {
                s.write("Mov ");
                s.printOperand(mov_inst.src);
                s.write(", ");
                s.printOperand(mov_inst.dst);
            },
            .Ret => {
                s.write("Ret");
            },
            .Unary => |unary| {
                s.write("Unary(");
                s.write("operator: ");
                switch (unary.operator) {
                    .neg => s.write("neg"),
                    .not => s.write("not"),
                }
                s.write(", ");
                s.write("operand: ");
                s.printOperand(unary.operand);
                s.write(")");
            },
            .AllocateStack => unreachable,
        }
        s.write("\n");
    }

    fn printOperand(s: @This(), op: Asm.Operand) void {
        switch (op) {
            .Imm => |imm| s.writeFmt("Imm({d})", .{imm}),
            .Pseudo => |pseudo| s.writeFmt("Pseudo({s})", .{pseudo}),
            .Stack => |stack| s.writeFmt("Stack({d})", .{stack}),
            .Register => |r| s.printRegister(r),
        }
    }

    fn printRegister(s: @This(), reg: Asm.Register) void {
        const reg_str = switch (reg) {
            .rax => "rax",
            .rdx => "rdx",
            .r10 => "r10",
        };
        s.writeFmt("register({s})", .{reg_str});
    }

    fn printSpace(s: @This(), depth: usize) void {
        for (0..depth) |_| s.write("  ");
    }

    fn write(s: *const @This(), bytes: []const u8) void {
        _ = s.writer.write(bytes) catch unreachable;
    }
    fn writeFmt(s: *const @This(), comptime fmt: []const u8, args: anytype) void {
        _ = s.writer.print(fmt, args) catch unreachable;
    }
};

const std = @import("std");
const Tac = @import("TackyIR.zig").Tac;
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;
const ArrayList = std.ArrayList;
const HashMap = std.StringHashMap;
