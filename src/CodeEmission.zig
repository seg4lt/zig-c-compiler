arena: Allocator,
scratch_arena: Allocator,
writer: AnyWriter,

const Self = @This();

const Options = struct {
    arena: Allocator,
    scratch_arena: Allocator,
    src_path_no_ext: []const u8,
    pg: Asm.Program,
};

pub fn emit(opt: Options) !void {
    var buffer = std.ArrayList(u8).init(opt.arena);
    const s = Self{
        .arena = opt.arena,
        .scratch_arena = opt.scratch_arena,
        .writer = buffer.writer().any(),
    };
    s.emitProgram(opt.pg);

    const asm_file = std.fmt.allocPrint(opt.scratch_arena, "{s}.s", .{opt.src_path_no_ext}) catch unreachable;
    try std.fs.cwd().writeFile(.{ .sub_path = asm_file, .data = buffer.items });
}

fn emitProgram(s: *const Self, pg: Asm.Program) void {
    defer if (builtin.os.tag == .linux and builtin.os.tag != .macos) {
        const progbits = "\t.section .note.GNU-stack,\"\",@progbits\n";
        s.write(progbits);
    };
    s.emitFnDecl(pg.fn_defn);
}

fn emitFnDecl(s: *const Self, fn_decl: Asm.FnDefn) void {
    s.write_fmt("\t.globl\t{s}\n", .{fn_decl.name});
    s.write_fmt("{s}:\n", .{fn_decl.name});

    for (fn_decl.instructions.items) |item| {
        s.emitInstructions(item);
    }
}

fn emitInstructions(s: *const Self, instruction: Asm.Instruction) void {
    switch (instruction) {
        .Mov => |mov| {
            s.write_fmt("\tmovl\t{s}, {s}\n", .{ s.fmtOperand(mov.src), s.fmtOperand(mov.dst) });
        },
        .Ret => {
            s.write("\tret\n");
        },
    }
}

fn fmtOperand(s: *const Self, op: Asm.Operand) []const u8 {
    return switch (op) {
        .Imm => |imm| std.fmt.allocPrint(s.arena, "${d}", .{imm}) catch unreachable,
        .Register => |r| s.fmtRegister(r),
    };
}

fn fmtRegister(s: *const Self, reg: Asm.Register) []const u8 {
    _ = s;
    return switch (reg) {
        .rax => "%eax", // "%rax",
        .rdx => "%rdx",
    };
}

fn write(s: *const Self, comptime bytes: []const u8) void {
    _ = s.writer.write(bytes) catch unreachable;
}
fn write_fmt(s: *const Self, comptime fmt: []const u8, args: anytype) void {
    _ = s.writer.print(fmt, args) catch unreachable;
}
const std = @import("std");
const builtin = @import("builtin");
const Asm = @import("AsmGen.zig").Asm;
const AnyWriter = std.io.AnyWriter;
const Allocator = std.mem.Allocator;
