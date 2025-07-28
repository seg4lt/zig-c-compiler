arena: Allocator,
scratch_arena: Allocator,
writer: AnyWriter,

const Self = @This();

const Options = struct {
    arena: Allocator,
    scratch_arena: Allocator,
    src_path_no_ext: []const u8,
    pg: Asm.Program,
    print: bool = false,
};

pub fn emit(opt: Options) !void {
    var buffer = std.ArrayList(u8).init(opt.arena);
    const s = Self{
        .arena = opt.arena,
        .scratch_arena = opt.scratch_arena,
        .writer = buffer.writer().any(),
    };
    s.emitProgram(opt.pg);

    if (opt.print) {
        const stdout = std.io.getStdOut();
        _ = stdout.write("-- ASM --\n") catch unreachable;
        _ = stdout.write(buffer.items) catch unreachable;
    }

    const asm_file = std.fmt.allocPrint(opt.scratch_arena, "{s}.s", .{opt.src_path_no_ext}) catch unreachable;
    try std.fs.cwd().writeFile(.{ .sub_path = asm_file, .data = buffer.items });
}

fn emitProgram(s: Self, pg: Asm.Program) void {
    defer if (builtin.os.tag == .linux and builtin.os.tag != .macos) {
        const progbits = "\t.section .note.GNU-stack,\"\",@progbits\n";
        s.write(progbits);
    };
    s.emitFnDecl(pg.fn_defn);
}

fn emitFnDecl(s: Self, fn_decl: Asm.FnDefn) void {
    s.writeFmt("\t.globl\t{s}\n", .{fn_decl.name});
    s.writeFmt("{s}:\n", .{fn_decl.name});

    // prologue
    s.writeFmt("\tpushq\t{any}\n", .{Asm.Register.register(.bp, .qword)});
    s.writeFmt("\tmovq\t{any}, {any}\n", .{ Asm.Register.register(.sp, .qword), Asm.Register.register(.bp, .qword) });

    for (fn_decl.instructions.items) |item| s.emitInstructions(item);
}

fn emitInstructions(s: Self, instruction: Asm.Instruction) void {
    switch (instruction) {
        .Mov => |mov| {
            s.writeFmt("\tmovl\t{s}, {s}\n", .{
                s.fmtOperand(mov.src),
                s.fmtOperand(mov.dst),
            });
        },
        .Unary => |unary| {
            s.writeFmt("\t{s}\t{s}\n", .{
                fmtUnaryOperator(unary.operator),
                s.fmtOperand(unary.operand),
            });
        },
        .Binary => |binary| {
            const op = switch (binary.operator) {
                .Add => "addl",
                .Subtract => "subl",
                .Multiply => "imull",
            };
            const dst = s.fmtOperand(binary.dst);
            const src = s.fmtOperand(binary.operand);
            s.writeFmt("\t{s}\t{s}, {s}\n", .{ op, src, dst });
        },
        .IDiv => |operand| {
            const op = "idivl";
            const divider = s.fmtOperand(operand);
            s.writeFmt("\t{s}\t{s}\n", .{ op, divider });
        },
        .Cdq => {
            s.write("\tcdq\n");
        },
        .AllocateStack => |stack_size| {
            if (stack_size != 0) {
                s.writeFmt("\tsubq\t${d}, {any}\n", .{
                    stack_size,
                    Asm.Register.register(.sp, .qword),
                });
                s.write("\t# ^^^\tPrologue\n");
            } else {
                s.write("\t# ^^^\tPrologue (no stack allocation needed)\n");
            }
        },
        .Ret => {
            // epilogue
            s.write("\t# vv\tEpilogue\n");
            s.writeFmt(
                "\tmovq\t{any}, {any}\n",
                .{
                    Asm.Register.register(.bp, .qword),
                    Asm.Register.register(.sp, .qword),
                },
            );
            s.writeFmt("\tpopq\t{any}\n", .{Asm.Register.register(.bp, .qword)});
            s.write("\tret\n");
        },
    }
}

fn fmtUnaryOperator(operator: Asm.UnaryOperator) []const u8 {
    return switch (operator) {
        .neg => "negl",
        .not => "notl",
    };
}

fn fmtOperand(s: Self, op: Asm.Operand) []const u8 {
    return switch (op) {
        .Imm => |imm| std.fmt.allocPrint(s.arena, "${d}", .{imm}) catch unreachable,
        .Register => |r| std.fmt.allocPrint(s.arena, "{any}", .{r}) catch unreachable,
        .Stack => |stack_size| std.fmt.allocPrint(s.arena, "{d}({any})", .{
            stack_size,
            Asm.Register.register(.bp, .qword),
        }) catch unreachable,
        .Pseudo => unreachable, // all variable should be converted to relative stack value
    };
}

fn write(s: Self, bytes: []const u8) void {
    _ = s.writer.write(bytes) catch unreachable;
}

fn writeFmt(s: Self, comptime fmt: []const u8, args: anytype) void {
    _ = s.writer.print(fmt, args) catch unreachable;
}

const std = @import("std");
const builtin = @import("builtin");
const Asm = @import("Codegen.zig").Asm;
const AnyWriter = std.io.AnyWriter;
const Allocator = std.mem.Allocator;
