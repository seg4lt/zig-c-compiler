arena: Allocator,
scratch_arena: Allocator,
writer: AnyWriter,
symbol_table: *SymbolTable,

const Self = @This();

const Options = struct {
    arena: Allocator,
    scratch_arena: Allocator,
    src_path_no_ext: []const u8,
    pg: Asm.Program,
    symbol_table: *SymbolTable,
    print: bool = false,
};

pub fn emit(opt: Options) !void {
    var buffer = ArrayList(u8).init(opt.arena);
    const s = Self{
        .arena = opt.arena,
        .scratch_arena = opt.scratch_arena,
        .symbol_table = opt.symbol_table,
        .writer = buffer.writer().any(),
    };
    s.emitProgram(opt.pg);

    if (opt.print) {
        var printer = Printer.init(opt.scratch_arena);
        const writer = printer.writer();
        _ = writer.write("-- ASM --\n") catch unreachable;
        _ = writer.write(buffer.items) catch unreachable;
        printer.printToStdErr(.{}) catch unreachable;
    }

    const asm_file = std.fmt.allocPrint(opt.scratch_arena, "{s}.s", .{opt.src_path_no_ext}) catch unreachable;
    try std.fs.cwd().writeFile(.{ .sub_path = asm_file, .data = buffer.items });
}

fn emitProgram(s: Self, pg: Asm.Program) void {
    defer if (builtin.os.tag == .linux and builtin.os.tag != .macos) {
        const progbits = "\t.section .note.GNU-stack,\"\",@progbits\n";
        s.write(progbits);
    };
    for (pg.decls.items) |decl| {
        switch (decl) {
            .Fn => |fn_defn| {
                s.emitFnDecl(fn_defn);
            },
            .StaticVar => |static_var| {
                const symbol_name = switch (builtin.os.tag) {
                    .macos => std.fmt.allocPrint(s.arena, "_{s}", .{static_var.ident}) catch unreachable,
                    else => static_var.ident,
                };
                s.write("\n");
                if (static_var.global) s.writeFmt("\t.global {s}\n", .{symbol_name});
                if (static_var.initializer != 0) s.write("\t.data\n") else s.write("\t.bss\n");
                s.write("\t.balign 4\n");
                s.writeFmt("{s}:\n", .{symbol_name});
                if (static_var.initializer != 0) s.writeFmt("\t.long {d}\n", .{static_var.initializer}) else s.write("\t.zero 4\n");
            },
        }
    }
}

fn emitFnDecl(s: Self, fn_decl: Asm.FnDecl) void {
    const symbol_name = switch (builtin.os.tag) {
        .linux => std.fmt.allocPrint(s.arena, "{s}", .{fn_decl.ident}) catch unreachable,
        .macos => std.fmt.allocPrint(s.arena, "_{s}", .{fn_decl.ident}) catch unreachable,
        else => |os| std.debug.panic("Unsupported OS for code emission: {s}", .{@tagName(os)}),
    };

    switch (fn_decl.global) {
        // fn needs globl
        true => s.writeFmt("\t.globl\t{s}\n", .{symbol_name}),
        false => s.writeFmt("\t.local\t{s}\n", .{symbol_name}),
    }

    s.write("\t.text\n");
    s.writeFmt("{s}:\n", .{symbol_name});

    // prologue
    s.writeFmt("\tpushq\t{f}\n", .{Asm.Register.register(.bp, .qword)});
    s.writeFmt("\tmovq\t{f}, {f}\n", .{ Asm.Register.register(.sp, .qword), Asm.Register.register(.bp, .qword) });

    for (fn_decl.instructions.items) |item| s.emitInstructions(item);
}

fn emitInstructions(s: Self, instruction: Asm.Instruction) void {
    switch (instruction) {
        .Call => |call| {
            switch (builtin.os.tag) {
                .linux => {
                    var add_plt = false;
                    if (s.symbol_table.get(call)) |symbol| {
                        if (symbol == .Fn and !symbol.Fn.defined) {
                            add_plt = true;
                        }
                    }
                    s.writeFmt("\tcall\t{s}", .{call});
                    s.writeFmt("{s}\n", .{if (!add_plt) "" else "@PLT"});
                },
                .macos => {
                    s.writeFmt("\tcall\t_{s}\n", .{call});
                },
                else => |os| std.debug.panic("Unsupported OS for code emission: {s}", .{@tagName(os)}),
            }
        },
        .DeallocateStack => |size| {
            s.writeFmt("\taddq\t${d}, {f}\n", .{ size, Asm.Register.register(.sp, .qword) });
        },
        .Push => |push| {
            s.writeFmt("\tpushq\t{s}\n", .{s.fmtOperand(push)});
        },
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
                .LeftShift => "sall", // doing arthmetic shift - shll for logical shift
                .RightShift => "sarl", // doing arthmetic shift - shrl for logical
                .BitAnd => "andl",
                .BitOr => "orl",
                .BitXor => "xorl",
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
        .Cmp => |cmp| {
            const op = "cmpl";
            const left = s.fmtOperand(cmp.op1);
            const right = s.fmtOperand(cmp.op2);
            s.writeFmt("\t{s}\t{s}, {s}\n", .{ op, left, right });
        },
        .Jmp => |jmp| {
            const op = "jmp";
            s.writeFmt("\t{s}\t{s}\n", .{ op, jmp });
        },
        .JmpCC => |jmp_cc| {
            const op = switch (jmp_cc.condition_code) {
                .EqualEqual => "je",
                .NotEqual => "jne",
                .GreaterThan => "jg",
                .GreaterThanEqual => "jge",
                .LessThan => "jl",
                .LessThanEqual => "jle",
            };
            s.writeFmt("\t{s}\t{s}\n", .{ op, jmp_cc.label });
        },
        .SetCC => |set_cc| {
            const op = switch (set_cc.condition_code) {
                .EqualEqual => "sete",
                .NotEqual => "setne",
                .GreaterThan => "setg",
                .GreaterThanEqual => "setge",
                .LessThan => "setl",
                .LessThanEqual => "setle",
            };
            s.writeFmt("\t{s}\t{s}\n", .{ op, s.fmtOperand(set_cc.dst) });
        },
        .Label => |label| {
            s.writeFmt("{s}:\n", .{label});
        },
        .Cdq => {
            s.write("\tcdq\n");
        },
        .AllocateStack => |stack_size| {
            if (stack_size != 0) {
                s.writeFmt("\tsubq\t${d}, {f}\n", .{
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
                "\tmovq\t{f}, {f}\n",
                .{
                    Asm.Register.register(.bp, .qword),
                    Asm.Register.register(.sp, .qword),
                },
            );
            s.writeFmt("\tpopq\t{f}\n", .{Asm.Register.register(.bp, .qword)});
            s.write("\tret\n");
        },
    }
}

fn fmtUnaryOperator(operator: Asm.UnaryOperator) []const u8 {
    return switch (operator) {
        .Neg => "negl",
        .BitNot => "notl",
    };
}

fn fmtOperand(s: Self, op: Asm.Operand) []const u8 {
    const allocPrint = std.fmt.allocPrint;
    return switch (op) {
        .Imm => |imm| allocPrint(s.arena, "${d}", .{imm}),
        .Register => |r| allocPrint(s.arena, "{f}", .{r}),
        .Stack => |stack_size| allocPrint(s.arena, "{d}({f})", .{
            stack_size,
            Asm.Register.register(.bp, .qword),
        }),
        .Data => |ident| switch (builtin.os.tag) {
            .macos => allocPrint(s.arena, "_{s}({f})", .{ ident, Asm.Register.register(.rip, .qword) }),
            else => allocPrint(s.arena, "{s}({f})", .{ ident, Asm.Register.register(.rip, .qword) }),
        },
        .Pseudo => unreachable, // all variable should be converted to relative stack value
    } catch unreachable;
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
const ArrayList = @import("from_scratch.zig").ArrayList;
const AnyWriter = std.io.AnyWriter;
const Allocator = std.mem.Allocator;
const SymbolTable = @import("SymbolTable.zig");
const Printer = @import("util.zig").Printer;
