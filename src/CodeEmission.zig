arena: Allocator,
scratch_arena: Allocator,
writer: AnyWriter,
backend_symbols: *const BackendSymbolTable,

const Self = @This();

const Options = struct {
    arena: Allocator,
    scratch_arena: Allocator,
    src_path_no_ext: []const u8,
    pg: Asm.Program,
    backend_symbols: *const BackendSymbolTable,
    print: bool = false,
};

pub fn emit(opt: Options) !void {
    var buffer = ArrayList(u8).init(opt.arena);
    const s = Self{
        .arena = opt.arena,
        .scratch_arena = opt.scratch_arena,
        .backend_symbols = opt.backend_symbols,
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

                const init_value: i64 = switch (static_var.initializer) {
                    .Int => |i| i,
                    .Long => |l| l,
                };

                if (init_value != 0) s.write("\t.data\n") else s.write("\t.bss\n");
                s.writeFmt("\t.balign {d}\n", .{static_var.alignment});
                s.writeFmt("{s}:\n", .{symbol_name});
                if (init_value != 0) {
                    const value: i64 = switch (static_var.initializer) {
                        .Int => |i| i,
                        .Long => |l| l,
                    };
                    s.writeFmt("\t.long {d}\n", .{value});
                } else {
                    s.writeFmt("\t.zero {d}\n", .{static_var.alignment});
                }
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
                    if (s.backend_symbols.get(call)) |symbol| {
                        if (symbol == .FnEntry and !symbol.FnEntry.is_defined) {
                            add_plt = true;
                        }
                    }
                    s.writeFmt("\t{f}\t{s}", .{ instruction, call });
                    s.writeFmt("{s}\n", .{if (!add_plt) "" else "@PLT"});
                },
                .macos => {
                    s.writeFmt("\t{f}\t_{s}\n", .{ instruction, call });
                },
                else => |os| std.debug.panic("Unsupported OS for code emission: {s}", .{@tagName(os)}),
            }
        },
        .Push => |push| {
            s.writeFmt("\t{f}\t{s}\n", .{ instruction, s.fmtOperand(push) });
        },
        .Mov => |mov| {
            s.writeFmt("\t{f}\t{s}, {s}\n", .{
                instruction,
                s.fmtOperand(mov.src),
                s.fmtOperand(mov.dst),
            });
        },
        .Movsx => |movsx| {
            s.writeFmt("\t{f}\t{s}, {s}\n", .{
                instruction,
                s.fmtOperand(movsx.src),
                s.fmtOperand(movsx.dst),
            });
        },
        .Unary => |unary| {
            s.writeFmt("\t{f}\t{s}\n", .{
                instruction,
                s.fmtOperand(unary.operand),
            });
        },
        .Binary => |binary| {
            const dst = s.fmtOperand(binary.dst);
            const src = s.fmtOperand(binary.operand);
            s.writeFmt("\t{f}\t{s}, {s}\n", .{ instruction, src, dst });
        },
        .IDiv => |it| {
            const divider = s.fmtOperand(it.operand);
            s.writeFmt("\t{f}\t{s}\n", .{ instruction, divider });
        },
        .Cmp => |cmp| {
            const left = s.fmtOperand(cmp.op1);
            const right = s.fmtOperand(cmp.op2);
            s.writeFmt("\t{f}\t{s}, {s}\n", .{ instruction, left, right });
        },
        .Jmp => |jmp| {
            s.writeFmt("\t{f}\t{s}\n", .{ instruction, jmp });
        },
        .JmpCC => |jmp_cc| {
            s.writeFmt("\t{f}\t{s}\n", .{ instruction, jmp_cc.label });
        },
        .SetCC => |set_cc| {
            s.writeFmt("\t{f}\t{s}\n", .{ instruction, s.fmtOperand(set_cc.dst) });
        },
        .Label => |label| {
            s.writeFmt("{s}:\n", .{label});
        },
        .Cdq => {
            s.writeFmt("\t{f}\n", .{instruction});
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
            s.writeFmt("\t{f}\n", .{instruction});
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
const BackendSymbolTable = @import("BackendSymbolTable.zig");
const Printer = @import("util.zig").Printer;
