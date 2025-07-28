const Options = struct {
    arena: Allocator,
    scratch_arena: Allocator,
    pg: Tac.Program,
    print_codegen: bool = false,
};

pub fn emit(opt: Options) Asm.Program {
    const stdErrorWriter = std.io.getStdOut().writer().any();

    var pg = Stage1.init(opt.arena).emitPg(opt.pg);
    if (opt.print_codegen) Printer.print(stdErrorWriter, pg, "Stage 1");

    Stage2.init(opt.arena).processPg(&pg);
    if (opt.print_codegen) Printer.print(stdErrorWriter, pg, "Stage 2");

    const final_pg = Stage3.init(opt.arena).fixPg(pg);
    if (opt.print_codegen) Printer.print(stdErrorWriter, final_pg, "Stage 3");

    return final_pg;
}

/// Convert Tacky IR to Asm IR.
const Stage1 = struct {
    arena: Allocator,

    const Self = @This();

    fn init(arena: Allocator) Self {
        return .{ .arena = arena };
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
        return .{
            .name = fn_defn.name,
            .instructions = instructions,
            .stack_size = 0,
        };
    }

    fn emitInst(s: Self, inst: Tac.Instruction, instructions: *Asm.Instructions) void {
        _ = s;
        switch (inst) {
            .Return => |ret| {
                const src = valToOperand(ret);
                const dst = Asm.Operand.register(.ax, .dword);

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
            .Binary => |binary| {
                switch (binary.operator) {
                    .Add, .Subtract, .Multiply => {
                        const left = valToOperand(binary.left);
                        const dst = valToOperand(binary.dst);
                        instructions.append(.mov(left, dst)) catch unreachable;

                        const op = switch (binary.operator) {
                            .Add => Asm.BinaryOperator.Add,
                            .Subtract => Asm.BinaryOperator.Subtract,
                            .Multiply => Asm.BinaryOperator.Multiply,
                            else => @panic("** Compiler Bug ** Unreachable path: expected add, sub or mul binary operator"),
                        };
                        const right = valToOperand(binary.right);
                        instructions.append(.binary(op, right, dst)) catch unreachable;
                    },
                    .Divide, .Mod => {
                        const left = valToOperand(binary.left);
                        const first_move_dst: Asm.Operand = .register(.ax, .dword);
                        instructions.append(.mov(left, first_move_dst)) catch unreachable;

                        instructions.append(.cdq()) catch unreachable;

                        const right = valToOperand(binary.right);
                        instructions.append(.idiv(right)) catch unreachable;

                        const final_mov_src: Asm.Operand = switch (binary.operator) {
                            .Divide => .register(.ax, .dword),
                            .Mod => .register(.dx, .dword),
                            else => @panic("** Compiler Bug ** Unreachable path: expected divide or mod binary operator"),
                        };
                        const final_mov_dst: Asm.Operand = valToOperand(binary.dst);
                        instructions.append(.mov(final_mov_src, final_mov_dst)) catch unreachable;
                    },
                }
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

/// Replace pseudo variables with stack offsets.
const Stage2 = struct {
    arena: Allocator,
    variable_map: *HashMap(i64),

    // Note:
    // this is set to 4 because any variable we add is just 32-bit int as of now
    const STACK_STEP = 4;

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
        fn_defn.stack_size = s.variable_map.count() * STACK_STEP;
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
            .Binary => |*binary| {
                binary.operand = s.mapOperandToStack(binary.operand);
                binary.dst = s.mapOperandToStack(binary.dst);
            },
            .IDiv => |*operand| {
                operand.* = s.mapOperandToStack(operand.*);
            },
            .Cdq, .AllocateStack, .Ret => {}, // noop
        }
    }
    fn mapOperandToStack(s: @This(), operand: Asm.Operand) Asm.Operand {
        return switch (operand) {
            .Pseudo => |ident| {
                if (s.variable_map.getEntry(ident)) |saved_offset| {
                    return .stack(saved_offset.value_ptr.*);
                }
                const new_offset: i64 = @as(i64, @intCast(s.variable_map.count() + 1)) * -STACK_STEP;
                s.variable_map.put(ident, new_offset) catch unreachable;
                return .stack(new_offset);
            },
            else => return operand,
        };
    }
};

/// Fix assembly instructions
const Stage3 = struct {
    arena: Allocator,

    pub fn init(arena: Allocator) @This() {
        return .{ .arena = arena };
    }

    pub fn fixPg(s: @This(), pg: Asm.Program) Asm.Program {
        const fn_defn = s.fixFn(pg.fn_defn);
        return .{ .fn_defn = fn_defn };
    }

    fn fixFn(s: @This(), fn_defn: Asm.FnDefn) Asm.FnDefn {
        var instructions = ArrayList(Asm.Instruction).init(s.arena);

        // Align stack size to 16 bytes - which is required as per System V ABI
        const ALIGNMENT = 16;
        var aligned_stack_size: usize = fn_defn.stack_size;
        if (aligned_stack_size % ALIGNMENT != 0) aligned_stack_size += (ALIGNMENT - (aligned_stack_size % ALIGNMENT));

        instructions.append(.allocateStack(aligned_stack_size)) catch unreachable;
        for (fn_defn.instructions.items) |inst| {
            switch (inst) {
                .Mov => |mov| {
                    if (mov.src == .Stack and mov.dst == .Stack) {
                        instructions.append(.mov(mov.src, .register(.r10, .dword))) catch unreachable;
                        instructions.append(.mov(.register(.r10, .dword), mov.dst)) catch unreachable;
                        continue;
                    }
                    instructions.append(inst) catch unreachable;
                },
                .Binary => |binary| {
                    switch (binary.operator) {
                        .Add, .Subtract => {
                            const reg: Asm.Operand = .register(.r10, .dword);
                            instructions.append(.mov(binary.operand, reg)) catch unreachable;
                            instructions.append(.binary(binary.operator, reg, binary.dst)) catch unreachable;
                        },
                        .Multiply => {
                            const reg: Asm.Operand = .register(.r11, .dword);
                            instructions.append(.mov(binary.dst, reg)) catch unreachable;
                            instructions.append(.binary(binary.operator, binary.operand, reg)) catch unreachable;
                            instructions.append(.mov(reg, binary.dst)) catch unreachable;
                        },
                    }
                },
                .IDiv => |operand| {
                    switch (operand) {
                        .Imm => {
                            const reg: Asm.Operand = .register(.r10, .dword);
                            instructions.append(.mov(operand, reg)) catch unreachable;
                            instructions.append(.idiv(reg)) catch unreachable;
                        },
                        else => instructions.append(inst) catch unreachable,
                    }
                },
                .Cdq, .Unary, .AllocateStack, .Ret => instructions.append(inst) catch unreachable,
            }
        }
        return .{ .name = fn_defn.name, .instructions = instructions, .stack_size = aligned_stack_size };
    }
};

pub const Asm = struct {
    pub const Program = struct {
        fn_defn: FnDefn,
    };
    pub const FnDefn = struct {
        name: []const u8,
        stack_size: usize,
        instructions: std.ArrayList(Instruction),
    };
    const Instructions = ArrayList(Instruction);
    pub const Instruction = union(enum) {
        Mov: struct { src: Operand, dst: Operand },
        Unary: struct { operator: UnaryOperator, operand: Operand },
        Binary: struct { operator: BinaryOperator, operand: Operand, dst: Operand },
        IDiv: Operand,
        Cdq,
        AllocateStack: usize,
        Ret,

        pub fn cdq() Instruction {
            return .Cdq;
        }

        pub fn idiv(operand: Operand) Instruction {
            return .{ .IDiv = operand };
        }

        pub fn binary(operator: BinaryOperator, operand: Operand, dst: Operand) Instruction {
            return .{
                .Binary = .{ .operator = operator, .operand = operand, .dst = dst },
            };
        }

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
    pub const BinaryOperator = enum {
        Add,
        Subtract,
        Multiply,
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

        pub fn register(reg: Register.Type, size: Register.Size) Operand {
            return .{ .Register = .register(reg, size) };
        }
    };
    pub const Register = struct {
        type: Type,
        size: Size,

        pub fn register(reg: Register.Type, size: Register.Size) @This() {
            return .{ .type = reg, .size = size };
        }

        const Type = enum {
            ax,
            bp,
            sp,
            dx,
            r10,
            r11,
        };
        const Size = enum(u8) {
            byte = 1,
            word = 2,
            dword = 4,
            qword = 8,
        };

        pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = options;
            _ = fmt;
            const value = switch (self.type) {
                .ax => switch (self.size) {
                    .byte => "%al",
                    .word => "%ax",
                    .dword => "%eax",
                    .qword => "%rax",
                },
                .dx => switch (self.size) {
                    .byte => "%dl",
                    .word => "%dx",
                    .dword => "%edx",
                    .qword => "%rdx",
                },
                .sp => switch (self.size) {
                    .byte => "%spl",
                    .word => "%sp",
                    .dword => "%esp",
                    .qword => "%rsp",
                },
                .bp => switch (self.size) {
                    .byte => "%bpl",
                    .word => "%bp",
                    .dword => "%ebp",
                    .qword => "%rbp",
                },
                .r10 => switch (self.size) {
                    .byte => "%r10b",
                    .word => "%r10w",
                    .dword => "%r10d",
                    .qword => "%r10",
                },
                .r11 => switch (self.size) {
                    .byte => "%r11b",
                    .word => "%r11w",
                    .dword => "%r11d",
                    .qword => "%r11",
                },
            };
            writer.print("{s}", .{value}) catch unreachable;
        }
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
            .Binary => |binary| {
                s.write("Binary(");
                s.write("operator: ");
                switch (binary.operator) {
                    .Add => s.write("Add"),
                    .Subtract => s.write("Subtract"),
                    .Multiply => s.write("Multiply"),
                }
                s.write(", ");
                s.write("operand: ");
                s.printOperand(binary.operand);
                s.write(", ");
                s.write("dst: ");
                s.printOperand(binary.dst);
                s.write(")");
            },
            .IDiv => |operand| {
                s.write("IDiv(");
                s.printOperand(operand);
                s.write(")");
            },
            .Cdq => {
                s.write("Cdq");
            },
            .AllocateStack => |size| {
                s.writeFmt("AllocateStack({d})", .{size});
            },
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
        s.writeFmt("Register({any})", .{reg});
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
