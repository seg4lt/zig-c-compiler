const Options = struct {
    arena: Allocator,
    scratch_arena: Allocator,
    pg: Tac.Program,
    symbol_table: *const SymbolTable,
    print_codegen: bool = false,
};

const PUSH_SIZE = 8; // 64-bit architecture pushes 8-byte
const STACK_ALIGNMENT = 16;
const COUNT_REGISTER_ARGS = 6;
const ARGS_REGISTER = [_]Asm.Register.Type{ .di, .si, .dx, .cx, .r8, .r9 };

pub fn emit(opt: Options) Asm.Program {
    var pg = Stage1.init(opt.arena, opt.symbol_table).emitPg(opt.pg);
    if (opt.print_codegen) {
        var printer = AllocatingPrinter.init(opt.scratch_arena);
        Printer.print(printer.writer(), pg, "Stage 1");
        printer.printToStdErr(.{}) catch unreachable;
    }

    Stage2.init(opt.arena, opt.symbol_table).processPg(&pg);
    if (opt.print_codegen) {
        var printer = AllocatingPrinter.init(opt.scratch_arena);
        Printer.print(printer.writer(), pg, "Stage 2");
        printer.printToStdErr(.{}) catch unreachable;
    }

    const final_pg = Stage3.init(opt.arena).fixPg(pg);
    if (opt.print_codegen) {
        var printer = AllocatingPrinter.init(opt.scratch_arena);
        Printer.print(printer.writer(), final_pg, "Stage 3");
        printer.printToStdErr(.{}) catch unreachable;
    }

    return final_pg;
}

/// Convert Tacky IR to Asm IR.
const Stage1 = struct {
    arena: Allocator,
    symbol_table: *const SymbolTable,

    const Self = @This();

    fn init(arena: Allocator, symbol_table: *const SymbolTable) Self {
        return .{ .arena = arena, .symbol_table = symbol_table };
    }

    fn emitPg(s: Self, pg: Tac.Program) Asm.Program {
        var decls = ArrayList(Asm.TopLevelDecl).init(s.arena);

        for (pg.decls.items) |decl| {
            switch (decl) {
                .Fn => |fn_decl| {
                    const fn_asm = s.emitFnDefn(fn_decl);
                    decls.append(fn_asm);
                },
                .StaticVar => |static_var| {
                    decls.append(.staticVar(s.arena, static_var.attribute.ident, static_var.attribute.global, static_var.initializer));
                },
            }
        }
        return .{ .decls = decls };
    }

    fn emitFnDefn(s: Self, fn_defn: Tac.TopLevelDecl.FnDecl) Asm.TopLevelDecl {
        var instructions = Asm.Instructions.init(s.arena);

        const entry = s.symbol_table.get(fn_defn.attribute.ident) orelse {
            std.debug.panic("** Compiler Bug ** - function name not found in symbol table: {s}", .{fn_defn.attribute.ident});
        };
        if (entry != .Fn) {
            std.debug.panic("** Compiler Bug ** - expected function symbol, found: {s}", .{@tagName(entry)});
        }
        const fn_entry = entry.Fn;
        const fn_params = fn_entry.params.items;

        const register_args = fn_params[0..@min(fn_params.len, COUNT_REGISTER_ARGS)];
        const stack_args = if (fn_params.len > COUNT_REGISTER_ARGS) fn_params[COUNT_REGISTER_ARGS..] else &.{};

        for (register_args, 0..) |reg_param, i| {
            const reg_to_use = ARGS_REGISTER[i];
            const asm_arg: Asm.Operand = .pseudo(reg_param.ident);
            instructions.append(.mov(.register(reg_to_use, .dword), asm_arg));
        }

        for (stack_args, 0..) |stack_param, i| {
            const asm_arg: Asm.Operand = .pseudo(stack_param.ident);
            const offset: i64 = STACK_ALIGNMENT + (@as(i64, @intCast(i)) * PUSH_SIZE);
            const stack_offset: Asm.Operand = .stack(offset);
            instructions.append(.mov(stack_offset, asm_arg));
        }

        for (fn_defn.instructions.items) |inst| s.emitInst(inst, &instructions);
        return .fnDecl(s.arena, fn_defn.attribute.ident, 0, fn_defn.attribute.global, instructions);
    }

    fn emitInst(s: Self, inst: Tac.Instruction, instructions: *Asm.Instructions) void {
        switch (inst) {
            .Return => |ret| {
                const src = valToOperand(s.arena, ret);
                const dst = Asm.Operand.register(.ax, .dword);

                instructions.append(.mov(src, dst));
                instructions.append(.ret());
            },
            .Unary => |unary| {
                if (unary.operator == .Not) {
                    const zero: Asm.Operand = .imm(0);
                    const src = valToOperand(s.arena, unary.src);
                    const dst = valToOperand(s.arena, unary.dst);
                    instructions.append(.mov(zero, dst));
                    instructions.append(.cmp(zero, src));
                    instructions.append(.setCC(.EqualEqual, dst));
                } else {
                    const src = valToOperand(s.arena, unary.src);
                    const dst = valToOperand(s.arena, unary.dst);
                    instructions.append(.mov(src, dst));
                    const operator: Asm.UnaryOperator = switch (unary.operator) {
                        .Negate => .Neg,
                        .BitNot => .BitNot,
                        .Not => std.debug.panic("** Compiler Bug ** Unreachable path: expected negate or bit_not unary operator", .{}),
                    };
                    instructions.append(.unary(operator, dst));
                }
            },
            .Binary => |binary| {
                switch (binary.operator) {
                    .EqualEqual, .GreaterThan, .GreaterThanEqual, .LessThan, .LessThanEqual, .NotEqual => {
                        const dst = valToOperand(s.arena, binary.dst);
                        instructions.append(.mov(.imm(0), dst));

                        const src2 = valToOperand(s.arena, binary.right);
                        switch (binary.left) {
                            .Const => {
                                const src1 = valToOperand(s.arena, binary.left);
                                const reg: Asm.Operand = .register(.r11, .dword);
                                instructions.append(.mov(src1, reg));
                                instructions.append(.cmp(src2, reg));
                            },
                            .Var => {
                                const src1 = valToOperand(s.arena, binary.left);
                                instructions.append(.cmp(src2, src1));
                            },
                        }
                        const condition_code: Asm.ConditionCode = switch (binary.operator) {
                            .EqualEqual => .EqualEqual,
                            .GreaterThan => .GreaterThan,
                            .GreaterThanEqual => .GreaterThanEqual,
                            .LessThan => .LessThan,
                            .LessThanEqual => .LessThanEqual,
                            .NotEqual => .NotEqual,
                            else => std.debug.panic("** Compiler Bug ** Unreachable path: expected comparison operator", .{}),
                        };
                        instructions.append(.setCC(condition_code, dst));
                    },
                    .LeftShift, .RightShift, .BitAnd, .BitOr, .BitXor, .Add, .Subtract, .Multiply => {
                        const left = valToOperand(s.arena, binary.left);
                        const dst = valToOperand(s.arena, binary.dst);
                        instructions.append(.mov(left, dst));

                        const op = switch (binary.operator) {
                            .Add => Asm.BinaryOperator.Add,
                            .Subtract => Asm.BinaryOperator.Subtract,
                            .Multiply => Asm.BinaryOperator.Multiply,
                            .LeftShift => Asm.BinaryOperator.LeftShift,
                            .RightShift => Asm.BinaryOperator.RightShift,
                            .BitAnd => Asm.BinaryOperator.BitAnd,
                            .BitOr => Asm.BinaryOperator.BitOr,
                            .BitXor => Asm.BinaryOperator.BitXor,
                            else => std.debug.panic("** Compiler Bug ** Unreachable path: expected add, sub or mul binary operator", .{}),
                        };
                        const right = valToOperand(s.arena, binary.right);
                        instructions.append(.binary(op, right, dst));
                    },
                    .Divide, .Mod => {
                        const left = valToOperand(s.arena, binary.left);
                        const first_move_dst: Asm.Operand = .register(.ax, .dword);
                        instructions.append(.mov(left, first_move_dst));

                        instructions.append(.cdq());

                        const right = valToOperand(s.arena, binary.right);
                        instructions.append(.idiv(right));

                        const final_mov_src: Asm.Operand = switch (binary.operator) {
                            .Divide => .register(.ax, .dword),
                            .Mod => .register(.dx, .dword),
                            else => std.debug.panic("** Compiler Bug ** Unreachable path: expected divide or mod binary operator", .{}),
                        };
                        const final_mov_dst: Asm.Operand = valToOperand(s.arena, binary.dst);
                        instructions.append(.mov(final_mov_src, final_mov_dst));
                    },
                }
            },
            .Copy => |copy| {
                const src = valToOperand(s.arena, copy.src);
                const dst = valToOperand(s.arena, copy.dst);
                instructions.append(.mov(src, dst));
            },
            .Jump => |jmp| {
                const label = s.arena.dupe(u8, jmp) catch unreachable;
                instructions.append(.jmp(label));
            },
            .JumpIfZero => |jmp| {
                const condition = valToOperand(s.arena, jmp.condition);
                const zero: Asm.Operand = .imm(0);
                const label = s.arena.dupe(u8, jmp.label) catch unreachable;
                instructions.append(.cmp(zero, condition));
                instructions.append(.jmpCC(.EqualEqual, label));
            },
            .JumpIfNotZero => |jmp| {
                const condition = valToOperand(s.arena, jmp.condition);
                const zero: Asm.Operand = .imm(0);
                const label = s.arena.dupe(u8, jmp.label) catch unreachable;
                instructions.append(.cmp(zero, condition));
                instructions.append(.jmpCC(.NotEqual, label));
            },
            .Label => |label| {
                const owned_label = s.arena.dupe(u8, label) catch unreachable;
                instructions.append(.label(owned_label));
            },
            .FnCall => |fn_call| {
                const fn_args = fn_call.args.items;
                const register_args = fn_args[0..@min(fn_args.len, COUNT_REGISTER_ARGS)];
                const stack_args = if (fn_args.len > COUNT_REGISTER_ARGS) fn_args[COUNT_REGISTER_ARGS..] else &.{};

                const stack_padding: usize = if (stack_args.len % 2 == 0) 0 else PUSH_SIZE;
                if (stack_padding != 0) {
                    instructions.append(.allocateStack(stack_padding));
                }

                for (register_args, 0..) |reg_arg, i| {
                    const reg_to_use = ARGS_REGISTER[i];
                    const asm_arg = valToOperand(s.arena, reg_arg);
                    instructions.append(.mov(asm_arg, .register(reg_to_use, .dword)));
                }

                var iter = std.mem.reverseIterator(stack_args);
                while (iter.next()) |stack_arg| {
                    const asm_arg = valToOperand(s.arena, stack_arg);
                    if (asm_arg == .Register or asm_arg == .Imm) {
                        instructions.append(.push(asm_arg));
                    } else {
                        // push needs either register or immediate value
                        instructions.append(.mov(asm_arg, .register(.ax, .dword)));
                        instructions.append(.push(.register(.ax, .qword)));
                    }
                }

                instructions.append(.call(fn_call.ident));
                const bytes_to_pop = (stack_args.len * PUSH_SIZE) + stack_padding;
                if (bytes_to_pop > 0) {
                    instructions.append(.deallocateStack(bytes_to_pop));
                }
                const dst = valToOperand(s.arena, fn_call.dst);
                instructions.append(.mov(.register(.ax, .dword), dst));
            },
        }
    }
    fn valToOperand(allocator: Allocator, val: Tac.Val) Asm.Operand {
        return switch (val) {
            .Const => |constant| .imm(constant),
            .Var => |ident| .pseudo(allocator.dupe(u8, ident) catch unreachable),
        };
    }
};

/// Replace pseudo variables with stack offsets.
const Stage2 = struct {
    arena: Allocator,
    variable_map: *HashMap(i64),
    symbol_table: *const SymbolTable,

    // Note:
    // this is set to 4 because any variable we add is just 32-bit int as of now
    const STACK_STEP = 4;

    pub fn init(arena: Allocator, symbol_table: *const SymbolTable) Stage2 {
        const map = arena.create(HashMap(i64)) catch unreachable;
        map.* = HashMap(i64).init(arena);
        return .{ .arena = arena, .variable_map = map, .symbol_table = symbol_table };
    }

    pub fn processPg(s: @This(), pg: *Asm.Program) void {
        for (pg.decls.items) |*decl| {
            switch (decl.*) {
                .Fn => |*fn_decl| s.processFn(fn_decl),
                .StaticVar => {}, //noop
            }
        }
    }

    fn processFn(s: @This(), fn_defn: *Asm.FnDecl) void {
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
            .Cmp => |*cmp| {
                cmp.op1 = s.mapOperandToStack(cmp.op1);
                cmp.op2 = s.mapOperandToStack(cmp.op2);
            },
            .SetCC => |*set_cc| {
                set_cc.dst = s.mapOperandToStack(set_cc.dst);
            },
            .Push => |*push| {
                push.* = s.mapOperandToStack(push.*);
            },
            .Call, .DeallocateStack, .Jmp, .JmpCC, .Label, .Cdq, .AllocateStack, .Ret => {}, // noop
        }
    }
    fn mapOperandToStack(s: @This(), operand: Asm.Operand) Asm.Operand {
        if (operand != .Pseudo) return operand;

        const ident = operand.Pseudo;

        if (s.symbol_table.get(ident)) |symbol| {
            if (symbol == .Var and symbol.Var == .Static) return .data(ident);
        }

        if (s.variable_map.getEntry(ident)) |saved_offset| {
            return .stack(saved_offset.value_ptr.*);
        }

        const new_offset: i64 = @as(i64, @intCast(s.variable_map.count() + 1)) * -STACK_STEP;
        s.variable_map.put(ident, new_offset) catch unreachable;
        return .stack(new_offset);
    }
};

/// Fix assembly instructions
const Stage3 = struct {
    arena: Allocator,

    pub fn init(arena: Allocator) @This() {
        return .{ .arena = arena };
    }

    pub fn fixPg(s: @This(), pg: Asm.Program) Asm.Program {
        var decls = ArrayList(Asm.TopLevelDecl).init(s.arena);
        for (pg.decls.items) |it| {
            switch (it) {
                .Fn => |fn_decl| {
                    const fn_defn = s.fixFn(fn_decl);
                    decls.append(fn_defn);
                },
                .StaticVar => decls.append(it),
            }
        }
        return .{ .decls = decls };
    }

    fn isMemoryAddress(op: Asm.Operand) bool {
        return op == .Stack or op == .Data;
    }

    fn fixFn(s: @This(), fn_defn: Asm.FnDecl) Asm.TopLevelDecl {
        var instructions = ArrayList(Asm.Instruction).init(s.arena);
        const ALIGNMENT: usize = STACK_ALIGNMENT;
        const aligned_stack_size = (fn_defn.stack_size + ALIGNMENT - 1) & ~(ALIGNMENT - 1);

        instructions.append(.allocateStack(aligned_stack_size));
        for (fn_defn.instructions.items) |inst| {
            switch (inst) {
                .Mov => |mov| {
                    if (isMemoryAddress(mov.src) and isMemoryAddress(mov.dst)) {
                        instructions.append(.mov(mov.src, .register(.r10, .dword)));
                        instructions.append(.mov(.register(.r10, .dword), mov.dst));
                        continue;
                    }
                    instructions.append(inst);
                },
                .Binary => |binary| {
                    switch (binary.operator) {
                        .LeftShift, .RightShift => {
                            const count_reg: Asm.Operand = .register(.cx, .dword);
                            instructions.append(.mov(binary.operand, count_reg));

                            const dst_reg: Asm.Operand = .register(.r10, .dword);
                            instructions.append(.mov(binary.dst, dst_reg));

                            const result_reg: Asm.Operand = .register(.cx, .byte);
                            instructions.append(.binary(binary.operator, result_reg, dst_reg));
                            instructions.append(.mov(dst_reg, binary.dst));
                        },
                        .BitAnd, .BitOr, .BitXor, .Add, .Subtract => {
                            const reg: Asm.Operand = .register(.r10, .dword);
                            instructions.append(.mov(binary.operand, reg));
                            instructions.append(.binary(binary.operator, reg, binary.dst));
                        },
                        .Multiply => {
                            const reg: Asm.Operand = .register(.r11, .dword);
                            instructions.append(.mov(binary.dst, reg));
                            instructions.append(.binary(binary.operator, binary.operand, reg));
                            instructions.append(.mov(reg, binary.dst));
                        },
                    }
                },
                .IDiv => |operand| {
                    switch (operand) {
                        .Imm => {
                            const reg: Asm.Operand = .register(.r10, .dword);
                            instructions.append(.mov(operand, reg));
                            instructions.append(.idiv(reg));
                        },
                        else => instructions.append(inst),
                    }
                },
                .Cmp => |cmp| {
                    if (isMemoryAddress(cmp.op1) and isMemoryAddress(cmp.op2)) {
                        const reg: Asm.Operand = .register(.r10, .dword);
                        instructions.append(.mov(cmp.op1, reg));
                        instructions.append(.cmp(reg, cmp.op2));
                    } else if (cmp.op2 == .Imm) {
                        const reg: Asm.Operand = .register(.r11, .dword);
                        instructions.append(.mov(cmp.op2, reg));
                        instructions.append(.cmp(cmp.op1, reg));
                    } else {
                        instructions.append(inst);
                    }
                },
                .Jmp,
                .JmpCC,
                .Label,
                .SetCC,
                .Cdq,
                .Unary,
                .AllocateStack,
                .Ret,
                .Call,
                .Push,
                .DeallocateStack,
                => instructions.append(inst),
            }
        }
        return .fnDecl(s.arena, fn_defn.ident, aligned_stack_size, fn_defn.global, instructions);
    }
};

pub const Asm = struct {
    pub const Program = struct {
        decls: ArrayList(TopLevelDecl),
    };
    pub const TopLevelDecl = union(enum) {
        Fn: FnDecl,
        StaticVar: StaticVar,

        pub fn staticVar(allocator: Allocator, ident: []const u8, global: bool, initializer: i32) @This() {
            const owned_ident = allocator.dupe(u8, ident) catch unreachable;
            return .{
                .StaticVar = .{
                    .ident = owned_ident,
                    .global = global,
                    .initializer = initializer,
                },
            };
        }
        pub fn fnDecl(allocator: Allocator, ident: []const u8, stack_size: usize, global: bool, instructions: ArrayList(Instruction)) @This() {
            const owned_ident = allocator.dupe(u8, ident) catch unreachable;
            return .{
                .Fn = .{
                    .ident = owned_ident,
                    .stack_size = stack_size,
                    .global = global,
                    .instructions = instructions,
                },
            };
        }
    };
    pub const StaticVar = struct {
        ident: []const u8,
        global: bool,
        initializer: i32,
    };

    pub const FnDecl = struct {
        ident: []const u8,
        stack_size: usize,
        global: bool,
        instructions: ArrayList(Instruction),
    };
    const Instructions = ArrayList(Instruction);
    pub const Instruction = union(enum) {
        Mov: struct { src: Operand, dst: Operand },
        Unary: struct { operator: UnaryOperator, operand: Operand },
        Binary: struct { operator: BinaryOperator, operand: Operand, dst: Operand },
        Cmp: struct { op1: Operand, op2: Operand },
        Jmp: []const u8,
        JmpCC: struct { condition_code: ConditionCode, label: []const u8 },
        SetCC: struct { condition_code: ConditionCode, dst: Operand },
        Label: []const u8,
        IDiv: Operand,
        Cdq,
        AllocateStack: usize,
        Call: []const u8,
        DeallocateStack: usize,
        Push: Operand,
        Ret,

        pub fn call(fn_name: []const u8) Instruction {
            return .{ .Call = fn_name };
        }

        pub fn deallocateStack(size: usize) Instruction {
            return .{ .DeallocateStack = size };
        }

        pub fn push(operand: Operand) Instruction {
            return .{ .Push = operand };
        }

        pub fn cmp(op1: Operand, op2: Operand) Instruction {
            return .{ .Cmp = .{ .op1 = op1, .op2 = op2 } };
        }

        pub fn jmp(label_name: []const u8) Instruction {
            return .{ .Jmp = label_name };
        }

        pub fn jmpCC(condition_code: ConditionCode, label_name: []const u8) Instruction {
            return .{ .JmpCC = .{ .condition_code = condition_code, .label = label_name } };
        }

        pub fn setCC(condition_code: ConditionCode, dst: Operand) Instruction {
            return .{ .SetCC = .{ .condition_code = condition_code, .dst = dst } };
        }

        pub fn label(name: []const u8) Instruction {
            return .{ .Label = name };
        }

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

    pub const ConditionCode = enum {
        EqualEqual,
        NotEqual,
        LessThan,
        LessThanEqual,
        GreaterThan,
        GreaterThanEqual,
    };

    pub const UnaryOperator = enum {
        Neg,
        BitNot,
    };
    pub const BinaryOperator = enum {
        Add,
        Subtract,
        Multiply,
        LeftShift,
        RightShift,
        BitAnd,
        BitOr,
        BitXor,
    };
    pub const Operand = union(enum) {
        Imm: i64,
        Register: Register,
        Pseudo: []const u8,
        Stack: i64,
        Data: []const u8,

        pub fn data(ident: []const u8) Operand {
            return .{ .Data = ident };
        }

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
            cx,
            sp,
            dx,
            di,
            si,
            rip,
            r8,
            r9,
            r10,
            r11,
        };
        const Size = enum(u8) {
            byte = 1,
            word = 2,
            dword = 4,
            qword = 8,
        };

        pub fn format(self: @This(), writer: anytype) !void {
            const value = switch (self.type) {
                .rip => switch (self.size) {
                    .byte, .word, .dword => std.debug.panic("** compiler bug ** rip cannot be used in these sizes", .{}),
                    .qword => "%rip",
                },
                .ax => switch (self.size) {
                    .byte => "%al",
                    .word => "%ax",
                    .dword => "%eax",
                    .qword => "%rax",
                },
                .cx => switch (self.size) {
                    .byte => "%cl",
                    .word => "%cx",
                    .dword => "%ecx",
                    .qword => "%rcx",
                },
                .di => switch (self.size) {
                    .byte => "%dil",
                    .word => "%di",
                    .dword => "%edi",
                    .qword => "%rdi",
                },
                .dx => switch (self.size) {
                    .byte => "%dl",
                    .word => "%dx",
                    .dword => "%edx",
                    .qword => "%rdx",
                },
                .si => switch (self.size) {
                    .byte => "%sil",
                    .word => "%si",
                    .dword => "%esi",
                    .qword => "%rsi",
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
                .r9 => switch (self.size) {
                    .byte => "%r9b",
                    .word => "%r9w",
                    .dword => "%r9d",
                    .qword => "%r9",
                },
                .r8 => switch (self.size) {
                    .byte => "%r8b",
                    .word => "%r8w",
                    .dword => "%r8d",
                    .qword => "%r8",
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
    writer: *std.Io.Writer,

    pub fn print(writer: *std.Io.Writer, pg: Asm.Program, title: []const u8) void {
        const s = Printer{ .writer = writer };
        s.writeFmt("-- Codegen: {s} --\n", .{title});
        for (pg.decls.items) |decl| {
            switch (decl) {
                .Fn => |fn_decl| {
                    s.printFnDecl(fn_decl, 0);
                    s.write("\n");
                },
                .StaticVar => |static_var| {
                    s.printSpace(1);
                    s.write(".data\n");
                    s.writeFmt(".{s}: .long {d}\n", .{ static_var.ident, static_var.initializer });
                },
            }
        }
    }
    fn printFnDecl(s: @This(), fn_defn: Asm.FnDecl, depth: usize) void {
        s.printSpace(depth);
        s.writeFmt("{s}:\n", .{fn_defn.ident});
        for (fn_defn.instructions.items) |inst| {
            s.printInst(inst, depth + 1);
        }
    }
    fn printInst(s: @This(), inst: Asm.Instruction, depth: usize) void {
        s.printSpace(depth);
        switch (inst) {
            .Call => |call_inst| {
                s.write("Call ");
                s.writeFmt("{s}", .{call_inst});
            },
            .DeallocateStack => |size| {
                s.writeFmt("DeallocateStack {d}", .{size});
            },
            .Push => |operand| {
                s.write("Push ");
                s.printOperand(operand);
            },
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
                s.write("Unary ");
                switch (unary.operator) {
                    .Neg => s.write("Neg"),
                    .BitNot => s.write("BitNot"),
                }
                s.write(", ");
                s.printOperand(unary.operand);
            },
            .Binary => |binary| {
                s.write("Binary ");
                switch (binary.operator) {
                    .Add => s.write("Add"),
                    .Subtract => s.write("Subtract"),
                    .Multiply => s.write("Multiply"),
                    .LeftShift => s.write("LeftShift"),
                    .RightShift => s.write("RightShift"),
                    .BitAnd => s.write("BitAnd"),
                    .BitOr => s.write("BitOr"),
                    .BitXor => s.write("BitXor"),
                }
                s.write(", ");
                s.printOperand(binary.operand);
                s.write(", ");
                s.printOperand(binary.dst);
            },
            .IDiv => |operand| {
                s.write("IDiv ");
                s.printOperand(operand);
            },
            .Cmp => |cmp| {
                s.write("Cmp ");
                s.printOperand(cmp.op1);
                s.write(", ");
                s.printOperand(cmp.op2);
            },
            .Jmp => |jmp| {
                s.write("Jmp ");
                s.writeFmt("{s})", .{jmp});
            },
            .JmpCC => |jmp| {
                s.write("JmpCC ");
                s.writeFmt("{s}, ", .{@tagName(jmp.condition_code)});
                s.writeFmt("{s})", .{jmp.label});
            },
            .SetCC => |setcc| {
                s.write("SetCC ");
                s.writeFmt("{s}, ", .{@tagName(setcc.condition_code)});
                s.printOperand(setcc.dst);
            },
            .Label => |label| {
                s.write("Label ");
                s.writeFmt("{s})", .{label});
            },
            .Cdq => {
                s.write("Cdq");
            },
            .AllocateStack => |size| {
                s.writeFmt("AllocateStack {d}", .{size});
            },
        }
        s.write("\n");
    }

    fn printOperand(s: @This(), op: Asm.Operand) void {
        switch (op) {
            .Imm => |imm| s.writeFmt("Imm({d})", .{imm}),
            .Pseudo => |pseudo| s.writeFmt("Pseudo({s})", .{pseudo}),
            .Stack => |stack| s.writeFmt("Stack({d})", .{stack}),
            .Data => |ident| s.writeFmt("Data({s})", .{ident}),
            .Register => |r| s.printRegister(r),
        }
    }

    fn printRegister(s: @This(), reg: Asm.Register) void {
        s.writeFmt("Register({f})", .{reg});
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
// const ArrayList = std.ArrayListq
const ArrayList = @import("from_scratch.zig").ArrayList;
const HashMap = std.StringHashMap;
const SymbolTable = @import("SymbolTable.zig");
const AllocatingPrinter = @import("util.zig").Printer;
