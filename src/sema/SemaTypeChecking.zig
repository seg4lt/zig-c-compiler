arena: Allocator,
scratch_arena: Allocator,
error_reporter: *ErrorReporter,
random: std.Random,
symbol_table: *SymbolTable,
return_type_stack: ArrayList(*Ast.BuiltinType),

const Self = @This();

pub fn check(opt: SemaOptions) SemaError!void {
    var self = Self{
        .arena = opt.arena,
        .scratch_arena = opt.scratch_arena,
        .symbol_table = opt.symbol_table,
        .error_reporter = opt.error_reporter,
        .random = opt.random,
        .return_type_stack = ArrayList(*Ast.BuiltinType).init(opt.arena),
    };

    self.checkPg(opt.program) catch |e| {
        var printer = Printer.init(self.scratch_arena);
        self.error_reporter.printError(printer.writer());
        printer.printToStdErr(.{}) catch return SemaError.PrintFailed;
        return e;
    };
}

fn checkPg(s: *Self, pg: *Ast.Program) SemaError!void {
    for (pg.decls.items) |decl| {
        switch (decl.*) {
            .Fn => |fn_decl| try s.checkFn(fn_decl, true),
            .Var => |var_decl| try s.checkFileScopeVarDecl(var_decl),
        }
    }
}

fn getVarInitialValue(s: *Self, var_decl: *Ast.VarDecl) SemaError!Symbol.InitialValue {
    const initializer = var_decl.initializer orelse {
        if (var_decl.storage_class == .Extern) {
            return .no_initializer;
        }
        return .tentative;
    };
    if (initializer.* != .Constant) try s.semaError(SemaError.InvalidVarInitializer, var_decl.loc.line, var_decl.loc.start, "file scope variable initializer must be a constant expression", .{});

    return .initialValue(.init(initializer.Constant.value));
}

fn checkFileScopeVarDecl(s: *Self, var_decl: *Ast.VarDecl) SemaError!void {
    var initial_value = try s.getVarInitialValue(var_decl);
    if (var_decl.initializer) |init| {
        try s.checkExpr(init);
        std.log.debug("type = {any}, ident = {s}", .{ var_decl.type, var_decl.ident });
        init.setType(var_decl.type);
    }

    var is_global = var_decl.storage_class != .Static;

    if (s.symbol_table.get(var_decl.ident)) |entry| {
        const prev_attribute = entry.Var;

        if (!isSameType(var_decl.type, prev_attribute.getType())) {
            try s.semaError(SemaError.InvalidType, var_decl.loc.line, var_decl.loc.start, "type mismatch", .{});
        }

        if (prev_attribute != .Static) {
            try s.semaError(SemaError.InvalidFileScopeVarDeclaration, var_decl.loc.line, var_decl.loc.start, "file scope variable {s} should have static storage class", .{var_decl.ident});
        }

        // this is always static as we already to != .Static check above
        const prev_static_attr = prev_attribute.Static;

        if (var_decl.storage_class == .Extern) {
            is_global = prev_static_attr.global;
        } else if (prev_static_attr.global != is_global) {
            try s.semaError(SemaError.InvalidFileScopeVarDeclaration, var_decl.loc.line, var_decl.loc.start, "conflicting linkage for variable {s}", .{var_decl.ident});
        }

        const prev_initial_value = prev_static_attr.initial_value;
        if (prev_initial_value == .initial) {
            if (initial_value == .initial) {
                try s.semaError(SemaError.InvalidFileScopeVarDeclaration, var_decl.loc.line, var_decl.loc.start, "redefinition of variable {s}", .{var_decl.ident});
            }
            initial_value = prev_initial_value;
        } else if (initial_value != .initial and prev_initial_value == .tentative) {
            initial_value = .tentative;
        }
    }
    s.symbol_table.put(
        var_decl.ident,
        .staticVarSymbol(
            s.symbol_table.arena,
            var_decl.ident,
            is_global,
            initial_value,
            var_decl.type.clone(s.symbol_table.arena),
        ),
    );
}

fn checkFn(s: *Self, fn_decl: *Ast.FnDecl, file_scope: bool) SemaError!void {
    var fn_params = ArrayList([]const u8).init(s.symbol_table.arena);
    for (fn_decl.params.items) |param| {
        fn_params.append(s.symbol_table.arena.dupe(u8, param.ident) catch unreachable);
    }

    const has_body = fn_decl.body != null;
    var already_defined = false;
    var is_global = fn_decl.storage_class != .Static;

    if (!file_scope and fn_decl.storage_class == .Static) {
        try s.semaError(
            SemaError.InvalidPlacement,
            fn_decl.loc.line,
            fn_decl.loc.start,
            "static storage class not allowed here: {s}",
            .{fn_decl.name},
        );
    }
    var found_on_symbol_table: bool = false;

    if (s.symbol_table.get(fn_decl.name)) |prev_entry| {
        found_on_symbol_table = true;
        switch (prev_entry) {
            .Var => try s.semaError(SemaError.InvalidType, fn_decl.loc.line, fn_decl.loc.start, "function name conflicts with variable name: {s}", .{fn_decl.name}),
            .Fn => |saved_fn| {
                already_defined = saved_fn.defined;
                if (!isSameType(saved_fn.type, fn_decl.type)) try s.semaError(SemaError.InvalidType, fn_decl.loc.line, fn_decl.loc.start, "function type mismatch for {s}", .{fn_decl.name});
                if (already_defined and has_body) try s.semaError(SemaError.DuplicateFnDeclaration, fn_decl.loc.line, fn_decl.loc.start, "duplicate function decleration: {s}", .{fn_decl.name});
                if (saved_fn.global and fn_decl.storage_class == .Static) try s.semaError(SemaError.InvalidFnLinkage, fn_decl.loc.line, fn_decl.loc.start, "conflicting linkage for function {s}", .{fn_decl.name});

                // if previous declaration is not global (aka static)
                // and new declaration is global, we keep old linkage i.e. static
                is_global &= saved_fn.global;
            },
        }
    }

    // If we don't have this symbol on our table add one
    // Or we always update symbol table if current AST we are type checking happens to have a body
    // this is to make sure during ASM_IR phase we have same names for params
    // Maybe this is not right way to do this as, this is kind of hidden dependency of sort??
    if (!found_on_symbol_table or has_body) {
        const fn_symbol: Symbol = .fnSymbol(s.symbol_table.arena, fn_decl.name, fn_params, is_global, has_body or already_defined, fn_decl.type.?.clone(s.symbol_table.arena));
        s.symbol_table.put(fn_decl.name, fn_symbol);
    }

    if (has_body) {
        std.debug.assert(fn_decl.type != null);

        for (fn_decl.params.items, 0..) |it, i| {
            const fn_param_type = fn_decl.type.?.Fn.params.items[i].clone(s.symbol_table.arena);
            s.symbol_table.put(it.ident, .localVarSymbol(s.symbol_table.arena, it.ident, fn_param_type));
        }
        if (fn_decl.body) |body| {
            const fn_return_type = fn_decl.type.?.Fn.return_type;
            s.return_type_stack.append(fn_return_type);
            defer std.debug.assert(s.return_type_stack.pop() != null);
            try s.checkBlock(body);
        }
    }
}

fn checkBlock(s: *Self, block: *Ast.Block) SemaError!void {
    for (block.block_item.items) |item| {
        try s.checkBlockItem(item);
    }
}

fn checkBlockItem(s: *Self, block_item: *Ast.BlockItem) SemaError!void {
    switch (block_item.*) {
        .Decl => |decl| try s.checkDecl(decl, .{ .file_scope = false, .static_allowed = true }),
        .Stmt => |stmt| try s.checkStmt(stmt),
    }
}

const CheckDeclOption = struct {
    file_scope: bool,
    static_allowed: bool,
};

fn checkDecl(s: *Self, decl: *Ast.Decl, opt: CheckDeclOption) SemaError!void {
    switch (decl.*) {
        .Var => |var_decl| try s.checkVarDecl(var_decl, opt.static_allowed),
        .Fn => |fn_decl| try s.checkFn(fn_decl, opt.file_scope),
    }
}

fn checkVarDecl(s: *Self, var_decl: *Ast.VarDecl, static_allowed: bool) SemaError!void {
    const storage_class = var_decl.storage_class orelse {
        s.symbol_table.put(var_decl.ident, .localVarSymbol(s.symbol_table.arena, var_decl.ident, var_decl.type.clone(s.symbol_table.arena)));
        if (var_decl.initializer) |initializer| {
            try s.checkExpr(initializer);
            var_decl.type = initializer.getType().?;
        }
        return;
    };
    if (var_decl.initializer) |initializer| {
        try s.checkExpr(initializer);
        var_decl.type = initializer.getType().?;
    }
    switch (storage_class) {
        .Extern => {
            if (var_decl.initializer != null) {
                try s.semaError(SemaError.InvalidVarInitializer, var_decl.loc.line, var_decl.loc.start, "extern variable cannot have an initializer: {s}", .{var_decl.ident});
            }
            if (s.symbol_table.get(var_decl.ident)) |prev_entry| {
                if (prev_entry != .Var) try s.semaError(SemaError.InvalidType, var_decl.loc.line, var_decl.loc.start, "variable name conflicts with function name: {s}", .{var_decl.ident});
                if (!isSameType(var_decl.type, prev_entry.Var.getType())) try s.semaError(SemaError.InvalidType, var_decl.loc.line, var_decl.loc.start, "type mismatch", .{});
            } else {
                s.symbol_table.put(var_decl.ident, .staticVarSymbol(s.symbol_table.arena, var_decl.ident, true, .no_initializer, var_decl.type.clone(s.symbol_table.arena)));
            }
        },
        .Static => {
            if (!static_allowed) {
                try s.semaError(SemaError.InvalidPlacement, var_decl.loc.line, var_decl.loc.start, "static storage class not allowed here: {s}", .{var_decl.ident});
            }
            const initial_value: Symbol.InitialValue = blk: {
                const initializer = var_decl.initializer orelse {
                    break :blk .initialValue(.int(0));
                };
                if (initializer.* != .Constant) {
                    try s.semaError(
                        SemaError.InvalidVarInitializer,
                        var_decl.loc.line,
                        var_decl.loc.start,
                        "static variable initializer must be a constant expression: {s}",
                        .{var_decl.ident},
                    );
                }
                break :blk .initialValue(.init(initializer.Constant.value));
            };
            s.symbol_table.put(var_decl.ident, .staticVarSymbol(s.symbol_table.arena, var_decl.ident, false, initial_value, var_decl.type.clone(s.symbol_table.arena)));
        },
    }
}

fn checkStmt(s: *Self, stmt: *Ast.Stmt) SemaError!void {
    switch (stmt.*) {
        .Return => |*return_stmt| {
            const fn_return_type = s.return_type_stack.getLast();
            return_stmt.expr = maybeExplicitCast(s.arena, return_stmt.expr, fn_return_type);
            try s.checkExpr(return_stmt.expr);
        },
        .Expr => |expr_stmt| try s.checkExpr(expr_stmt.expr),
        .If => |if_stmt| {
            try s.checkExpr(if_stmt.condition);
            try s.checkStmt(if_stmt.then);
            if (if_stmt.@"else") |else_block| {
                try s.checkStmt(else_block);
            }
        },
        .Compound => |compound_stmt| {
            try s.checkBlock(compound_stmt.body);
        },
        .Label => |label_stmt| try s.checkStmt(label_stmt.stmt),
        .DoWhile => |do_while_stmt| {
            try s.checkStmt(do_while_stmt.body);
            try s.checkExpr(do_while_stmt.condition);
        },
        .While => |while_stmt| {
            try s.checkExpr(while_stmt.condition);
            try s.checkStmt(while_stmt.body);
        },
        .For => |for_stmt| {
            switch (for_stmt.init.*) {
                .Decl => |decl| try s.checkDecl(decl, .{ .file_scope = false, .static_allowed = false }),
                .Expr => |expr| if (expr) |init| try s.checkExpr(init),
            }
            if (for_stmt.condition) |condition| try s.checkExpr(condition);
            if (for_stmt.post) |post| try s.checkExpr(post);
            try s.checkStmt(for_stmt.body);
        },
        .Switch => |switch_stmt| {
            try s.checkExpr(switch_stmt.condition);
            if (switch_stmt.condition.* == .Var) {
                if (s.symbol_table.get(switch_stmt.condition.Var.ident)) |saved_ident| {
                    if (saved_ident == .Fn) {
                        try s.semaError(
                            SemaError.InvalidPlacement,
                            switch_stmt.condition.Var.loc.line,
                            switch_stmt.condition.Var.loc.start,
                            "switch condition cannot be a function: {s}",
                            .{switch_stmt.condition.Var.ident},
                        );
                    }
                }
            }
            for (switch_stmt.body.items) |it| {
                try s.checkBlockItem(it);
            }
        },
        .Continue, .Break, .Case, .Default, .Null, .Goto => {}, // noop
    }
}

fn checkExpr(s: *Self, expr: *Ast.Expr) SemaError!void {
    switch (expr.*) {
        .Cast => |cast_expr| {
            try s.checkExpr(cast_expr.expr);
            expr.setType(cast_expr.target_type);
        },
        .Prefix => |prefix_expr| {
            try s.checkExpr(prefix_expr.expr);
            expr.setType(prefix_expr.expr.getType().?);
        },
        .Var => |var_expr| {
            if (s.symbol_table.get(var_expr.ident)) |saved_ident| {
                if (saved_ident == .Fn) try s.semaError(SemaError.InvalidPlacement, var_expr.loc.line, var_expr.loc.start, "variable cannot be a function: {s}", .{var_expr.ident});
                const var_type = switch (saved_ident.Var) {
                    .Static => |static_var| static_var.type,
                    .Local => |local_var| local_var.type,
                };
                expr.setType(var_type);
            } else {
                @panic("can this happen? Maybe var decl will always put this on symbol table?");
            }
        },
        .Unary => |unary_expr| {
            try s.checkExpr(unary_expr.expr);
            expr.setType(unary_expr.expr.getType().?);
        },
        .Binary => |*binary_expr| {
            try s.checkExpr(binary_expr.left);
            try s.checkExpr(binary_expr.right);

            _, const common_type = getCommonType(
                s.arena,
                binary_expr.left.getType() orelse std.debug.panic("** compiler bug ** binary left expr should already have type", .{}),
                binary_expr.right.getType() orelse std.debug.panic("** compiler bug ** binary right expr should already have type", .{}),
            );
            binary_expr.left = maybeExplicitCast(s.arena, binary_expr.left, common_type.?);
            binary_expr.right = maybeExplicitCast(s.arena, binary_expr.right, common_type.?);

            switch (binary_expr.operator) {
                .And, .Or, .BitAnd, .BitOr, .BitXor, .LeftShift, .RightShift, .EqualEqual, .NotEqual, .LessThan, .GreaterThan, .GreaterThanEqual, .LessThanEqual => expr.setType(.intType(s.arena)),
                .Add,
                .Subtract,
                .Multiply,
                .Divide,
                .Mod,
                => expr.setType(common_type.?),
            }
        },
        .Assignment => |*assignment_expr| {
            try s.checkExpr(assignment_expr.src);
            try s.checkExpr(assignment_expr.dst);
            assignment_expr.src = maybeExplicitCast(s.arena, assignment_expr.src, assignment_expr.dst.getType().?);
            expr.setType(assignment_expr.dst.getType().?);
        },
        .Group => |group_expr| {
            try s.checkExpr(group_expr.expr);
            expr.setType(group_expr.expr.getType().?);
        },
        .Postfix => |pe| {
            try s.checkExpr(pe.expr);
            expr.setType(pe.expr.getType().?);
        },
        .Ternary => |*ternary_expr| {
            try s.checkExpr(ternary_expr.condition);
            try s.checkExpr(ternary_expr.then);
            try s.checkExpr(ternary_expr.@"else");

            _, const common_type = getCommonType(s.arena, ternary_expr.then.getType().?, ternary_expr.@"else".getType().?);
            ternary_expr.then = maybeExplicitCast(s.arena, ternary_expr.then, common_type.?);
            ternary_expr.@"else" = maybeExplicitCast(s.arena, ternary_expr.@"else", common_type.?);
            expr.setType(common_type.?);
        },
        .FnCall => |*fn_call| {
            const saved_symbol = s.symbol_table.get(fn_call.ident) orelse try s.semaError(SemaError.UndeclaredFunction, fn_call.loc.line, fn_call.loc.start, "function {s} not defined", .{fn_call.ident});
            if (saved_symbol != .Fn) try s.semaError(SemaError.InvalidType, fn_call.loc.line, fn_call.loc.start, "function call expected, but found {s}", .{@tagName(saved_symbol)});

            const fn_syn = saved_symbol.Fn;
            if (fn_syn.params.items.len != fn_call.args.items.len) try s.semaError(SemaError.WrongNumberOfArgument, fn_call.loc.line, fn_call.loc.start, "fn {s} expected {d} argument got {d}", .{ fn_call.ident, fn_syn.params.items.len, fn_call.args.items.len });

            var processed_args = ArrayList(*Ast.Expr).init(s.arena);
            for (fn_call.args.items, 0..) |arg, i| {
                try s.checkExpr(arg);
                const param_type = fn_syn.type.Fn.params.items[i];
                _, const common_type = getCommonType(s.arena, arg.getType().?, param_type);
                const processed_arg = maybeExplicitCast(s.arena, arg, common_type.?);
                processed_args.append(processed_arg);
            }
            fn_call.args = processed_args;
            expr.setType(saved_symbol.Fn.type.Fn.return_type);
        },
        .Constant => |const_expr| {
            switch (const_expr.value) {
                .Int => expr.setType(.intType(s.arena)),
                .Long => expr.setType(.longType(s.arena)),
            }
        },
    }
}

fn semaError(p: Self, e: SemaError, line: usize, start: usize, comptime fmt: []const u8, args: anytype) SemaError!noreturn {
    p.error_reporter.addError(line, start, fmt, args);
    return e;
}

const std = @import("std");
const Allocator = std.mem.Allocator;
// const ArrayList = std.ArrayList;
const ArrayList = @import("../from_scratch.zig").ArrayList;
const StringHashMap = std.StringHashMap;
const ErrorReporter = @import("../ErrorReporter.zig");
const sema_common = @import("sema_common.zig");
const SemaOptions = sema_common.SemaOptions;
const SemaError = sema_common.SemaError;
const SymbolTable = @import("../SymbolTable.zig");
const Symbol = SymbolTable.Symbol;
const AstParser = @import("../AstParser.zig");
const isSameType = AstParser.isSameType;
const getCommonType = AstParser.getCommonType;
const maybeExplicitCast = AstParser.maybeExplicitCast;
const Ast = @import("../AstParser.zig").Ast;
const Printer = @import("../util.zig").Printer;
