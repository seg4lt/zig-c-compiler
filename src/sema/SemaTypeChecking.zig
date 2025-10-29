arena: Allocator,
scratch_arena: Allocator,
error_reporter: *ErrorReporter,
random: std.Random,
symbol_table: *SymbolTable,

const Self = @This();

pub fn check(opt: SemaOptions) SemaError!void {
    var self = Self{
        .arena = opt.arena,
        .scratch_arena = opt.scratch_arena,
        .symbol_table = opt.symbol_table,
        .error_reporter = opt.error_reporter,
        .random = opt.random,
    };

    self.checkPg(opt.program) catch |e| {
        var printer = Printer.init(self.scratch_arena);
        self.error_reporter.printError(printer.writer());
        printer.printToStdErr(.{}) catch return SemaError.PrintFailed;
        return e;
    };
}

fn checkPg(s: Self, pg: *const Ast.Program) SemaError!void {
    for (pg.decls.items) |decl| {
        switch (decl.*) {
            .Fn => |fn_decl| try s.checkFn(fn_decl, true),
            .Var => |var_decl| try s.checkFileScopeVarDecl(var_decl),
        }
    }
}

fn getVarInitialValue(s: Self, var_decl: *const Ast.VarDecl) SemaError!Symbol.InitialValue {
    const initializer = var_decl.initializer orelse {
        if (var_decl.storage_class == .Extern) {
            return .no_initializer;
        }
        return .tentative;
    };
    if (initializer.* != .Constant) {
        try s.semaError(
            SemaError.InvalidVarInitializer,
            var_decl.loc.line,
            var_decl.loc.start,
            "file scope variable initializer must be a constant expression",
            .{},
        );
    }
    return .initialValue(initializer.Constant.value);
}

fn checkFileScopeVarDecl(s: Self, var_decl: *const Ast.VarDecl) SemaError!void {
    var initial_value = try s.getVarInitialValue(var_decl);
    var is_global = var_decl.storage_class != .Static;

    if (s.symbol_table.get(var_decl.ident)) |entry| {
        std.debug.assert(entry == .Var);
        const prev_attribute = entry.Var;
        if (prev_attribute != .Static) {
            try s.semaError(
                SemaError.InvalidFileScopeVarDeclaration,
                var_decl.loc.line,
                var_decl.loc.start,
                "file scope variable {s} should have static storage class",
                .{var_decl.ident},
            );
        }

        // this is always static as we already to != .Static check above
        const prev_static_attr = prev_attribute.Static;

        if (var_decl.storage_class == .Extern) {
            is_global = prev_static_attr.global;
        } else if (prev_static_attr.global != is_global) {
            try s.semaError(
                SemaError.InvalidFileScopeVarDeclaration,
                var_decl.loc.line,
                var_decl.loc.start,
                "conflicting linkage for variable {s}",
                .{var_decl.ident},
            );
        }
        
        const prev_initial_value = prev_static_attr.initial_value;
        if (prev_initial_value == .initial) {
            if (initial_value == .initial) {
                try s.semaError(
                    SemaError.InvalidFileScopeVarDeclaration,
                    var_decl.loc.line,
                    var_decl.loc.start,
                    "redefinition of variable {s}",
                    .{var_decl.ident},
                );
            }
            initial_value = prev_initial_value;
        } else if (initial_value != .initial and prev_initial_value == .tentative) {
            initial_value = .tentative;
        }
    }
    s.symbol_table.put(var_decl.ident, .staticVarSymbol(s.symbol_table.arena, var_decl.ident, is_global, initial_value));
}

fn checkFn(s: Self, fn_decl: *Ast.FnDecl, file_scope: bool) SemaError!void {
    var fn_params = ArrayList(SymbolTable.Symbol.FnSymbol.Param).init(s.symbol_table.arena);
    for (fn_decl.params.items) |param| {
        fn_params.append(.fnParam(s.symbol_table.arena, param.ident, "int"));
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
            .Var => try s.semaError(
                SemaError.InvalidType,
                fn_decl.loc.line,
                fn_decl.loc.start,
                "function name conflicts with variable name: {s}",
                .{fn_decl.name},
            ),
            .Fn => |saved_fn| {
                already_defined = saved_fn.defined;

                if (already_defined and has_body) {
                    try s.semaError(
                        SemaError.DuplicateFnDeclaration,
                        fn_decl.loc.line,
                        fn_decl.loc.start,
                        "duplicate function decleration: {s}",
                        .{fn_decl.name},
                    );
                }

                if (saved_fn.global and fn_decl.storage_class == .Static) {
                    try s.semaError(
                        SemaError.InvalidFnLinkage,
                        fn_decl.loc.line,
                        fn_decl.loc.start,
                        "conflicting linkage for function {s}",
                        .{fn_decl.name},
                    );
                }

                // if previous declaration is not global (aka static)
                // and new declaration is global, we keep old linkage i.e. static
                is_global &= saved_fn.global;

                if (fn_decl.params.items.len != saved_fn.params.items.len) {
                    try s.semaError(
                        SemaError.InvalidFnParamCount,
                        fn_decl.loc.line,
                        fn_decl.loc.start,
                        "function parameter count mismatch for {s}: expected {d}, got {d}",
                        .{ fn_decl.name, saved_fn.params.items.len, fn_decl.params.items.len },
                    );
                }
            },
        }
    }

    // If we don't have this symbol on our table add one
    // Or we always update symbol table if current AST we are type checking happens to have a body
    // this is to make sure during ASM_IR phase we have same names for params
    // Maybe this is not right way to do this as, this is kind of hidden dependency of sort??
    if (!found_on_symbol_table or has_body) {
        const fn_type: Symbol = .fnSymbol(s.symbol_table.arena, fn_decl.name, fn_params, is_global, has_body or already_defined);
        s.symbol_table.put(fn_decl.name, fn_type);
    }

    if (has_body) {
        for (fn_decl.params.items) |it| s.symbol_table.put(it.ident, .localVarSymbol(s.symbol_table.arena, it.ident));
        if (fn_decl.body) |body| try s.checkBlock(body);
    }
}

fn checkBlock(s: Self, block: *Ast.Block) SemaError!void {
    for (block.block_item.items) |item| {
        try s.checkBlockItem(item);
    }
}

fn checkBlockItem(s: Self, block_item: *Ast.BlockItem) SemaError!void {
    switch (block_item.*) {
        .Decl => |decl| try s.checkDecl(decl, .{ .file_scope = false, .static_allowed = true }),
        .Stmt => |stmt| try s.checkStmt(stmt),
    }
}

const CheckDeclOption = struct {
    file_scope: bool,
    static_allowed: bool,
};

fn checkDecl(s: Self, decl: *Ast.Decl, opt: CheckDeclOption) SemaError!void {
    switch (decl.*) {
        .Var => |var_decl| try s.checkVarDecl(var_decl, opt.static_allowed),
        .Fn => |fn_decl| try s.checkFn(fn_decl, opt.file_scope),
    }
}

fn checkVarDecl(s: Self, var_decl: *Ast.VarDecl, static_allowed: bool) SemaError!void {
    const storage_class = var_decl.storage_class orelse {
        s.symbol_table.put(var_decl.ident, .localVarSymbol(s.symbol_table.arena, var_decl.ident));
        if (var_decl.initializer) |initializer| try s.checkExpr(initializer);
        return;
    };

    switch (storage_class) {
        .Extern => {
            if (var_decl.initializer != null) {
                try s.semaError(
                    SemaError.InvalidVarInitializer,
                    var_decl.loc.line,
                    var_decl.loc.start,
                    "extern variable cannot have an initializer: {s}",
                    .{var_decl.ident},
                );
            }
            if (s.symbol_table.get(var_decl.ident)) |prev_entry| {
                if (prev_entry != .Var) try s.semaError(SemaError.InvalidType, var_decl.loc.line, var_decl.loc.start, "variable name conflicts with function name: {s}", .{var_decl.ident});
            } else {
                s.symbol_table.put(var_decl.ident, .staticVarSymbol(s.symbol_table.arena, var_decl.ident, true, .no_initializer));
            }
        },
        .Static => {
            if (!static_allowed) {
                try s.semaError(
                    SemaError.InvalidPlacement,
                    var_decl.loc.line,
                    var_decl.loc.start,
                    "static storage class not allowed here: {s}",
                    .{var_decl.ident},
                );
            }
            const initial_value: Symbol.InitialValue = blk: {
                const initializer = var_decl.initializer orelse {
                    break :blk .initialValue(0);
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
                break :blk .initialValue(initializer.Constant.value);
            };
            s.symbol_table.put(var_decl.ident, .staticVarSymbol(s.symbol_table.arena, var_decl.ident, false, initial_value));
        },
    }
}

fn checkStmt(s: Self, stmt: *Ast.Stmt) SemaError!void {
    switch (stmt.*) {
        .Return => |return_stmt| try s.checkExpr(return_stmt.expr),
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

fn checkExpr(s: Self, expr: *Ast.Expr) SemaError!void {
    switch (expr.*) {
        .Prefix => |prefix_expr| try s.checkExpr(prefix_expr.expr),
        .Var => |var_expr| {
            if (s.symbol_table.get(var_expr.ident)) |saved_ident| {
                if (saved_ident == .Fn) {
                    try s.semaError(
                        SemaError.InvalidPlacement,
                        var_expr.loc.line,
                        var_expr.loc.start,
                        "variable cannot be a function: {s}",
                        .{var_expr.ident},
                    );
                }
            }
        },
        .Unary => |unary_expr| try s.checkExpr(unary_expr.expr),
        .Binary => |binary_expr| {
            try s.checkExpr(binary_expr.left);
            try s.checkExpr(binary_expr.right);
        },
        .Assignment => |assignment_expr| {
            try s.checkExpr(assignment_expr.src);
            try s.checkExpr(assignment_expr.dst);
        },
        .Group => |group_expr| try s.checkExpr(group_expr.expr),
        .Postfix => |pe| try s.checkExpr(pe.expr),
        .Ternary => |ternary_expr| {
            try s.checkExpr(ternary_expr.condition);
            try s.checkExpr(ternary_expr.then);
            try s.checkExpr(ternary_expr.@"else");
        },
        .FnCall => |fn_call| {
            const saved_symbol = s.symbol_table.get(fn_call.ident) orelse {
                try s.semaError(
                    SemaError.UndeclaredFunction,
                    fn_call.loc.line,
                    fn_call.loc.start,
                    "function {s} not defined",
                    .{fn_call.ident},
                );
            };
            if (saved_symbol != .Fn) {
                try s.semaError(
                    SemaError.InvalidType,
                    fn_call.loc.line,
                    fn_call.loc.start,
                    "function call expected, but found {s}",
                    .{@tagName(saved_symbol)},
                );
            }
            const fn_syn = saved_symbol.Fn;
            if (fn_syn.params.items.len != fn_call.args.items.len) {
                try s.semaError(
                    SemaError.WrongNumberOfArgument,
                    fn_call.loc.line,
                    fn_call.loc.start,
                    "fn {s} expected {d} argument got {d}",
                    .{
                        fn_call.ident,
                        fn_syn.params.items.len,
                        fn_call.args.items.len,
                    },
                );
            }
            for (fn_call.args.items) |arg| {
                try s.checkExpr(arg);
            }
        },
        .Constant => {},
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
const Ast = @import("../AstParser.zig").Ast;
const Printer = @import("../util.zig").Printer;
