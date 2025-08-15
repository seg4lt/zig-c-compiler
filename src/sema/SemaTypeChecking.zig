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
        self.error_reporter.printError(std.io.getStdErr().writer().any());
        return e;
    };
}

fn checkPg(s: Self, pg: *Ast.Program) SemaError!void {
    for (pg.fns.items) |it| {
        try s.checkFn(it);
    }
}

fn checkFn(s: Self, fn_decl: *Ast.FnDecl) SemaError!void {
    var fn_params = ArrayList(SymbolTable.FnParam).init(s.symbol_table.arena);
    for (fn_decl.params.items) |param| {
        fn_params.append(.fnParam(s.symbol_table.arena, param.ident, "int")) catch unreachable;
    }

    const has_body = fn_decl.body != null;
    var already_defined = false;

    if (s.symbol_table.get(fn_decl.name)) |prev_entry| {
        switch (prev_entry) {
            .Int => try s.semaError(
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
    const fn_type: Symbol = .fnSymbol(s.symbol_table.arena, fn_decl.name, fn_params, has_body or already_defined);
    s.symbol_table.put(fn_decl.name, fn_type) catch unreachable;

    if (has_body) {
        for (fn_decl.params.items) |it| {
            s.symbol_table.put(it.ident, .intSymbol(s.symbol_table.arena, it.ident)) catch unreachable;
        }
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
        .Decl => |decl| try s.checkDecl(decl),
        .Stmt => |stmt| try s.checkStmt(stmt),
    }
}

fn checkDecl(s: Self, decl: *Ast.Decl) SemaError!void {
    switch (decl.*) {
        .Var => |var_decl| try s.checkVarDecl(var_decl),
        .Fn => |fn_decl| try s.checkFn(fn_decl),
    }
}

fn checkVarDecl(s: Self, var_decl: *Ast.VarDecl) SemaError!void {
    _ = s.symbol_table.get(var_decl.ident) orelse {
        s.symbol_table.put(var_decl.ident, .intSymbol(s.symbol_table.arena, var_decl.ident)) catch unreachable;
    };
    if (var_decl.init) |initializer| {
        try s.checkExpr(initializer);
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
                .Decl => |decl| try s.checkDecl(decl),
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
                try s.semaError(SemaError.UndeclaredFunction, fn_call.loc.line, fn_call.loc.start, "function {s} not defined", .{fn_call.ident});
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
                try s.semaError(SemaError.WrongNumberOfArgument, fn_call.loc.line, fn_call.loc.start, "fn {s} expected {d} argument got {d}", .{
                    fn_call.ident,
                    fn_syn.params.items.len,
                    fn_call.args.items.len,
                });
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
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const ErrorReporter = @import("../ErrorReporter.zig");
const sema_common = @import("sema_common.zig");
const SemaOptions = sema_common.SemaOptions;
const SemaError = sema_common.SemaError;
const SymbolTable = @import("../SymbolTable.zig");
const Symbol = SymbolTable.Symbol;
const Ast = @import("../AstParser.zig").Ast;
