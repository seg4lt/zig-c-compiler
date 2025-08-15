arena: Allocator,
scratch_arena: Allocator,
error_reporter: *ErrorReporter,
random: std.Random,

const Self = @This();

pub fn resolve(opt: SemaOptions) SemaError!void {
    var self = Self{
        .arena = opt.arena,
        .scratch_arena = opt.scratch_arena,
        .error_reporter = opt.error_reporter,
        .random = opt.random,
    };

    self.resolvePg(opt.program) catch |e| {
        self.error_reporter.printError(std.io.getStdErr().writer().any());
        return e;
    };
}

fn resolvePg(s: Self, pg: *Ast.Program) SemaError!void {
    var new_scope = ScopeIdents.init(s.arena);

    const can_have_body = true;
    for (pg.fns.items) |it| {
        try s.resolveFnDecl(it, &new_scope, can_have_body);
    }
}

fn semaError(p: Self, e: SemaError, line: usize, start: usize, comptime fmt: []const u8, args: anytype) SemaError!noreturn {
    p.error_reporter.addError(line, start, fmt, args);
    return e;
}

fn resolveFnDecl(s: Self, fn_decl: *Ast.FnDecl, scope: *ScopeIdents, can_have_body: bool) SemaError!void {
    if (scope.get(fn_decl.name)) |saved_name| {
        if (saved_name.type == .Fn and saved_name.has_body) {
            try s.semaError(
                SemaError.DuplicateIdentifier,
                fn_decl.loc.line,
                fn_decl.loc.start,
                "duplicate function declaration `{s}`\n",
                .{fn_decl.name},
            );
        }
        if (saved_name.type == .Var and saved_name.from_current_scope) {
            try s.semaError(
                SemaError.DuplicateIdentifier,
                fn_decl.loc.line,
                fn_decl.loc.start,
                "function name conflicts with variable name `{s}`\n",
                .{fn_decl.name},
            );
        }
    }
    const ident = Ident{
        .from_current_scope = true,
        .type = .Fn,
        .external_linkage = true,
        .name = fn_decl.name,
        .has_body = fn_decl.body != null,
    };
    scope.put(fn_decl.name, ident) catch unreachable;

    var scope_for_fn_param = createNewScope(scope);

    for (fn_decl.params.items) |param| {
        if (scope_for_fn_param.get(param.ident)) |entry| {
            if (entry.from_current_scope) {
                try s.semaError(
                    SemaError.DuplicateIdentifier,
                    param.loc.line,
                    param.loc.start,
                    "duplicate parameter name `{s}` found in function `{s}`\n",
                    .{ param.ident, fn_decl.name },
                );
            }
        }

        const mapped_ident = makeVar(s.arena, s.random, param.ident);

        const param_ident = Ident{
            .from_current_scope = true,
            .type = .Var,
            .external_linkage = false,
            .name = mapped_ident,
            .has_body = false,
        };

        scope_for_fn_param.put(param.ident, param_ident) catch unreachable;
        param.ident = mapped_ident;
    }

    if (!can_have_body and fn_decl.body != null) {
        try s.semaError(
            SemaError.InvalidDeclaration,
            fn_decl.loc.line,
            fn_decl.loc.start,
            "function declaration cannot have body: {s}",
            .{fn_decl.name},
        );
    }

    if (fn_decl.body) |body| try s.resolveBlock(body, &scope_for_fn_param);
}

fn resolveBlock(s: Self, block: *Ast.Block, scope: *ScopeIdents) SemaError!void {
    try s.resolveBlockItem(&block.*.block_item, scope);
}

fn resolveBlockItem(s: Self, block_items: *ArrayList(*Ast.BlockItem), scope: *ScopeIdents) SemaError!void {
    for (block_items.*.items) |item| {
        try switch (item.*) {
            .Decl => |decl| s.resolveDecl(decl, scope),
            .Stmt => |stmt| s.resolveStmt(stmt, scope),
        };
    }
}

fn resolveDecl(s: Self, decl: *Ast.Decl, scope: *ScopeIdents) SemaError!void {
    switch (decl.*) {
        .Fn => |fn_decl| try s.resolveFnDecl(fn_decl, scope, true),
        .Var => |var_decl| {
            if (scope.get(var_decl.ident)) |entry| {
                if (entry.from_current_scope) {
                    if (entry.type == .Var) {
                        try s.semaError(
                            SemaError.DuplicateIdentifier,
                            var_decl.loc.line,
                            var_decl.loc.start,
                            "duplicate variable declaration `{s}`\n",
                            .{var_decl.ident},
                        );
                    }
                    if (entry.type == .Fn and entry.external_linkage) {
                        try s.semaError(
                            SemaError.DuplicateIdentifier,
                            var_decl.loc.line,
                            var_decl.loc.start,
                            "variable name conflicts with function name `{s}`\n",
                            .{var_decl.ident},
                        );
                    }
                }
            }

            const new_ident = makeVar(s.arena, s.random, var_decl.ident);
            const ident = Ident{
                .from_current_scope = true,
                .external_linkage = false,
                .name = new_ident,
                .type = .Var,
                .has_body = false,
            };
            scope.put(var_decl.ident, ident) catch unreachable;

            if (var_decl.init) |initializer| try s.resolveExpr(initializer, scope);
            var_decl.ident = new_ident;
        },
    }
}

fn resolveStmt(s: Self, stmt: *Ast.Stmt, scope: *ScopeIdents) SemaError!void {
    try switch (stmt.*) {
        .Switch => |switch_stmt| {
            try s.resolveExpr(switch_stmt.condition, scope);
            var nested_scope = createNewScope(scope);
            var body = switch_stmt.body;
            try s.resolveBlockItem(&body, &nested_scope);
        },
        .DoWhile => |do_stmt| {
            try s.resolveStmt(do_stmt.body, scope);
            try s.resolveExpr(do_stmt.condition, scope);
        },
        .While => |while_stmt| {
            try s.resolveExpr(while_stmt.condition, scope);
            try s.resolveStmt(while_stmt.body, scope);
        },
        .For => |for_stmt| {
            var scope_for_init = createNewScope(scope);
            switch (for_stmt.init.*) {
                .Decl => |decl| try s.resolveDecl(decl, &scope_for_init),
                .Expr => |expr| if (expr) |initializer| try s.resolveExpr(initializer, &scope_for_init),
            }
            if (for_stmt.condition) |condition| try s.resolveExpr(condition, &scope_for_init);
            if (for_stmt.post) |post| try s.resolveExpr(post, &scope_for_init);

            var scope_for_body = createNewScope(&scope_for_init);
            try s.resolveStmt(for_stmt.body, &scope_for_body);
        },
        .Label => |label_stmt| {
            try s.resolveStmt(label_stmt.stmt, scope);
        },
        .If => |if_stmt| {
            try s.resolveExpr(if_stmt.condition, scope);
            try s.resolveStmt(if_stmt.then, scope);
            if (if_stmt.@"else") |else_block| {
                try s.resolveStmt(else_block, scope);
            }
        },
        .Compound => |compound| {
            var nested_scope = createNewScope(scope);
            try s.resolveBlock(compound.body, &nested_scope);
        },
        .Return => |return_stmt| s.resolveExpr(return_stmt.expr, scope),
        .Expr => |expr_stmt| s.resolveExpr(expr_stmt.expr, scope),
        .Case, .Default, .Break, .Continue, .Goto, .Null => {}, // noop
    };
}

fn resolveExpr(s: Self, expr: *Ast.Expr, scope: *ScopeIdents) SemaError!void {
    switch (expr.*) {
        .Prefix => |prefix_expr| {
            const inner_expr = recurseGetGroupInnerExpr(prefix_expr.expr);
            if (inner_expr.* != .Var) {
                try s.semaError(
                    SemaError.InvalidLValue,
                    prefix_expr.loc.line,
                    prefix_expr.loc.start,
                    "invalid lvalue for prefix: `{s}`\n",
                    .{@tagName(inner_expr.*)},
                );
            }
            try s.resolveExpr(inner_expr, scope);
        },
        .FnCall => |*fn_call| {
            const ident = scope.get(fn_call.ident) orelse {
                try s.semaError(
                    SemaError.UndeclaredVariable,
                    fn_call.loc.line,
                    fn_call.loc.start,
                    "undeclared function: {s}",
                    .{fn_call.ident},
                );
            };

            if (ident.type != .Fn) {
                try s.semaError(
                    SemaError.InvalidDeclaration,
                    fn_call.loc.line,
                    fn_call.loc.start,
                    "expected function, found: {s}",
                    .{ident.name},
                );
            }

            if (ident.from_current_scope) {
                fn_call.ident = ident.name;
            }

            for (fn_call.args.items) |arg| {
                try s.resolveExpr(arg, scope);
            }
        },
        .Ternary => |ternary_expr| {
            try s.resolveExpr(ternary_expr.condition, scope);
            try s.resolveExpr(ternary_expr.then, scope);
            try s.resolveExpr(ternary_expr.@"else", scope);
        },
        .Postfix => |postfix_expr| {
            const inner_expr = recurseGetGroupInnerExpr(postfix_expr.expr);
            if (inner_expr.* != .Var) {
                try s.semaError(
                    SemaError.InvalidLValue,
                    postfix_expr.loc.line,
                    postfix_expr.loc.start,
                    "invalid lvalue for postfix: `{s}`\n",
                    .{@tagName(inner_expr.*)},
                );
            }
            try s.resolveExpr(inner_expr, scope);
        },
        .Var => |*variable| {
            const ident = scope.get(variable.ident) orelse {
                try s.semaError(
                    SemaError.UndeclaredVariable,
                    variable.loc.line,
                    variable.loc.start,
                    "undeclared variable `{s}`\n",
                    .{variable.ident},
                );
            };
            variable.*.ident = ident.name;
        },
        .Unary => |unary| try s.resolveExpr(unary.expr, scope),
        .Binary => |binary| {
            try s.resolveExpr(binary.left, scope);
            try s.resolveExpr(binary.right, scope);
        },
        .Assignment => |assignment| {
            const dst = recurseGetGroupInnerExpr(assignment.dst);
            if (dst.* != .Var) {
                try s.semaError(
                    SemaError.InvalidLValue,
                    assignment.loc.line,
                    // to much work to get actual start, maybe should have been a normal c union type
                    // - 2 so it goes back two step from `=` symbol, maybe this is good enough
                    assignment.loc.start - 2,
                    "expected variable (invalid lvalue) found: `{s}`\n",
                    .{@tagName(dst.*)},
                );
            }
            try s.resolveExpr(dst, scope);
            try s.resolveExpr(assignment.src, scope);
        },
        .Group => |grp| try s.resolveExpr(grp.expr, scope),
        .Constant => {}, // noop
    }
}

fn recurseGetGroupInnerExpr(expr: *Ast.Expr) *Ast.Expr {
    if (expr.* != .Group) return expr;
    return recurseGetGroupInnerExpr(expr.Group.expr);
}

const ScopeIdents = StringHashMap(Ident);

const Ident = struct {
    from_current_scope: bool,
    type: IdentType,
    external_linkage: bool,
    name: []const u8,
    has_body: bool,
};

const IdentType = enum { Fn, Var };

fn createNewScope(scope: *ScopeIdents) ScopeIdents {
    const new_scope = scope.clone() catch unreachable;
    var it = new_scope.iterator();
    while (it.next()) |entry| {
        entry.value_ptr.*.from_current_scope = false;
    }
    return new_scope;
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
// const ArrayList = std.ArrayList;
const ArrayList = @import("../from_scratch.zig").ArrayList;
const ErrorReporter = @import("../ErrorReporter.zig");
const SemaError = @import("sema_common.zig").SemaError;
const SemaOptions = @import("sema_common.zig").SemaOptions;
const makeVar = @import("sema_common.zig").makeVar;
const Ast = @import("../AstParser.zig").Ast;
