arena: Allocator,
scratch_arena: Allocator,
error_reporter: *ErrorReporter,
random: std.Random,

const Self = @This();

const ScopeLabels = StringHashMap(Label);
const Label = struct {
    owing_block: bool,
    processed_label: []const u8,
    loc: struct { line: usize, start: usize },
};

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
    try s.resolveFnDecl(pg.@"fn");
}

fn resolveFnDecl(s: Self, fn_decl: *Ast.FnDecl) SemaError!void {
    var scope_labels = ScopeLabels.init(s.arena);

    // find all labels
    // doing this so we can make sure goto is pointing to correct label
    try s.scanLabelOnBlock(fn_decl.body, &scope_labels);

    var goto_labels = ScopeLabels.init(s.arena);
    // resolve all goto labels
    try s.resolveGotoOnBlock(fn_decl.body, &goto_labels, &scope_labels);

    // any ghost labels
    var goto_iter = goto_labels.iterator();
    while (goto_iter.next()) |goto_label| {
        if (!scope_labels.contains(goto_label.key_ptr.*)) {
            try s.semaError(
                SemaError.UndeclaredLabel,
                goto_label.value_ptr.*.loc.line,
                goto_label.value_ptr.*.loc.start,
                "undeclared label `{s}`",
                .{goto_label.key_ptr.*},
            );
        }
    }
}
fn scanLabelOnBlock(s: Self, block: *Ast.Block, scope_labels: *ScopeLabels) SemaError!void {
    var items = block.block_item;
    try s.scanLabelOnBlockItem(&items, scope_labels);
}

fn scanLabelOnBlockItem(s: Self, block_items: *ArrayList(*Ast.BlockItem), scope_labels: *ScopeLabels) SemaError!void {
    for (block_items.items) |block_item| {
        switch (block_item.*) {
            .Stmt => |stmt| {
                try s.scanLabelOnStmt(stmt, scope_labels);
            },
            .Decl => {}, // noop
        }
    }
}

fn scanLabelOnStmt(s: Self, stmt: *Ast.Stmt, scope_labels: *ScopeLabels) SemaError!void {
    switch (stmt.*) {
        .DoWhile => |do_while| try s.scanLabelOnStmt(do_while.body, scope_labels),
        .While => |wh| try s.scanLabelOnStmt(wh.body, scope_labels),
        .For => |for_stmt| try s.scanLabelOnStmt(for_stmt.body, scope_labels),
        .Label => |*label_stmt| {
            if (scope_labels.contains(label_stmt.ident)) {
                try s.semaError(
                    SemaError.DuplicateLabel,
                    label_stmt.loc.line,
                    label_stmt.loc.start,
                    "duplicate label `{s}`",
                    .{label_stmt.ident},
                );
            }
            const new_label = makeGotoLabel(s.arena, s.random, label_stmt.ident);
            const label: Label = .{
                .owing_block = true,
                .processed_label = new_label,
                .loc = .{ .line = label_stmt.loc.line, .start = label_stmt.loc.start },
            };
            scope_labels.put(label_stmt.ident, label) catch unreachable;
            label_stmt.ident = new_label;
            try s.scanLabelOnStmt(label_stmt.stmt, scope_labels);
        },
        .Return => |return_stmt| try s.scanLabelOnExpr(return_stmt.expr, scope_labels),
        .Expr => |expr_stmt| try s.scanLabelOnExpr(expr_stmt.expr, scope_labels),
        .If => |if_stmt| {
            try s.scanLabelOnExpr(if_stmt.condition, scope_labels);
            try s.scanLabelOnStmt(if_stmt.then, scope_labels);
            if (if_stmt.@"else") |else_stmt| {
                try s.scanLabelOnStmt(else_stmt, scope_labels);
            }
        },
        .Compound => |compound_stmt| {
            try s.scanLabelOnBlock(compound_stmt.body, scope_labels);
        },
        .Break, .Continue, .Goto, .Null => {}, // noop
    }
}

fn scanLabelOnExpr(s: Self, expr: *Ast.Expr, scope_labels: *ScopeLabels) SemaError!void {
    switch (expr.*) {
        .Unary => |unary| {
            try s.scanLabelOnExpr(unary.expr, scope_labels);
        },
        .Binary => |binary| {
            try s.scanLabelOnExpr(binary.left, scope_labels);
            try s.scanLabelOnExpr(binary.right, scope_labels);
        },
        .Assignment => |assignment| {
            try s.scanLabelOnExpr(assignment.src, scope_labels);
            try s.scanLabelOnExpr(assignment.dst, scope_labels);
        },
        .Group => |grp| {
            try s.scanLabelOnExpr(grp.expr, scope_labels);
        },
        .Postfix => |postfix| {
            try s.scanLabelOnExpr(postfix.expr, scope_labels);
        },
        .Ternary => |ternary| {
            try s.scanLabelOnExpr(ternary.condition, scope_labels);
            try s.scanLabelOnExpr(ternary.then, scope_labels);
            try s.scanLabelOnExpr(ternary.@"else", scope_labels);
        },
        .Constant, .Var => {}, // noop
    }
}

fn resolveGotoOnBlock(s: Self, block: *Ast.Block, goto_labels: *ScopeLabels, scope_labels: *ScopeLabels) SemaError!void {
    for (block.block_item.items) |item| {
        switch (item.*) {
            .Stmt => |stmt| {
                try s.resolveGotoOnStmt(stmt, goto_labels, scope_labels);
            },
            .Decl => {}, // noop
        }
    }
}

fn resolveGotoOnStmt(s: Self, stmt: *Ast.Stmt, goto_labels: *ScopeLabels, scope_labels: *ScopeLabels) SemaError!void {
    switch (stmt.*) {
        .DoWhile => |do_while| try s.resolveGotoOnStmt(do_while.body, goto_labels, scope_labels),
        .While => |wh| try s.resolveGotoOnStmt(wh.body, goto_labels, scope_labels),
        .For => |for_stmt| try s.resolveGotoOnStmt(for_stmt.body, goto_labels, scope_labels),
        .Goto => |*goto_stmt| {
            const label = scope_labels.get(goto_stmt.ident) orelse {
                try s.semaError(
                    SemaError.UndeclaredLabel,
                    goto_stmt.loc.line,
                    goto_stmt.loc.start,
                    "undeclared label `{s}`",
                    .{goto_stmt.ident},
                );
            };
            goto_labels.put(goto_stmt.ident, label) catch unreachable;
            goto_stmt.ident = label.processed_label;
        },
        .Return => |return_stmt| try s.resolveGotoOnExpr(return_stmt.expr, goto_labels, scope_labels),
        .Expr => |expr_stmt| try s.resolveGotoOnExpr(expr_stmt.expr, goto_labels, scope_labels),
        .If => |if_stmt| {
            try s.resolveGotoOnExpr(if_stmt.condition, goto_labels, scope_labels);
            try s.resolveGotoOnStmt(if_stmt.then, goto_labels, scope_labels);
            if (if_stmt.@"else") |else_stmt| {
                try s.resolveGotoOnStmt(else_stmt, goto_labels, scope_labels);
            }
        },
        .Compound => |compound_stmt| {
            try s.resolveGotoOnBlock(compound_stmt.body, goto_labels, scope_labels);
        },
        .Label => |label_stmt| try s.resolveGotoOnStmt(label_stmt.stmt, goto_labels, scope_labels),
        .Break, .Continue, .Null => {}, // noop
    }
}
fn resolveGotoOnExpr(s: Self, expr: *Ast.Expr, goto_labels: *ScopeLabels, scope_labels: *ScopeLabels) SemaError!void {
    switch (expr.*) {
        .Unary => |unary| {
            try s.resolveGotoOnExpr(unary.expr, goto_labels, scope_labels);
        },
        .Binary => |binary| {
            try s.resolveGotoOnExpr(binary.left, goto_labels, scope_labels);
            try s.resolveGotoOnExpr(binary.right, goto_labels, scope_labels);
        },
        .Assignment => |assignment| {
            try s.resolveGotoOnExpr(assignment.src, goto_labels, scope_labels);
            try s.resolveGotoOnExpr(assignment.dst, goto_labels, scope_labels);
        },
        .Group => |grp| {
            try s.resolveGotoOnExpr(grp.expr, goto_labels, scope_labels);
        },
        .Postfix => |postfix| {
            try s.resolveGotoOnExpr(postfix.expr, goto_labels, scope_labels);
        },

        .Ternary => |ternary| {
            try s.resolveGotoOnExpr(ternary.condition, goto_labels, scope_labels);
            try s.resolveGotoOnExpr(ternary.then, goto_labels, scope_labels);
            try s.resolveGotoOnExpr(ternary.@"else", goto_labels, scope_labels);
        },
        .Constant, .Var => {}, // noop
    }
}

fn semaError(s: Self, e: SemaError, line: usize, start: usize, comptime fmt: []const u8, args: anytype) SemaError!noreturn {
    s.error_reporter.addError(line, start, fmt, args);
    return e;
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const sema_common = @import("sema_common.zig");
const SemaError = sema_common.SemaError;
const SemaOptions = sema_common.SemaOptions;
const makeGotoLabel = sema_common.makeGotoLabel;
const ErrorReporter = @import("../ErrorReporter.zig");
const AstParser = @import("../AstParser.zig");
const Ast = AstParser.Ast;
