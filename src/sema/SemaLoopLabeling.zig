arena: Allocator,
scratch_arena: Allocator,
error_reporter: *ErrorReporter,
random: std.Random,

const Self = @This();

const LoopLabel = struct {
    label: []const u8,
};

const SwitchLabel = struct {
    label: []const u8,
    case_labels: ArrayList(Ast.CaseLabel),
};

const Label = union(enum) {
    Loop: LoopLabel,
    Switch: SwitchLabel,
};
const Labels = ArrayList(Label);

pub fn label(opt: SemaOptions) SemaError!void {
    var self = Self{
        .arena = opt.arena,
        .scratch_arena = opt.scratch_arena,
        .error_reporter = opt.error_reporter,
        .random = opt.random,
    };

    self.labelPg(opt.program) catch |e| {
        self.error_reporter.printError(std.io.getStdErr().writer().any());
        return e;
    };
}

fn labelPg(s: Self, pg: *Ast.Program) SemaError!void {
    try s.labelFnDecl(pg.@"fn");
}

fn labelFnDecl(s: Self, fn_decl: *Ast.FnDecl) SemaError!void {
    var labels = Labels.init(s.arena);
    try s.labelBlock(fn_decl.body, &labels);
}

fn labelBlock(s: Self, block: *Ast.Block, labels: *Labels) SemaError!void {
    var items = block.block_item;
    try s.labelBlockItem(&items, labels);
}
fn labelBlockItem(s: Self, block_items: *ArrayList(*Ast.BlockItem), labels: *Labels) SemaError!void {
    for (block_items.items) |block_item| {
        switch (block_item.*) {
            .Stmt => |stmt| try s.labelStmt(stmt, labels),
            .Decl => {}, // noop
        }
    }
}

fn labelStmt(s: Self, stmt: *Ast.Stmt, labels: *Labels) SemaError!void {
    switch (stmt.*) {
        .Return => |return_stmt| try s.labelExpr(return_stmt.expr, labels),
        .Expr => |expr_stmt| try s.labelExpr(expr_stmt.expr, labels),
        .If => |if_stmt| {
            try s.labelExpr(if_stmt.condition, labels);
            try s.labelStmt(if_stmt.then, labels);
            if (if_stmt.@"else") |else_stmt| try s.labelStmt(else_stmt, labels);
        },
        .Compound => |compound_stmt| try s.labelBlock(compound_stmt.body, labels),
        .Label => |label_stmt| try s.labelStmt(label_stmt.stmt, labels),
        .Break => |*break_stmt| {
            if (labels.items.len <= 0) {
                try s.semaError(
                    SemaError.InvalidPlacement,
                    break_stmt.loc.line,
                    break_stmt.loc.start,
                    "You need to be in loop or switch to use break",
                    .{},
                );
            }
            break_stmt.ident = getAnyClosestLabel(labels);
        },
        .Continue => |*continue_stmt| {
            if (labels.items.len <= 0) {
                try s.semaError(
                    SemaError.InvalidPlacement,
                    continue_stmt.loc.line,
                    continue_stmt.loc.start,
                    "You need to be in loop to use continue",
                    .{},
                );
            }
            continue_stmt.ident = getClosestLoopLabel(labels) orelse {
                try s.semaError(
                    SemaError.InvalidPlacement,
                    continue_stmt.loc.line,
                    continue_stmt.loc.start,
                    "You need to be in loop to use continue",
                    .{},
                );
            };
        },
        .DoWhile => |*do_stmt| {
            const new_label = makeLabel(s.arena, s.random, "dowhile");
            do_stmt.label = new_label;
            labels.append(.{ .Loop = .{ .label = new_label } }) catch unreachable;
            try s.labelStmt(do_stmt.body, labels);
            _ = labels.pop();
        },
        .While => |*while_stmt| {
            const new_label = makeLabel(s.arena, s.random, "while");
            while_stmt.label = new_label;
            labels.append(.{ .Loop = .{ .label = new_label } }) catch unreachable;
            try s.labelStmt(while_stmt.body, labels);
            _ = labels.pop();
        },
        .For => |*for_stmt| {
            const new_label = makeLabel(s.arena, s.random, "for");
            for_stmt.label = new_label;
            labels.append(.{ .Loop = .{ .label = new_label } }) catch unreachable;
            try s.labelStmt(for_stmt.body, labels);
            _ = labels.pop();
        },
        .Null, .Goto => {}, // noop
    }
}
fn labelExpr(s: Self, expr: *Ast.Expr, labels: *Labels) SemaError!void {
    switch (expr.*) {
        .Ternary => |ternary_expr| {
            try s.labelExpr(ternary_expr.condition, labels);
            try s.labelExpr(ternary_expr.then, labels);
            try s.labelExpr(ternary_expr.@"else", labels);
        },
        .Group => |grp| try s.labelExpr(grp.expr, labels),
        .Unary => |u| try s.labelExpr(u.expr, labels),
        .Binary => |binary| {
            try s.labelExpr(binary.left, labels);
            try s.labelExpr(binary.right, labels);
        },
        .Assignment => |assignment| {
            try s.labelExpr(assignment.src, labels);
            try s.labelExpr(assignment.dst, labels);
        },
        .Postfix => |postfix| try s.labelExpr(postfix.expr, labels),
        .Constant, .Var => {}, // noop
    }
}

fn semaError(p: Self, e: SemaError, line: usize, start: usize, comptime fmt: []const u8, args: anytype) SemaError!noreturn {
    p.error_reporter.addError(line, start, fmt, args);
    return e;
}

fn getAnyClosestLabel(labels: *Labels) []const u8 {
    if (labels.items.len == 0) std.debug.panic("** Compiler Bug ** no label info found", .{});
    const last_label = labels.getLast();
    return switch (last_label) {
        .Loop => |loop| loop.label,
        .Switch => |s| s.label,
    };
}

fn getClosestLoopLabel(labels: *Labels) ?[]const u8 {
    var rev = std.mem.reverseIterator(labels.items);
    while (rev.next()) |label_info| {
        switch (label_info) {
            .Loop => |loop| return loop.label,
            .Switch => {}, // noop
        }
    }
    return null;
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const ArrayList = std.ArrayList;
const AstParser = @import("../AstParser.zig");
const Ast = AstParser.Ast;
const sema_common = @import("sema_common.zig");
const SemaError = sema_common.SemaError;
const SemaOptions = sema_common.SemaOptions;
const makeLabel = sema_common.makeLabel;
const makeVar = sema_common.makeVar;
const ErrorReporter = @import("../ErrorReporter.zig");
