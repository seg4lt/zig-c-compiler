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
    case_labels: *ArrayList(Ast.CaseLabel),
};

const Label = union(enum) {
    Loop: *LoopLabel,
    Switch: *SwitchLabel,

    pub fn loopLabel(allocator: Allocator, label_name: []const u8) *Label {
        const loop_label = allocator.create(LoopLabel) catch unreachable;
        loop_label.* = .{ .label = label_name };
        const label_ = allocator.create(Label) catch unreachable;
        label_.* = .{ .Loop = loop_label };
        return label_;
    }
    pub fn switchLabel(allocator: Allocator, label_name: []const u8, case_labels: *ArrayList(Ast.CaseLabel)) *Label {
        const switch_label = allocator.create(SwitchLabel) catch unreachable;
        switch_label.* = .{ .label = label_name, .case_labels = case_labels };
        const label_ = allocator.create(Label) catch unreachable;
        label_.* = .{ .Switch = switch_label };
        return label_;
    }
};
const Labels = ArrayList(*Label);

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
    for (pg.fns.items) |it| {
        try s.labelFnDecl(it);
    }
}

fn labelFnDecl(s: Self, fn_decl: *Ast.FnDecl) SemaError!void {
    var labels = Labels.init(s.arena);
    if (fn_decl.body) |body| try s.labelBlock(body, &labels);
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
        .Switch => |*switch_stmt| {
            const switch_label = makeLabel(s.arena, s.random, "switch");
            switch_stmt.label = switch_label;

            const case_labels = s.arena.create(ArrayList(Ast.CaseLabel)) catch unreachable;
            case_labels.* = ArrayList(Ast.CaseLabel).init(s.arena);

            labels.append(.switchLabel(s.arena, switch_label, case_labels)) catch unreachable;

            var body = switch_stmt.body;
            try s.labelBlockItem(&body, labels);

            const switch_labels = labels.pop() orelse {
                std.debug.panic("** Compiler Bug ** no label info found", .{});
            };
            if (switch_labels.* != .Switch) {
                std.debug.panic("** Compiler Bug ** expected switch label info", .{});
            }
            // saving references to all cases switch has
            // Need this on tacky gen phase to know what cases we have so we can jump to right location
            switch_stmt.case_labels = switch_labels.Switch.case_labels;
        },
        .Case => |*case_stmt| {
            const latest_switch_label = getClosestSwitchLabel(labels) orelse {
                try s.semaError(
                    SemaError.InvalidPlacement,
                    case_stmt.loc.line,
                    case_stmt.loc.start,
                    "case statement outside of switch",
                    .{},
                );
            };
            if (hasSameCase(latest_switch_label.case_labels, case_stmt.value)) {
                try s.semaError(
                    SemaError.DuplicateCase,
                    case_stmt.loc.line,
                    case_stmt.loc.start,
                    "case already defined `{s}`",
                    .{case_stmt.value},
                );
            }

            const case_label_ident = makeLabel(s.arena, s.random, "case");
            const case_label: Ast.CaseLabel = .{ .label = case_label_ident, .value = case_stmt.value, .is_default = false };
            latest_switch_label.case_labels.append(case_label) catch unreachable;

            case_stmt.label = case_label_ident;
        },
        .Default => |*default_stmt| {
            const latest_switch_label = getClosestSwitchLabel(labels) orelse {
                try s.semaError(
                    SemaError.InvalidPlacement,
                    default_stmt.loc.line,
                    default_stmt.loc.start,
                    "case statement outside of switch",
                    .{},
                );
            };

            if (hasDefaultCase(latest_switch_label.case_labels)) {
                try s.semaError(
                    SemaError.DuplicateDefaultCase,
                    default_stmt.loc.line,
                    default_stmt.loc.start,
                    "default case already defined",
                    .{},
                );
            }

            const default_case_label_ident = makeLabel(s.arena, s.random, "default");
            default_stmt.label = default_case_label_ident;

            const default_case: Ast.CaseLabel = .{ .label = default_case_label_ident, .value = "", .is_default = true };
            latest_switch_label.case_labels.append(default_case) catch unreachable;
        },
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
            labels.append(.loopLabel(s.arena, new_label)) catch unreachable;
            try s.labelStmt(do_stmt.body, labels);
            _ = labels.pop();
        },
        .While => |*while_stmt| {
            const new_label = makeLabel(s.arena, s.random, "while");
            while_stmt.label = new_label;
            labels.append(.loopLabel(s.arena, new_label)) catch unreachable;
            try s.labelStmt(while_stmt.body, labels);
            _ = labels.pop();
        },
        .For => |*for_stmt| {
            const new_label = makeLabel(s.arena, s.random, "for");
            for_stmt.label = new_label;
            labels.append(.loopLabel(s.arena, new_label)) catch unreachable;
            try s.labelStmt(for_stmt.body, labels);
            _ = labels.pop();
        },
        .Null, .Goto => {}, // noop
    }
}
fn labelExpr(s: Self, expr: *Ast.Expr, labels: *Labels) SemaError!void {
    switch (expr.*) {
        .FnCall => |fn_call| {
            for (fn_call.args.items) |arg| {
                try s.labelExpr(arg, labels);
            }
        },
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
    return switch (last_label.*) {
        .Loop => |loop| loop.label,
        .Switch => |s| s.label,
    };
}

fn getClosestLoopLabel(labels: *Labels) ?[]const u8 {
    var rev = std.mem.reverseIterator(labels.items);
    while (rev.next()) |label_info| {
        switch (label_info.*) {
            .Loop => |loop| return loop.label,
            .Switch => {}, // noop
        }
    }
    return null;
}
fn getClosestSwitchLabel(labels: *Labels) ?*SwitchLabel {
    var i = labels.items.len - 1;
    while (i >= 0) {
        const label_item = labels.items[i];
        switch (label_item.*) {
            .Switch => |switch_label| return switch_label,
            .Loop => {}, // noop
        }
        if (i == 0) break;
        i -= 1;
    }
    return null;
}

fn hasSameCase(case_labels: *ArrayList(Ast.CaseLabel), value: []const u8) bool {
    for (case_labels.items) |it| {
        if (std.mem.eql(u8, it.value, value)) {
            return true;
        }
    }
    return false;
}

fn hasDefaultCase(case_labels: *ArrayList(Ast.CaseLabel)) bool {
    for (case_labels.items) |it| {
        if (it.is_default) {
            return true;
        }
    }
    return false;
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
