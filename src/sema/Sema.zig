pub fn sema(opt: SemaOptions) SemaError!void {
    try SemaIdentResolution.resolve(opt);
    if (opt.print_ast) print(opt, "Sema Phase: Ident Resolution");

    try SemaGotoResolution.resolve(opt);
    if (opt.print_ast) print(opt, "Sema Phase: Goto Resolution");

    try SemaLoopLabeling.label(opt);
    if (opt.print_ast) print(opt, "Sema Phase: Loop Labeling");
}

fn print(opt: SemaOptions, label: []const u8) void {
    var buffer = ArrayList(u8).init(opt.arena);
    var stdErr = std.io.getStdErr().writer();
    AstPrinter.print(buffer.writer().any(), opt.program);
    stdErr.print("-- {s} --\n", .{label}) catch unreachable;
    // all of this to check if I have extra space.. Ouch...
    for (buffer.items) |c| {
        _ = (if (c == ' ') stdErr.write("﹍") else stdErr.write(&[_]u8{c})) catch unreachable;
    }
}

const std = @import("std");
const ArrayList = std.ArrayList;
const SemaError = @import("sema_common.zig").SemaError;
const SemaOptions = @import("sema_common.zig").SemaOptions;
const AstPrinter = @import("../AstParser.zig").AstPrinter;
const CompilerError = @import("../util.zig").CompilerError;
const Allocator = std.mem.Allocator;
const SemaIdentResolution = @import("SemaIdentResolution.zig");
const SemaGotoResolution = @import("SemaGotoResolution.zig");
const SemaLoopLabeling = @import("SemaLoopLabeling.zig");
