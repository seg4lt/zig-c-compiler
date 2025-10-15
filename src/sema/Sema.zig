pub fn sema(opt: SemaOptions) SemaError!void {
    try SemaIdentResolution.resolve(opt);
    if (opt.print_ast) print(opt, "Sema Phase: Ident Resolution");

    try SemaGotoResolution.resolve(opt);
    if (opt.print_ast) print(opt, "Sema Phase: Goto Resolution");

    try SemaLoopLabeling.label(opt);
    if (opt.print_ast) print(opt, "Sema Phase: Loop Labeling");

    try SemaTypeChecking.check(opt);
    if (opt.print_ast) print(opt, "Sema Phase: Type Checking");
}

fn print(opt: SemaOptions, label: []const u8) void {
    var printer = Printer.init(opt.arena);
    const writer = printer.writer();
    writer.print("-- {s} --\n", .{label}) catch unreachable;
    AstPrinter.print(writer, opt.program);
    printer.printToStdErr(.{ .show_whitespace = true }) catch unreachable;
}

const std = @import("std");
// const ArrayList = std.ArrayList;
const ArrayList = @import("../from_scratch.zig").ArrayList;
const SemaError = @import("sema_common.zig").SemaError;
const SemaOptions = @import("sema_common.zig").SemaOptions;
const AstPrinter = @import("../AstParser.zig").AstPrinter;
const CompilerError = @import("../util.zig").CompilerError;
const Allocator = std.mem.Allocator;
const SemaIdentResolution = @import("SemaIdentResolution.zig");
const SemaGotoResolution = @import("SemaGotoResolution.zig");
const SemaLoopLabeling = @import("SemaLoopLabeling.zig");
const SemaTypeChecking = @import("SemaTypeChecking.zig");
const Printer = @import("../util.zig").Printer;
