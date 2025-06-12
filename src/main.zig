pub fn main() !void {
    const gpa, const deinit = getAllocator();
    defer _ = if (deinit) debug_allocator.deinit();

    try runCompiler(gpa);
}

fn runCompiler(allocator: Allocator) !void {
    const args: CliArgs = try .parse();
    log.debug("Args: {any}", .{args});

    const src = try preprocessor(allocator, args.src_path);
    defer allocator.free(src);

    log.debug("Src: \n{s}", .{src});
}

fn getAllocator() struct { Allocator, bool } {
    return switch (builtin.mode) {
        .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
        .ReleaseSmall, .ReleaseFast => .{ std.heap.smp_allocator, false },
    };
}

const std = @import("std");
const builtin = @import("builtin");
const CliArgs = @import("CliArgs.zig");
const compiler_driver = @import("compiler_driver.zig");
const preprocessor = compiler_driver.preprocessor;
const Allocator = std.mem.Allocator;
const log = std.log;

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
