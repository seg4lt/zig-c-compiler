pub fn preprocessor(allocator: Allocator, src_path: []const u8) ![]u8 {
    const src_path_no_ext = if (std.mem.endsWith(u8, src_path, ".c"))
        src_path[0 .. src_path.len - 2]
    else
        src_path;

    const preprocessed_path = std.fmt.allocPrint(allocator, "{s}.i", .{src_path_no_ext}) catch unreachable;
    defer allocator.free(preprocessed_path);

    var child = std.process.Child.init(&[_][]const u8{ "gcc", "-E", "-P", src_path, "-o", preprocessed_path }, allocator);
    const result = child.spawnAndWait() catch unreachable;
    std.debug.assert(result == .Exited);

    return try std.fs.cwd().readFileAlloc(allocator, preprocessed_path, 4096);
}

const OutputType = enum {
    exe,
    obj,
};

pub fn assembleAndLink(arena: Allocator, src_path_no_ext: []const u8, output_type: OutputType) void {
    const asm_file = std.fmt.allocPrint(arena, "{s}.s", .{src_path_no_ext}) catch unreachable;
    var cmd = std.ArrayList([]const u8).init(arena);
    cmd.append("gcc") catch unreachable;

    switch (output_type) {
        .exe => {
            cmd.append(asm_file) catch unreachable;
            cmd.append("-o") catch unreachable;
            cmd.append(src_path_no_ext) catch unreachable;
        },
        .obj => {
            cmd.append("-c") catch unreachable;
            cmd.append(asm_file) catch unreachable;
            cmd.append("-o") catch unreachable;
            cmd.append(std.fmt.allocPrint(arena, "{s}.o", .{src_path_no_ext}) catch unreachable) catch unreachable;
        },
    }
    var child = std.process.Child.init(cmd.items, arena);
    const result = child.spawnAndWait() catch unreachable;
    std.debug.assert(result == .Exited);
}

const std = @import("std");
const Allocator = std.mem.Allocator;
