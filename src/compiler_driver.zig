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

const std = @import("std");
const Allocator = std.mem.Allocator;
