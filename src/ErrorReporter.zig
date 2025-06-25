error_items: std.ArrayList(ErrorItem),
src: []const u8,
src_path: []const u8,
writer: AnyWriter,
allocator: Allocator,

const Self = @This();

pub const ErrorItem = struct {
    msg: []const u8,
    file: []const u8,
    line: usize,
    column: usize,
};

pub fn init(allocator: Allocator, src: []const u8, src_path: []const u8) Self {
    return .initWithWriter(allocator, src, src_path, std.io.getStdErr().writer().any());
}

pub fn initWithWriter(allocator: Allocator, src: []const u8, src_path: []const u8, writer: AnyWriter) Self {
    return .{
        .error_items = std.ArrayList(ErrorItem).init(allocator),
        .src = src,
        .src_path = src_path,
        .writer = writer,
        .allocator = allocator,
    };
}

pub fn deinit(s: *Self) void {
    for (s.error_items.items) |it| s.allocator.free(it.msg);
    s.error_items.deinit();
}

pub fn printError(s: *const Self) void {
    if (s.error_items.items.len == 0) return;
    for (s.error_items.items) |it| s.writer.print("{s}", .{it.msg}) catch unreachable;
}

pub fn addError(s: *Self, line: usize, start: usize, comptime msg_fmt: []const u8, args: anytype) void {
    const item = getErrorItem(s.allocator, s.src, s.src_path, line, start, msg_fmt, args);
    s.error_items.append(item) catch unreachable;
}

fn getErrorItem(
    allocator: Allocator,
    src: []const u8,
    src_path: []const u8,
    line: usize,
    start: usize,
    comptime msg_fmt: []const u8,
    args: anytype,
) ErrorItem {
    const pls, const cls = findLineStart(src, start);
    var cle = start;
    for (start..src.len) |it| {
        if (src[it] == '\n') {
            cle = it;
            break;
        }
    }
    const column = start - cls;
    var sb = std.ArrayList(u8).init(allocator);
    defer sb.deinit();

    const error_in = std.fmt.allocPrint(
        allocator,
        "Error in {s}:{d},{d}\n",
        .{ src_path, line, column },
    ) catch unreachable;
    defer allocator.free(error_in);
    sb.appendSlice(error_in) catch unreachable;

    const support_line = std.fmt.allocPrint(allocator, "{s}", .{src[pls..cls]}) catch unreachable;
    defer allocator.free(support_line);
    sb.appendSlice(support_line) catch unreachable;

    const error_line = std.fmt.allocPrint(allocator, "{s}\n", .{src[cls..cle]}) catch unreachable;
    defer allocator.free(error_line);
    sb.appendSlice(error_line) catch unreachable;

    const padding = allocator.alloc(u8, column) catch unreachable;
    defer allocator.free(padding);
    @memset(padding, ' ');

    const detail_msg = std.fmt.allocPrint(allocator, msg_fmt, args) catch unreachable;
    defer allocator.free(detail_msg);

    const final_msg = std.fmt.allocPrint(allocator, "{s}^____ {s}", .{ padding, detail_msg }) catch unreachable;
    defer allocator.free(final_msg);

    sb.appendSlice(final_msg) catch unreachable;

    const full_msg = sb.toOwnedSlice() catch unreachable;
    return .{ .msg = full_msg, .file = src_path, .line = line, .column = column };
}

fn findLineStart(src: []const u8, start: usize) struct { usize, usize } {
    var prev_line_start: usize = 0;
    var cur_line_start: usize = 0;

    var cur = start;
    var count: i32 = 0;

    while (cur >= 0 and count <= 2) {
        if (src[cur] == '\n') {
            if (count == 0) cur_line_start = cur + 1;
            count += 1;
            prev_line_start = cur + 1;
        }
        if (cur == 0) break;
        cur -= 1;
    }
    return .{ prev_line_start, cur_line_start };
}

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;
