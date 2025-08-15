pub fn ArrayList(comptime T: type) type {
    return struct {
        items: []T,
        capacity: usize,
        allocator: Allocator,

        const Self = @This();

        pub const Writer = std.io.Writer(*Self, Allocator.Error, appendWrite);

        pub fn init(allocator: Allocator) Self {
            const INITIAL_CAPACITY = 8;
            const items = allocator.alloc(T, INITIAL_CAPACITY) catch unreachable;
            return .{
                .items = items[0..0],
                .capacity = INITIAL_CAPACITY,
                .allocator = allocator,
            };
        }
        pub fn deinit(self: *Self) void {
            self.allocator.free(self.items);
            self.capacity = 0;
        }
        pub fn append(self: *Self, item: T) void {
            self.items.len += 1;
            const ptr = &self.items[self.items.len - 1];
            ptr.* = item;
            self.ensureCapacity();
        }

        pub fn pop(self: *Self) ?T {
            if (self.items.len == 0) return null;
            const item = self.items[self.items.len - 1];
            self.items.len -= 1;
            return item;
        }
        pub fn getLast(self: *Self) T {
            // Risky?? as items can be empty?
            return self.items[self.items.len - 1];
        }

        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }

        // Really wanted to ignore memory errors, but Writer inteface requires method that returns this error. Ahhhhhh......
        fn appendWrite(self: *Self, m: []const T) error{OutOfMemory}!usize {
            self.appendSlice(m);
            return m.len;
        }
        pub fn appendSlice(self: *Self, items: []const T) void {
            for (items) |item| self.append(item);
        }

        pub fn toOwnedSlice(self: *Self) []T {
            if (self.items.len == 0) return &.{};
            const new_items = self.allocator.alloc(T, self.items.len) catch unreachable;
            @memcpy(new_items, self.items);
            return new_items;
        }

        fn ensureCapacity(self: *Self) void {
            if (self.items.len < self.capacity) return;
            const old_len = self.items.len;
            const new_capacity = self.capacity * 2;
            self.items = self.allocator.realloc(self.items, new_capacity) catch unreachable;
            self.items = self.items[0..old_len];
            self.capacity = new_capacity;
        }
    };
}

const std = @import("std");
const Allocator = std.mem.Allocator;
