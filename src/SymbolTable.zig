inner: StringHashMap(Symbol),
arena: Allocator,

const Self = @This();

pub fn init(arena: Allocator) Self {
    return .{
        .inner = StringHashMap(Symbol).init(arena),
        .arena = arena,
    };
}

pub fn get(self: Self, ident: []const u8) ?Symbol {
    return self.inner.get(ident);
}

pub fn put(self: *Self, ident: []const u8, symbol: Symbol) !void {
    const owned_ident = self.arena.dupe(u8, ident) catch unreachable;
    try self.inner.put(owned_ident, symbol);
}

pub const Symbol = union(enum) {
    Int: []const u8,
    Fn: Fn,

    pub fn intSymbol(arena: Allocator, ident: []const u8) Symbol {
        const owned_ident = arena.dupe(u8, ident) catch unreachable;
        return .{ .Int = owned_ident };
    }

    pub fn fnSymbol(arena: Allocator, ident: []const u8, params: ArrayList(FnParam), defined: bool) Symbol {
        const owned_ident = arena.dupe(u8, ident) catch unreachable;
        return .{
            .Fn = .{
                .ident = owned_ident,
                .params = params,
                .defined = defined,
            },
        };
    }
};

pub const Fn = struct {
    ident: []const u8,
    params: ArrayList(FnParam),
    defined: bool,
};

pub const FnParam = struct {
    ident: []const u8,
    type: []const u8,

    pub fn fnParam(arena: Allocator, ident: []const u8, typez: []const u8) FnParam {
        const owned_ident = arena.dupe(u8, ident) catch unreachable;
        const owned_type = arena.dupe(u8, typez) catch unreachable;
        return .{ .ident = owned_ident, .type = owned_type };
    }
};

const std = @import("std");
const Allocator = std.mem.Allocator;
// const ArrayList = std.ArrayList;
const ArrayList = @import("from_scratch.zig").ArrayList;
const StringHashMap = std.StringHashMap;
