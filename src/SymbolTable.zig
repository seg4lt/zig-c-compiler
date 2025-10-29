inner: StringHashMap(Symbol),
arena: Allocator,

const Self = @This();

pub fn init(arena: Allocator) Self {
    return .{
        .inner = StringHashMap(Symbol).init(arena),
        .arena = arena,
    };
}

pub fn iterator(self: Self) StringHashMap(Symbol).Iterator {
    return self.inner.iterator();
}

pub fn contains(self: Self, ident: []const u8) bool {
    return self.inner.contains(ident);
}

pub fn get(self: Self, ident: []const u8) ?Symbol {
    return self.inner.get(ident);
}

pub fn put(self: *Self, ident: []const u8, symbol: Symbol) void {
    const owned_ident = self.arena.dupe(u8, ident) catch unreachable;
    self.inner.put(owned_ident, symbol) catch unreachable;
}

pub const Symbol = union(enum) {
    Var: VarSymbol,
    Fn: FnSymbol,

    pub fn staticVarSymbol(allocator: Allocator, ident: []const u8, global: bool, initial_value: InitialValue) Symbol {
        const owned_ident = allocator.dupe(u8, ident) catch unreachable;
        return .{
            .Var = .{
                .Static = .{
                    .ident = owned_ident,
                    .global = global,
                    .initial_value = initial_value,
                },
            },
        };
    }
    pub fn localVarSymbol(allocator: Allocator, ident: []const u8) Symbol {
        const owned_ident = allocator.dupe(u8, ident) catch unreachable;
        return .{
            .Var = .{
                .Local = .{
                    .ident = owned_ident,
                },
            },
        };
    }

    pub fn fnSymbol(
        arena: Allocator,
        ident: []const u8,
        params: ArrayList(FnSymbol.Param),
        global: bool,
        defined: bool,
    ) Symbol {
        const owned_ident = arena.dupe(u8, ident) catch unreachable;
        return .{
            .Fn = .{
                .ident = owned_ident,
                .params = params,
                .global = global,
                .defined = defined,
            },
        };
    }

    pub const VarSymbol = union(enum) {
        // this static is not in terms of C, but rather ASM
        // static means we store this value in .data section of ASM file
        Static: struct { ident: []const u8, global: bool, initial_value: InitialValue },
        Local: struct { ident: []const u8 },
    };

    pub const FnSymbol = struct {
        ident: []const u8,
        params: ArrayList(Param),

        defined: bool,
        global: bool,

        pub const Param = struct {
            ident: []const u8,
            type: []const u8,

            pub fn fnParam(arena: Allocator, ident: []const u8, typez: []const u8) @This() {
                const owned_ident = arena.dupe(u8, ident) catch unreachable;
                const owned_type = arena.dupe(u8, typez) catch unreachable;
                return .{ .ident = owned_ident, .type = owned_type };
            }
        };
    };

    pub const InitialValue = union(enum) {
        tentative,
        initial: i32,
        no_initializer,

        pub fn initialValue(value: i32) @This() {
            return .{ .initial = value };
        }
    };
};

const std = @import("std");
const Allocator = std.mem.Allocator;
// const ArrayList = std.ArrayList;
const ArrayList = @import("from_scratch.zig").ArrayList;
const StringHashMap = std.StringHashMap;
