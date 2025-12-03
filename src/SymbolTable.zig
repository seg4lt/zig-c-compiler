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

    pub fn staticVarSymbol(allocator: Allocator, ident: []const u8, global: bool, initial_value: InitialValue, typez: *Ast.BuiltinType) Symbol {
        const owned_ident = allocator.dupe(u8, ident) catch unreachable;
        return .{
            .Var = .{
                .Static = .{
                    .ident = owned_ident,
                    .global = global,
                    .initial_value = initial_value,
                    .type = typez,
                },
            },
        };
    }
    pub fn localVarSymbol(allocator: Allocator, ident: []const u8, typez: *Ast.BuiltinType) Symbol {
        const owned_ident = allocator.dupe(u8, ident) catch unreachable;
        return .{
            .Var = .{
                .Local = .{
                    .ident = owned_ident,
                    .type = typez,
                },
            },
        };
    }

    pub fn fnSymbol(
        arena: Allocator,
        ident: []const u8,
        params: ArrayList([]const u8),
        global: bool,
        defined: bool,
        typez: *Ast.BuiltinType,
    ) Symbol {
        const owned_ident = arena.dupe(u8, ident) catch unreachable;
        return .{
            .Fn = .{
                .ident = owned_ident,
                .params = params,
                .global = global,
                .defined = defined,
                .type = typez,
            },
        };
    }

    pub const VarSymbol = union(enum) {
        // this static is not in terms of C, but rather ASM
        // static means we store this value in .data section of ASM file
        Static: struct { ident: []const u8, global: bool, initial_value: InitialValue, type: *Ast.BuiltinType },
        Local: struct { ident: []const u8, type: *Ast.BuiltinType },

        pub fn getType(self: @This()) *Ast.BuiltinType {
            return switch (self) {
                .Static => |s| s.type,
                .Local => |l| l.type,
            };
        }
    };

    pub const FnSymbol = struct {
        ident: []const u8,
        params: ArrayList([]const u8),
        type: *Ast.BuiltinType,

        defined: bool,
        global: bool,
    };

    pub const StaticInit = union(enum) {
        Int: i64,
        Long: i64,

        pub fn init(constant: Ast.Constant) @This() {
            return switch (constant) {
                .Int => |v| int(v),
                .Long => |v| long(v),
            };
        }

        pub fn int(value: i64) @This() {
            return .{ .Int = value };
        }
        pub fn long(value: i64) @This() {
            return .{ .Long = value };
        }
    };

    pub const InitialValue = union(enum) {
        tentative,
        initial: StaticInit,
        no_initializer,

        pub fn initialValue(value: StaticInit) @This() {
            return .{ .initial = value };
        }
    };
};

const std = @import("std");
const Allocator = std.mem.Allocator;
// const ArrayList = std.ArrayList;
const ArrayList = @import("from_scratch.zig").ArrayList;
const StringHashMap = std.StringHashMap;
const AstParser = @import("AstParser.zig");
const Ast = AstParser.Ast;
