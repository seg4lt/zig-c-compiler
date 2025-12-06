pub const Size = enum(u8) {
    byte = 1,
    word = 2,
    dword = 4,
    qword = 8,
};

pub const Symbol = union(enum) {
    ObjEntry: struct { asm_size: Size, is_static: bool },
    FnEntry: struct { is_defined: bool },
};

inner: StringHashMap(Symbol),
arena: Allocator,

const Self = @This();

pub fn from(symbol_table: *const SymbolTable) Self {
    // use same arena for now
    var self = Self.init(symbol_table.arena);
    var it = symbol_table.inner.iterator();
    while (it.next()) |entry| {
        switch (entry.value_ptr.*) {
            .Var => |var_symbol| {
                switch (var_symbol) {
                    .Local => |local| {
                        self.put(local.ident, .{
                            .ObjEntry = .{
                                .asm_size = getSizeFromType(local.type),
                                .is_static = false,
                            },
                        });
                    },
                    .Static => |static| {
                        self.put(static.ident, .{
                            .ObjEntry = .{
                                .asm_size = getSizeFromType(static.type),
                                .is_static = true,
                            },
                        });
                    },
                }
            },
            .Fn => |fn_symbol| {
                self.put(fn_symbol.ident, .{
                    .FnEntry = .{ .is_defined = fn_symbol.defined },
                });
            },
        }
    }
    return self;
}

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

pub fn getSizeFromType(typez: *const Ast.BuiltinType) Size {
    return switch (typez.*) {
        .Int => .dword,
        .Long => .qword,
        else => std.debug.panic("** Compiler Bug ** Unreachable path: expected int or long type", .{}),
    };
}

const std = @import("std");
const StringHashMap = std.StringHashMap;
const Allocator = std.mem.Allocator;
const SymbolTable = @import("SymbolTable.zig");
const Ast = @import("AstParser.zig").Ast;
