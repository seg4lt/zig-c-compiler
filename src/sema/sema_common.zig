pub const SemaOptions = struct {
    program: *Ast.Program,
    arena: Allocator,
    scratch_arena: Allocator,
    error_reporter: *ErrorReporter,
    random: std.Random,
    print_ast: bool = false,
};

pub const SemaError = error{
    UndeclaredLabel,
    DuplicateLabel,
    DuplicateIdentifier,
    UndeclaredVariable,
    InvalidLValue,
    InvalidPlacement,
} || CompilerError;

pub fn makeVar(allocator: Allocator, random: std.Random, name: []const u8) []const u8 {
    const random_int = random.int(u16);
    return std.fmt.allocPrint(allocator, "var_{s}___{d}", .{ name, random_int }) catch unreachable;
}

pub fn makeGotoLabel(allocator: Allocator, random: std.Random, name: []const u8) []const u8 {
    const random_int = random.int(u16);
    return std.fmt.allocPrint(allocator, "L_goto_label_{s}___{d}", .{ name, random_int }) catch unreachable;
}
pub fn makeLabel(allocator: Allocator, random: std.Random, name: []const u8) []const u8 {
    const random_int = random.int(u16);
    return std.fmt.allocPrint(allocator, "L_{s}___{d}", .{ name, random_int }) catch unreachable;
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const CompilerError = @import("../util.zig").CompilerError;
const Ast = @import("../AstParser.zig").Ast;
const ErrorReporter = @import("../ErrorReporter.zig");
