pub fn main() !void {
    const gpa, const deinit = getAllocator();
    defer {
        if (deinit) {
            const leak_status = debug_allocator.deinit();
            std.log.debug("----- LEAK STATUS: {any} ----- ", .{leak_status});
        }
    }

    try runCompiler(gpa);
}

fn runCompiler(gpa: Allocator) !void {
    const args: CliArgs = try .parse();
    const src = try preprocessor(gpa, args.src_path);
    defer gpa.free(src);

    var compiler_ctx = CompilerContext.init(gpa, src, args.src_path);
    defer compiler_ctx.deinit();

    const lex_tokens = if (args.flag.lex) try Lexer.parseTokens(.{
        .src = src,
        .arena = compiler_ctx.lexerArena(),
        .scratch_arena = compiler_ctx.scratchArena(),
        .error_reporter = compiler_ctx.error_reporter,
        .print_tokens = true,
    }) else null;

    compiler_ctx.resetScratchArena();
    const program_ast = if (args.flag.parse) try AstParser.parse(.{
        .tokens = lex_tokens orelse return error.OwwwMyyyGauudddLexTokenIsNull,
        .arena = compiler_ctx.parserArena(),
        .scratch_arena = compiler_ctx.scratchArena(),
        .error_reporter = compiler_ctx.error_reporter,
        .print_ast = true,
    }) else null;

    compiler_ctx.resetScratchArena();
    compiler_ctx.deinitLexerArena();

    const asm_gen = if (args.flag.codegen) AsmGen.asmGen(.{
        .arena = compiler_ctx.codegenArena(),
        .scratch_arena = compiler_ctx.scratchArena(),
        .pg = program_ast orelse return error.BooooAstIsNull,
        .print_codegen = true,
    }) else null;
    
    compiler_ctx.resetScratchArena();
    compiler_ctx.deinitParserArena();
    _ = asm_gen;
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
const Lexer = @import("Lexer.zig");
const AstParser = @import("AstParser.zig");
const AsmGen = @import("AsmGen.zig");
const compiler_driver = @import("compiler_driver.zig");
const ErrorReporter = @import("ErrorReporter.zig");
const CompilerContext = @import("CompilerContext.zig");
const preprocessor = compiler_driver.preprocessor;
const Allocator = std.mem.Allocator;
const log = std.log;

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

test {
    _ = @import("std").testing.refAllDeclsRecursive(@This());
}
