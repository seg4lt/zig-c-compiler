pub fn main() !void {
    const gpa, const deinit = getAllocator();
    defer {
        if (deinit) {
            const leak_status = debug_allocator.deinit();
            if (builtin.mode == .Debug) std.log.debug("----- LEAK STATUS: {any} ----- ", .{leak_status});
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

    if (args.flag.assemble) {
        try CodeEmission.emit(.{
            .arena = compiler_ctx.codeEmissionArena(),
            .scratch_arena = compiler_ctx.scratchArena(),
            .src_path_no_ext = args.src_path[0 .. args.src_path.len - 2],
            .pg = asm_gen orelse return error.WaattDHekkAsmGenIsNull,
        });
    }
    compiler_ctx.resetScratchArena();
    compiler_ctx.deinitCodeEmissionArena();

    if (args.flag.link) {
        assembleAndLink(compiler_ctx.scratchArena(), args.src_path[0 .. args.src_path.len - 2], .exe);
    }
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
const CodeEmission = @import("CodeEmission.zig");
const compiler_driver = @import("compiler_driver.zig");
const ErrorReporter = @import("ErrorReporter.zig");
const CompilerContext = @import("CompilerContext.zig");
const preprocessor = compiler_driver.preprocessor;
const assembleAndLink = compiler_driver.assembleAndLink;
const Allocator = std.mem.Allocator;
const log = std.log;

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

test {
    _ = @import("std").testing.refAllDeclsRecursive(@This());
}
