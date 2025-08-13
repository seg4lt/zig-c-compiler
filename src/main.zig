const Arena = @import("Arena.zig");
pub fn main() !void {
    const gpa, const deinit = getAllocator();
    defer if (deinit) {
        const leak_status = debug_allocator.deinit();
        if (builtin.mode == .Debug) std.log.debug("----- LEAK STATUS: {any} ----- ", .{leak_status});
    };

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

    if (args.flag.sema) {
        try Sema.sema(.{
            .program = program_ast orelse return error.OwwwMyyyGauudddAstIsNull,
            .arena = compiler_ctx.semaArena(),
            .scratch_arena = compiler_ctx.scratchArena(),
            .symbol_table = &compiler_ctx.symbol_table,
            .error_reporter = compiler_ctx.error_reporter,
            .random = compiler_ctx.random,
            .print_ast = true,
        });
    }

    compiler_ctx.resetScratchArena();
    compiler_ctx.deinitLexerArena();

    const tacky_pg = if (args.flag.tacky) TackyIR.genTacky(.{
        .arena = compiler_ctx.tackyArena(),
        .pg = program_ast orelse return error.OwwwMyyyGauudddAstIsNull,
        .print = true,
    }) else null;
    compiler_ctx.resetScratchArena();
    compiler_ctx.deinitParserArena();
    compiler_ctx.deinitSemaArena();

    const codegen_pg = if (args.flag.codegen) Codegen.emit(.{
        .arena = compiler_ctx.codegenArena(),
        .scratch_arena = compiler_ctx.scratchArena(),
        .pg = tacky_pg orelse return error.BooooYourEyeRrrrIsNull,
        .symbol_table = &compiler_ctx.symbol_table,
        .print_codegen = true,
    }) else null;
    compiler_ctx.resetScratchArena();
    compiler_ctx.deinitTackyArena();

    if (args.flag.assemble) {
        try CodeEmission.emit(.{
            .arena = compiler_ctx.codeEmissionArena(),
            .scratch_arena = compiler_ctx.scratchArena(),
            .src_path_no_ext = args.src_path[0 .. args.src_path.len - 2],
            .pg = codegen_pg orelse return error.WaattDHekkCogegenProgramIsNull,
            .symbol_table = &compiler_ctx.symbol_table,
            .print = true,
        });
    }
    compiler_ctx.resetScratchArena();
    compiler_ctx.deinitCodegenArena();
    compiler_ctx.deinitCodeEmissionArena();

    if (args.flag.link) {
        assembleAndLink(
            compiler_ctx.scratchArena(),
            args.src_path[0 .. args.src_path.len - 2],
            args.output_type,
        );
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
const TackyIR = @import("TackyIR.zig");
const Codegen = @import("Codegen.zig");
const CodeEmission = @import("CodeEmission.zig");
const compiler_driver = @import("compiler_driver.zig");
const ErrorReporter = @import("ErrorReporter.zig");
const CompilerContext = @import("CompilerContext.zig");
const Sema = @import("sema/Sema.zig");
const preprocessor = compiler_driver.preprocessor;
const assembleAndLink = compiler_driver.assembleAndLink;
const Allocator = std.mem.Allocator;
const log = std.log;

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

test {
    _ = @import("std").testing.refAllDeclsRecursive(@This());
}
