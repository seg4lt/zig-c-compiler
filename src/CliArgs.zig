src_path: []const u8,
flag: CliFlag,

const Self = @This();

const CliFlag = packed struct(u8) {
    lex: bool = true,
    parse: bool = true,
    codegen: bool = true,
    assemble: bool = true,
    _padding: u4 = 0,

    pub const LEX = fromBits(0b1);
    pub const PARSE = fromBits(0b11);
    pub const CODE_GEN = fromBits(0b111);
    pub const ALL = CliFlag{};

    pub fn fromBits(bits: u8) CliFlag {
        return @bitCast(bits);
    }
};

const CliArgsError = error{
    SourceFileAbsent,
    InvalidSourceFile,
};

pub fn parse() !Self {
    var args = std.process.args();
    _ = args.next();

    var flag: CliFlag = .ALL;
    var may_src_path: ?[]const u8 = null;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, "--lex", arg)) {
            flag = .LEX;
            continue;
        }
        if (std.mem.eql(u8, "--parse", arg)) {
            flag = .PARSE;
            continue;
        }
        if (std.mem.eql(u8, "--code-gen", arg)) {
            flag = .CODE_GEN;
            continue;
        }
        may_src_path = arg;
    }

    if (may_src_path == null) {
        return CliArgsError.SourceFileAbsent;
    }
    const src_path = may_src_path.?;

    if (!std.mem.endsWith(u8, src_path, ".c")) {
        return CliArgsError.InvalidSourceFile;
    }
    return .{
        .src_path = src_path,
        .flag = flag,
    };
}

const std = @import("std");
