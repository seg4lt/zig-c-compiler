pub const CompilerError = error{ CompilerBug, PrintFailed };

/// Printer for the compiler
/// Just keep Io things here
pub const Printer = struct {
    writer_state: std.Io.Writer.Allocating,

    pub fn init(arena: Allocator) @This() {
        return .{
            .writer_state = std.Io.Writer.Allocating.init(arena),
        };
    }
    pub fn writer(self: *@This()) *std.Io.Writer {
        return &self.writer_state.writer;
    }

    const PrintOptions = struct { show_whitespace: bool = false };

    pub fn printToStdErr(self: *@This(), options: PrintOptions) !void {
        const to_print = self.writer_state.written();
        if (!options.show_whitespace) {
            try std.fs.File.stderr().writeAll(to_print);
            return;
        }

        // Need to find better way to do this
        // I need to flush, but what if I have big []u8s like this and I don't know size of it
        // I don't want to create a buffer of same size as it is just waste of memory
        // ?? Can we stream data to stderr directly?
        // ?? Any other way to do this?
        var buf: [1024]u8 = undefined;
        var count: usize = 0;
        var stderr_state = std.fs.File.stderr().writer(&buf);
        const stderr = &stderr_state.interface;

        for (to_print) |c| {
            count += try if (c == ' ') stderr.write("﹍") else try stderr.write(&[_]u8{c});
            if (count >= 1000) {
                try stderr.flush();
                count = 0;
            }
        }
        try stderr.flush();
    }
};

const std = @import("std");
const Allocator = std.mem.Allocator;
