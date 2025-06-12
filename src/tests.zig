const main = @import("main.zig");

test {
    _ = @import("std").testing.refAllDeclsRecursive(main);
}
