first_block: ?*Block,
current_block: ?*Block,
default_block_size: usize,
block_allocator: Allocator,

pub fn init(block_allocator: Allocator) @This() {
    return initWithBlockSize(block_allocator, 4096);
}
pub fn initWithBlockSize(block_allocator: Allocator, comptime block_size: usize) @This() {
    comptime {
        if (block_size & (block_size - 1) != 0) @compileError("block size must be power of 2");
    }
    const block = Block.init(block_allocator, block_size);
    return .{
        .first_block = block,
        .current_block = block,
        .default_block_size = block_size,
        .block_allocator = block_allocator,
    };
}
pub fn deinit(arena: *@This()) void {
    var block = arena.first_block;
    while (block) |b| {
        const next = b.next;
        Block.deinit(b, arena.block_allocator);
        block = next;
    }
}

pub fn allocator(self: *@This()) Allocator {
    return .{
        .ptr = self,
        .vtable = &.{
            .alloc = typeErasedAlloc,
            .resize = typeErasedResize,
            .free = typeErasedFree,
            .remap = typeErasedRemap,
        },
    };
}

pub fn reset(arena: *@This(), mode: std.heap.ArenaAllocator.ResetMode) bool {
    // TODO(Aman): Add support for ResetMode so I don't have to free memory
    _ = mode;
    var block = arena.first_block;
    while (block) |b| {
        const next = b.next;
        Block.deinit(b, arena.block_allocator);
        block = next;
    }

    block = Block.init(arena.block_allocator, arena.default_block_size);
    arena.* = .{
        .first_block = block,
        .current_block = block,
        .default_block_size = arena.default_block_size,
        .block_allocator = arena.block_allocator,
    };
    return true;
}

fn typeErasedRemap(ctx: *anyopaque, memory: []u8, alignment: mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
    return remap(@ptrCast(@alignCast(ctx)), memory, alignment, new_len, ret_addr);
}

fn remap(arena: *@This(), memory: []u8, alignment: mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
    if (arena.resize(memory, alignment, new_len, ret_addr)) {
        return memory.ptr;
    }
    return null;
}

pub fn typeErasedAlloc(ctx: *anyopaque, len: usize, alignment: mem.Alignment, ret_addr: usize) ?[*]u8 {
    return alloc(@ptrCast(@alignCast(ctx)), len, alignment, ret_addr);
}

pub fn alloc(arena: *@This(), len: usize, alignment: mem.Alignment, ret_addr: usize) ?[*]u8 {
    _ = ret_addr;
    const alignment_in_bytes = alignment.toByteUnits();
    if (arena.current_block == null) std.debug.panic("arena is not setup properly", .{});

    var block = arena.current_block.?;
    var used_aligned = alignUp(block.used, alignment_in_bytes);

    if (used_aligned + len > block.memory.len) {
        const block_size = @max(len, arena.default_block_size);
        const new_block = Block.init(arena.block_allocator, block_size);
        block.next = new_block;
        arena.current_block = new_block;
        block = new_block;
        used_aligned = 0; // Reset used_aligned since we are starting a new block
    }
    const ptr: [*]u8 = block.memory.ptr + used_aligned;
    block.used = used_aligned;
    block.used += len;
    return ptr;
}

fn resize(ctx: *@This(), buf: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
    _ = ctx;
    _ = alignment;
    _ = ret_addr;
    return new_len <= buf.len;
}

fn typeErasedResize(ctx: *anyopaque, buf: []u8, alignment: mem.Alignment, new_len: usize, ret_addr: usize) bool {
    return resize(@ptrCast(@alignCast(ctx)), buf, alignment, new_len, ret_addr);
}

fn typeErasedFree(ctx: *anyopaque, memory: []u8, alignment: mem.Alignment, ret_addr: usize) void {
    free(@ptrCast(@alignCast(ctx)), memory, alignment, ret_addr);
}

fn free(ctx: *@This(), memory: []u8, alignment: mem.Alignment, ret_addr: usize) void {
    _ = ctx;
    _ = memory;
    _ = alignment;
    _ = ret_addr;
    return;
}

fn alignUp(size: usize, alignment: usize) usize {
    if (alignment == 0) return size;
    return (size + alignment - 1) & ~(alignment - 1);
}

const Block = struct {
    next: ?*Block = null,
    used: usize = 0,
    memory: []u8,

    pub fn init(block_allocator: Allocator, block_size: usize) *Block {
        const block = block_allocator.create(Block) catch unreachable;

        const memory = block_allocator.alloc(u8, block_size) catch unreachable;
        block.* = .{
            .memory = memory,
            .used = 0,
        };
        return block;
    }

    pub fn deinit(block: *Block, block_allocator: Allocator) void {
        block.used = 0;
        block_allocator.free(block.memory);
        block_allocator.destroy(block);
    }
};

const std = @import("std");
const mem = std.mem;
const Allocator = std.mem.Allocator;
