const std = @import("std");
const z80 = @import("z80.zig");

pub const TestData = struct {
    name: []const u8,
    initial: Initial,
    final: Final,
    cycles: []const struct { u16, ?u8, []const u8 },
};

pub const Initial = struct {
    pc: u16,
    sp: u16,
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
    i: u8,
    r: u8,
    ei: u2,
    wz: u16,
    ix: u16,
    iy: u16,
    af_: u16,
    bc_: u16,
    de_: u16,
    hl_: u16,
    im: u2,
    p: u8,
    q: u8,
    iff1: u1,
    iff2: u1,
    ram: []const struct { u16, u8 },
};

pub const Final = struct {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
    i: u8,
    r: u8,
    af_: u16,
    bc_: u16,
    de_: u16,
    hl_: u16,
    ix: u16,
    iy: u16,
    pc: u16,
    sp: u16,
    wz: u16,
    iff1: u1,
    iff2: u1,
    im: u2,
    ei: u2,
    p: u8,
    q: u8,
    ram: []const struct { u16, u8 },
};

//
// Loads a test file from json into a TestData struct
//
pub fn read_test(test_file: []const u8) ![]const TestData {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const file = try std.fs.cwd().openFile(test_file, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const json_bytes = try file.readToEndAlloc(allocator, file_size);
    defer allocator.free(json_bytes);

    //std.debug.print("Read: {s}\n", .{json_bytes});

    var scanner = std.json.Scanner.initCompleteInput(allocator, json_bytes);
    defer scanner.deinit();

    var diagnostics = std.json.Diagnostics{};
    scanner.enableDiagnostics(&diagnostics);
    errdefer std.log.err("byte offset {d}\nbytes: {s}|{s}\n", .{
        diagnostics.getByteOffset(),
        json_bytes[diagnostics.getByteOffset() -| 32..][0..32],
        json_bytes[diagnostics.getByteOffset()..][0..32],
    });

    const json = try std.json.parseFromSlice([]TestData, allocator, json_bytes, .{ .allocate = .alloc_always, .ignore_unknown_fields = true });
    const value = json.value;
    //std.debug.print("name is {any}", .{value[0].name});
    //std.debug.print("name is {any}", .{value});

    return value;
}

// Run a test
pub fn run_test(test_data: TestData) bool {
    // Set up CPU
    var in: [256]u8 = undefined;
    var out: [256]u8 = undefined;
    var cpu = z80.z80{ .ports_in = in[0..], .ports_out = out[0..] };
    var ram: [0x10000]u8 = undefined;
    // Set up initial state
    cpu.pc = test_data.initial.pc;
    cpu.sp = test_data.initial.sp;
    cpu.a = test_data.initial.a;
    cpu.b = test_data.initial.b;
    cpu.c = test_data.initial.c;
    cpu.d = test_data.initial.d;
    cpu.e = test_data.initial.e;
    cpu.f.byte = test_data.initial.f;
    cpu.h = test_data.initial.h;
    cpu.l = test_data.initial.l;
    cpu.i = test_data.initial.i;
    cpu.r = test_data.initial.r;
    cpu.ix = test_data.initial.ix;
    cpu.iy = test_data.initial.iy;
    cpu.interrupt_mode = test_data.initial.im;
    cpu.iff1 = test_data.initial.iff1;
    cpu.iff2 = test_data.initial.iff2;
    for (test_data.initial.ram) |entry| {
        ram[entry[0]] = entry[1];
    }

    // Run cycles
    cpu.execute_instruction(ram[0..], false);

    var fail_test: bool = false;
    // Compare final state to expected final state
    if (cpu.a != test_data.final.a) {
        fail_test = true;
        std.debug.print("   A mismatch: got {x:02}, expected {x:02}\n", .{ cpu.a, test_data.final.a });
    }
    if (cpu.b != test_data.final.b) {
        fail_test = true;
        std.debug.print("   B mismatch: got {x:02}, expected {x:02}\n", .{ cpu.b, test_data.final.b });
    }
    if (cpu.c != test_data.final.c) {
        fail_test = true;
        std.debug.print("   C mismatch: got {x:02}, expected {x:02}\n", .{ cpu.c, test_data.final.c });
    }
    if (cpu.d != test_data.final.d) {
        fail_test = true;
        std.debug.print("   D mismatch: got {x:02}, expected {x:02}\n", .{ cpu.d, test_data.final.d });
    }
    if (cpu.e != test_data.final.e) {
        fail_test = true;
        std.debug.print("   E mismatch: got {x:02}, expected {x:02}\n", .{ cpu.e, test_data.final.e });
    }
    if (cpu.f.byte & 0b11010111 != test_data.final.f & 0b11010111) {
        fail_test = true;
        std.debug.print("   F mismatch: initial {b:08}, got {b:08}, expected {b:08}\n", .{ test_data.initial.f, cpu.f.byte, test_data.final.f });
    }
    if (cpu.h != test_data.final.h) {
        fail_test = true;
        std.debug.print("   H mismatch: got {x:02}, expected {x:02}\n", .{ cpu.h, test_data.final.h });
    }
    if (cpu.l != test_data.final.l) {
        fail_test = true;
        std.debug.print("   L mismatch: got {x:02}, expected {x:02}\n", .{ cpu.l, test_data.final.l });
    }
    if (cpu.i != test_data.final.i) {
        fail_test = true;
        std.debug.print("   I mismatch: got {x:02}, expected {x:02}\n", .{ cpu.i, test_data.final.i });
    }
    if (cpu.r != test_data.final.r) {
        fail_test = true;
        std.debug.print("   R mismatch: got {x:02}, expected {x:02}\n", .{ cpu.r, test_data.final.r });
    }
    if (cpu.ix != test_data.final.ix) {
        fail_test = true;
        std.debug.print("   IX mismatch: got {x:04}, expected {x:02}\n", .{ cpu.ix, test_data.final.ix });
    }
    if (cpu.iy != test_data.final.iy) {
        fail_test = true;
        std.debug.print("   IY mismatch: got {x:04}, expected {x:02}\n", .{ cpu.iy, test_data.final.iy });
    }
    if (cpu.sp != test_data.final.sp) {
        fail_test = true;
        std.debug.print("   SP mismatch: got {x:04}, expected {x:02}\n", .{ cpu.sp, test_data.final.sp });
    }
    if (cpu.pc != test_data.final.pc) {
        fail_test = true;
        std.debug.print("   PC mismatch: got {x:04}, expected {x:02}\n", .{ cpu.pc, test_data.final.pc });
    }
    if (cpu.interrupt_mode != test_data.final.im) {
        fail_test = true;
        std.debug.print("   IM mismatch: got {x}, expected {x:02}\n", .{ cpu.interrupt_mode, test_data.final.im });
    }
    if (cpu.iff1 != test_data.final.iff1) {
        fail_test = true;
        std.debug.print("   IFF1 mismatch: got {x}, expected {x:02}\n", .{ cpu.iff1, test_data.final.iff1 });
    }
    if (cpu.iff2 != test_data.final.iff2) {
        fail_test = true;
        std.debug.print("   IFF2 mismatch: got {x}, expected {x:02}\n", .{ cpu.iff2, test_data.final.iff2 });
    }
    for (test_data.final.ram) |entry| {
        if (ram[entry[0]] != entry[1]) {
            fail_test = true;
            std.debug.print("   RAM mismatch at {x:04}: got {x:02}, expected {x:02}\n", .{ entry[0], ram[entry[0]], entry[1] });
            // Report results
        }
    }
    return fail_test;
}

// run all tests in a file
pub fn run_test_file(file_name: []const u8) !void {
    var tests_failed: u16 = 0;
    const tests = try read_test(file_name);

    std.debug.print("Loaded {d} tests\n", .{tests.len});
    for (tests) |a_test| {
        std.debug.print("Test name: {s}\n", .{a_test.name});
        if (run_test(a_test) == true)
            tests_failed += 1;
    }
    std.debug.print("Tests failed: {d}/{d}\n", .{ tests_failed, tests.len });
}

//
// run all tests
//
pub fn run_all_tests(directory: []const u8) !void {
    const dir = try std.fs.openDirAbsolute(directory, .{ .iterate = true });
    //defer dir.close();

    var file_list: std.ArrayList([]const u8) = std.ArrayList([]const u8)
        .init(std.heap.page_allocator);
    defer file_list.deinit();

    var it = dir.iterate();
    while (true) {
        const entry = try it.next() orelse null;
        if (entry == null) break;
        if (entry.?.kind == std.fs.File.Kind.file and
            std.mem.containsAtLeast(u8, entry.?.name, 1, ".json"))
        {
            // can't just append entry.name need to copy it to a new buffer, with memory allocation
            const value = try std.mem.Allocator.dupe(std.heap.page_allocator, u8, entry.?.name);
            try file_list.append(value);
        }
    }
    if (file_list.items.len == 0) {
        std.debug.print("No .json files found in directory {s}\n", .{directory});
        return;
    }

    // go through the list and print the files
    std.debug.print("Files in directory:\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    for (file_list.items, 0..) |file, idx| {
        //var path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const full_path = try std.fs.path.join(allocator, &.{ directory, file });
        std.debug.print("{}: {s}\n", .{ idx, file[0..] });
        try run_test_file(full_path);
    }
}

// pub fn main() !void {
//     try run_all_tests("C:/Users/cmare/Downloads/z80-main/z80-main/v1");
// }

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var args = try std.process.ArgIterator.initWithAllocator(allocator);
    defer args.deinit();

    _ = args.next(); // skip program name

    if (args.next()) |arg| {
        const full_path = try std.fs.path.join(allocator, &.{ "C:/Users/cmare/Downloads/z80-main/z80-main/v1", arg });

        // Run a single test file
        try run_test_file(full_path);
    } else {
        // Run all tests in the default directory
        try run_all_tests("C:/Users/cmare/Downloads/z80-main/z80-main/v1");
    }
}
