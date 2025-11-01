const std = @import("std");
const rl = @import("raylib");

const SAMPLE_RATE = 14080;
const FREQUENCY = 440.0; // A4 note
const AMPLITUDE = 0.3;
var sample_nbr = 0;

const wave: [32]u8 = .{ 0x07, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0d, 0x0e, 0x0e, 0x0e, 0x0d, 0x0d, 0x0c, 0x0b, 0x0a, 0x09, 0x07, 0x05, 0x04, 0x03, 0x02, 0x01, 0x01, 0x00, 0x00, 0x00, 0x01, 0x01, 0x02, 0x03, 0x04, 0x05 };

// Audio callback function that generates sine wave samples
fn audioCallback(buffer: ?*anyopaque, frames: c_uint, ram: ?*anyopaque) callconv(.C) void {
    const samples = @as([*]f32, @ptrCast(@alignCast(buffer)));
    const frame_count = @as(usize, @intCast(frames));

    // Static variable to maintain phase across callbacks
    const S = struct {
        var phase: f32 = 0.0;
    };

    var i: usize = 0;
    while (i < frame_count * 2) : (i += 2) { // *2 because stereo (left + right)
        // Generate sine wave sample
        const sample = @as(f32, @floatFromInt(ram[i + (sample_nbr * 32)])) * AMPLITUDE;

        // Write to both left and right channels
        samples[i] = sample;
        samples[i + 1] = sample;

        // Increment phase
        S.phase += 2.0 * std.math.pi * FREQUENCY / @as(f32, SAMPLE_RATE);

        // Keep phase in range [0, 2π) to prevent float overflow
        if (S.phase >= 2.0 * std.math.pi) {
            S.phase -= 2.0 * std.math.pi;
        }
    }
}

pub fn playSineWave() !void {
    const screenWidth = 800;
    const screenHeight = 450;

    rl.initWindow(screenWidth, screenHeight, "Raylib - Sine Wave Generator");
    defer rl.closeWindow();

    // Initialize audio device
    rl.initAudioDevice();
    defer rl.closeAudioDevice();

    // Set up audio stream
    rl.setAudioStreamBufferSizeDefault(4096);

    const stream = try rl.loadAudioStream(SAMPLE_RATE, 32, 2); // 32-bit float, stereo
    defer rl.unloadAudioStream(stream);

    // Attach our callback to the stream
    rl.setAudioStreamCallback(stream, audioCallback);

    // Start playing
    rl.playAudioStream(stream);

    rl.setTargetFPS(60);

    var isPlaying = true;

    while (!rl.windowShouldClose()) {
        // Toggle playback with spacebar
        if (rl.isKeyPressed(rl.KeyboardKey.space)) {
            if (isPlaying) {
                rl.pauseAudioStream(stream);
                isPlaying = false;
            } else {
                rl.resumeAudioStream(stream);
                isPlaying = true;
            }
        }

        if (rl.isKeyPressed(rl.KeyboardKey.right)) {
            sample_nbr = (sample_nbr + 1) % 8;
        } else if (rl.isKeyPressed(rl.KeyboardKey.left)) {
            sample_nbr = (sample_nbr + 7) % 8; // +7 is equivalent to -1 mod 8
        }

        // Update audio stream (this is handled automatically by raylib)
        // No manual update needed when using callback

        // Drawing
        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(rl.Color.ray_white);

        rl.drawText("SINE WAVE GENERATOR", 240, 150, 30, rl.Color.dark_gray);
        //rl.drawText(rl.textFormat("Frequency: {} Hz", .{FREQUENCY}), 280, 200, 20, rl.Color.gray);

        if (isPlaying) {
            rl.drawText("STATUS: PLAYING", 300, 250, 20, rl.Color.dark_green);
            rl.drawText("Press SPACE to pause", 270, 300, 20, rl.Color.light_gray);
        } else {
            rl.drawText("STATUS: PAUSED", 300, 250, 20, rl.Color.red);
            rl.drawText("Press SPACE to play", 275, 300, 20, rl.Color.light_gray);
        }

        rl.drawText("Press ESC to quit", 290, 350, 20, rl.Color.light_gray);
    }
}

const rom_layout = struct {
    filename: []const u8,
    offset: u16,
};

const samples_rom_start: u16 = 0x0000;

const roms = [_]rom_layout{
    .{ .filename = "82s126.1m", .offset = samples_rom_start },
    .{ .filename = "82s126.3m", .offset = samples_rom_start + 256 },
};

fn init(ram: []u8, rom_path: []const u8) void {

    // Load all roms
    for (roms) |rom| {
        std.debug.print("Loading ROM: {s} at offset {x:04}\n", .{ rom.filename, rom.offset });
        _ = load_rom(rom_path, rom.filename, ram[rom.offset..]) catch blk: {
            std.debug.print("Error loading ROM: {s}\n", .{rom.filename});
            break :blk 0;
        };
    }
}

// - load rom
// loads rom into memory
fn load_rom(rom_path: []const u8, rom_name: []const u8, ram: []u8) !u64 {
    // concatenate path and rom name
    const full_path = std.fs.path.join(std.heap.page_allocator, &.{ rom_path, rom_name }) catch {
        std.debug.print("Failed to join ROM path\n", .{});
        return 0;
    };
    defer std.heap.page_allocator.free(full_path);
    // open file
    const file = try std.fs.cwd().openFile(full_path, .{});
    defer file.close();

    const rom_size = try file.getEndPos();
    const rom_data = try file.readToEndAlloc(std.heap.page_allocator, rom_size);
    defer std.heap.page_allocator.free(rom_data);

    // Load ROM data into memory
    std.mem.copyForwards(u8, ram[0..], rom_data);
    return rom_size;
}

pub fn main() !void {
    var ram: [0x10000]u8 = undefined;
    const rom_path = "resources/roms";
    init(ram[0..], rom_path);

    try playSineWave();
}
