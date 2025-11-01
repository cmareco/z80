// PAC-MAN emulator in Zig
// Uses a Z80 CPU core and Raylib for graphics and input handling
//

const std = @import("std");
const rl = @import("raylib");
const zilog = @import("z80.zig");

const scale_factor: u8 = 3;

const Pacman = struct {
    // memory map

    cpu: zilog.z80 = undefined,
    ram: [0xFFFF]u8 = undefined, // 64KB RAM
    ports: [0xFF]u8 = undefined, // 256 I/O ports

    // RGB colors referenced by palette indices
    colors: [32]u32 = .{
        0x000000FF, 0xFF0000FF, 0xDE9751FF, 0xFFB8FFFF,
        0x000000FF, 0x00FFFFFF, 0x47B8FFFF, 0xFFB851FF,
        0x000000FF, 0xFFFF00FF, 0x000000FF, 0x2121FFFF,
        0x00FF00FF, 0x47B8AEFF, 0xFFB8AEFF, 0xDEDEFFFF,
        0x000000FF, 0x000000FF, 0x000000FF, 0x000000FF,
        0x000000FF, 0x000000FF, 0x000000FF, 0x000000FF,
        0x000000FF, 0x000000FF, 0x000000FF, 0x000000FF,
        0x000000FF, 0x000000FF, 0x000000FF, 0x000000FF,
    },

    raylib_colors: [32]rl.Color = undefined,

    // memory locations
    const video_ram_start: u16 = 0x4000;
    const palette_ram_start: u16 = 0x4400;
    const tile_rom_start: u16 = 0xA000;
    const palette_rom_start: u16 = 0xB000;
    const sprite_rom_start: u16 = 0xD000;
    const samples_rom_start: u16 = 0xE000;

    // screen dimensions
    const screen_width: i32 = 64 + 224 * @as(i32, scale_factor);
    const screen_height: i32 = 288 * @as(i32, scale_factor);

    const tilemap_width: u8 = 32;
    const tilemap_height: u8 = 36;

    // sprites offsets
    var sprite_offset_no_flip: [8][2]u8 = .{
        // 5 1 | 6 2 | 7 3 | 4 0
        .{ 16, 12 }, .{ 16, 0 }, .{ 16, 4 }, .{ 16, 8 },
        .{ 8, 12 },  .{ 8, 0 },  .{ 8, 4 },  .{ 8, 8 },
    };

    var sprite_offset_flip_x: [8][2]u8 = .{
        .{ 0, 12 }, .{ 0, 0 }, .{ 0, 4 }, .{ 0, 8 },
        .{ 8, 12 }, .{ 8, 0 }, .{ 8, 4 }, .{ 8, 8 },
    };

    var sprite_offset_flip_y: [8][2]u8 = .{
        .{ 16, 0 }, .{ 16, 12 }, .{ 16, 8 }, .{ 16, 4 },
        .{ 8, 0 },  .{ 8, 12 },  .{ 8, 8 },  .{ 8, 4 },
    };

    var sprite_offset_flip_xy: [8][2]u8 = .{
        .{ 0, 0 }, .{ 0, 12 }, .{ 0, 8 }, .{ 0, 4 },
        .{ 8, 0 }, .{ 8, 12 }, .{ 8, 8 }, .{ 8, 4 },
    };

    // y pixel order arrays: for the vertical arrangement of pixels in a sprite byte
    var sprite_y_pixels_no_flip: [4]u8 = .{ 3, 2, 1, 0 }; // pixel 0 is at the bottom (y + 3)
    var sprite_y_pixels_flip_y: [4]u8 = .{ 0, 1, 2, 3 }; // pixel 0 is at the top (y + 0)

    const rom_layout = struct {
        filename: []const u8,
        offset: u16,
    };

    const roms = [_]rom_layout{
        .{ .filename = "pacman.6e", .offset = 0x0000 },
        .{ .filename = "pacman.6f", .offset = 0x1000 },
        .{ .filename = "pacman.6h", .offset = 0x2000 },
        .{ .filename = "pacman.6j", .offset = 0x3000 },
        .{ .filename = "pacman.5e", .offset = tile_rom_start },
        .{ .filename = "82s126.4a", .offset = palette_rom_start },
        .{ .filename = "hpacman.5f", .offset = sprite_rom_start },
        .{ .filename = "82s126.1m", .offset = samples_rom_start },
        .{ .filename = "82s126.3m", .offset = samples_rom_start + 256 },
    };

    //
    // init machine: load roms, etc.
    //
    fn init(self: *Pacman, rom_path: []const u8) u64 {
        self.cpu = zilog.z80{ .ports_in = self.ports[0..], .ports_out = self.ports[0..] };

        // Load all roms
        for (roms) |rom| {
            std.debug.print("Loading ROM: {s} at offset {x:04}\n", .{ rom.filename, rom.offset });
            _ = load_rom(rom_path, rom.filename, self.ram[rom.offset..]) catch blk: {
                std.debug.print("Error loading ROM: {s}\n", .{rom.filename});
                break :blk 0;
            };
        }

        // initialize raylib colors from RGB values
        for (0..32) |i| {
            self.raylib_colors[i] = rl.getColor(self.colors[i]);
        }
        return 1;
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

    // - decode graphics, print sprite map
    // - tile/sprite rendering
    pub fn drawTile(self: *Pacman, tile_nbr: u8, tile_x: u16, tile_y: u16, palette_nbr: u8) void {
        // each tile is 16 bytes, 8x8 pixels, 2 bits per pixel
        // first 8 bytes are the lower part of the tile, next 8 bytes are the upper part
        // b7 b6 b5 b4 b3 b2 b1 b0
        // p3 p2 p1 p0 p3 p2 p1 p0  <- bit planes
        // each pixel is formed by combining the bits from the two planes
        // pixel value 0 = b4 & 0b00010000 >> 3 | b0 & 0b00000001
        // pixel value 1 = b5 & 0b00100000 >> 4 | b1 & 0b00000010 >> 1
        // etc

        // get tile from rom
        const buffer = self.ram[(tile_rom_start + @as(u16, tile_nbr) * 16) .. (tile_rom_start + @as(u16, tile_nbr) * 16) + 16];
        // get palette
        const palette = self.ram[palette_rom_start + (palette_nbr * 4) .. palette_rom_start + (palette_nbr * 4) + 4];

        // std.debug.print("buffer: {x:02}{x:02}{x:02}{x:02}{x:02}{x:02}{x:02}{x:02}\n", .{ buffer[0], buffer[1], buffer[2], buffer[3], buffer[4], buffer[5], buffer[6], buffer[7] });
        //std.debug.print("Drawing tile {} at ({}, {}), with palette {}\n", .{ tile_nbr, tile_x, tile_y, palette_nbr });
        for (0..16) |i| {
            var x: i32 = undefined;
            var y: i32 = undefined;

            // lower 8 bytes are the bottom half of the tile (hence the y  + 4)
            if (i < 8) {
                x = @intCast(tile_x * 8 + (7 - i));
                y = @intCast(tile_y * 8 + 4);
            } else {
                x = @intCast(tile_x * 8 + 7 - (i - 8));
                y = @intCast(tile_y * 8);
            }
            x *= @intCast(scale_factor);
            y *= @intCast(scale_factor);

            // calculate pixel values
            const pix0 = ((buffer[i] & 0b00010000) >> 3) | (buffer[i] & 0b00000001);
            rl.drawRectangle(x, y + (3 * scale_factor), scale_factor, scale_factor, self.raylib_colors[palette[pix0]]);

            const pix1 = ((buffer[i] & 0b00100000) >> 4) | (buffer[i] & 0b00000010) >> 1;
            rl.drawRectangle(x, y + (2 * scale_factor), scale_factor, scale_factor, self.raylib_colors[palette[pix1]]);

            const pix2 = ((buffer[i] & 0b01000000) >> 5) | (buffer[i] & 0b00000100) >> 2;
            rl.drawRectangle(x, y + (1 * scale_factor), scale_factor, scale_factor, self.raylib_colors[palette[pix2]]);

            const pix3 = ((buffer[i] & 0b10000000) >> 6) | (buffer[i] & 0b00001000) >> 3;
            rl.drawRectangle(x, y + 0, scale_factor, scale_factor, self.raylib_colors[palette[pix3]]);

            //std.debug.print("i={}, v = {x:02} x = {}, y = {}, pixels={}{}{}{}\n", .{ i, buffer[i], x, y, pix3, pix2, pix1, pix0 });
        }
    }

    pub fn drawTileMap(self: *Pacman) void {

        // draw bottom 2 rows of tiles (30-31)
        for (0x00..0x40) |_i| {
            const i = @as(u8, @truncate(_i));
            const tile_index: u8 = self.ram[video_ram_start + i];
            const palette_index: u8 = self.ram[palette_ram_start + i] & 0b00000011;
            const x: u8 = (31 - i % 32);
            const y = 34 + i / 32;
            //std.debug.print("Bottom: i:{} tile index: {} @ ({},{}) with palette {}\n", .{ i, tile_index, x, y, palette_index });
            self.drawTile(tile_index, x, y, palette_index);
        }

        // draw middle 32 rows of tiles (2-33)
        for (0x40..0x3C0) |_i| {
            const i = @as(u16, @truncate(_i));
            const tile_index: u8 = self.ram[video_ram_start + i];
            const palette_index: u8 = self.ram[palette_ram_start + i] & 0b00000011;
            const x = (29 - ((i - 0x40) / 32));
            const y = 2 + (i - 0x40) % 32;
            //std.debug.print("Middle: i:{} tile index: {} @ ({},{}) with palette {}\n", .{ i, tile_index, x, y, palette_index });
            self.drawTile(tile_index, x, y, palette_index);
        }

        // draw top 2 rows of tiles (0-1)
        for (0x3C0..0x400) |_i| {
            const i = @as(u16, @truncate(_i));
            const tile_index: u8 = self.ram[video_ram_start + i];
            const palette_index: u8 = self.ram[palette_ram_start + i] & 0b00000011;
            const x = (31 - (i - 0x3C0) % 32);
            const y = (i - 0x3C0) / 32;
            //std.debug.print("Top: tile index: {} @ ({},{}) with palette {}\n", .{ tile_index, x, y, palette_index });
            self.drawTile(tile_index, x, y, palette_index);
        }
    }

    pub fn drawSprite(self: *Pacman, sprite_nbr: u8, sprite_x: u16, sprite_y: u16, palette_nbr: u8, flip_x: bool, flip_y: bool) void {
        // each sprite is 64 bytes, 16x16 pixels, 2 bits per pixel
        // arranged as strips of 8 bytes (8x4 pixels)
        // no flip  flip x   flip y   flip xy
        // 5  1     1 5       5 1     1 5
        // 6  2     2 6       6 2     2 6
        // 7  3     3 7       7 3     3 7
        // 4  0     0 4       4 0     0 4
        // each byte represents 4 vertical pixels, with 2 bits per pixel, same as the tiles

        // get sprite from rom
        const buffer = self.ram[(sprite_rom_start + @as(u16, sprite_nbr) * 64) .. (sprite_rom_start + @as(u16, sprite_nbr) * 64) + 64];
        // get palette
        const palette = self.ram[palette_rom_start + (palette_nbr * 4) .. palette_rom_start + (palette_nbr * 4) + 4];

        var offset_x: u8 = undefined;
        var offset_y: u8 = undefined;
        var increment_x: i8 = undefined;
        var pixel_strip_order: *[4]u8 = undefined;

        // determine which offset array to use based on flip flags
        // set increment direction: if flip_x is set, we draw left to right (increment +1) else right to left (increment -1)
        // set pixel strip order: if flip_y is set, pixel 0 is at the top else pixel 0 is at the bottom
        var offset_array: *[8][2]u8 = undefined;
        if (flip_x and flip_y) {
            offset_array = &sprite_offset_flip_xy;
            pixel_strip_order = &sprite_y_pixels_flip_y;
            increment_x = 1;
        } else if (flip_x) {
            offset_array = &sprite_offset_flip_x;
            pixel_strip_order = &sprite_y_pixels_no_flip;
            increment_x = 1;
        } else if (flip_y) {
            offset_array = &sprite_offset_flip_y;
            pixel_strip_order = &sprite_y_pixels_flip_y;
            increment_x = -1;
        } else {
            offset_array = &sprite_offset_no_flip;
            pixel_strip_order = &sprite_y_pixels_no_flip;
            increment_x = -1;
        }

        //std.debug.print("-----------------------------\n", .{});

        // loop through each byte of the sprite
        for (0..64) |i| {
            const byte: u8 = @truncate(i / 8);
            // determine offsets
            offset_x = offset_array[byte][0];
            offset_y = offset_array[byte][1];
            // calculate screen position, based on the offset of the byte within the sprite
            const x: i32 = sprite_x + (offset_x + (increment_x * @as(i32, @intCast(i % 8)))) * scale_factor - 1;
            const y: i32 = sprite_y + (offset_y * scale_factor);

            // calculate pixel values
            const pix0 = ((buffer[i] & 0b00010000) >> 3) | (buffer[i] & 0b00000001);
            const pix1 = ((buffer[i] & 0b00100000) >> 4) | ((buffer[i] & 0b00000010) >> 1);
            const pix2 = ((buffer[i] & 0b01000000) >> 5) | ((buffer[i] & 0b00000100) >> 2);
            const pix3 = ((buffer[i] & 0b10000000) >> 6) | ((buffer[i] & 0b00001000) >> 3);
            // draw vertical strip of pixels
            rl.drawRectangle(x, y + (pixel_strip_order[0] * scale_factor), scale_factor, scale_factor, self.raylib_colors[palette[pix0]]);
            rl.drawRectangle(x, y + (pixel_strip_order[1] * scale_factor), scale_factor, scale_factor, self.raylib_colors[palette[pix1]]);
            rl.drawRectangle(x, y + (pixel_strip_order[2] * scale_factor), scale_factor, scale_factor, self.raylib_colors[palette[pix2]]);
            rl.drawRectangle(x, y + (pixel_strip_order[3] * scale_factor), scale_factor, scale_factor, self.raylib_colors[palette[pix3]]);

            //std.debug.print("({}, {}) pixel {}{}{}{} with colors {d:02}{d:02}{d:02}{d:02}\n", .{ x, y, pix3, pix2, pix1, pix0, palette[pix3], palette[pix2], palette[pix1], palette[pix0] });
        }
    }

    fn unpack4to16(packed_: []const u8, out: []i16) void {
        // out.len must be packed.len * 2
        var oi: usize = 0;
        for (packed_) |b| {
            const low = @as(u8, b & 0x0F);
            const high = @as(u8, (b >> 4) & 0x0F);

            // convert 0..15 -> -8..7 then scale to 16-bit range by << 12
            out[oi] = @as(i16, @as(i16, (@as(i16, low) - 8) * 4096));
            out[oi + 1] = @as(i16, @as(i16, (@as(i16, high) - 8) * 4096));
            oi += 2;
        }
    }

    pub fn playPacked4bit(rom_samples: []const u8) !void {
        // choose sample rate and channels that match original hardware
        const sample_rate = 9600; // adjust as needed
        const channels = 1;
        const bits = 16;

        rl.initAudioDevice();

        const total_samples = rom_samples.len * 2;
        std.debug.print("Total samples to play: {}\n", .{total_samples});
        const samples_buf = try std.heap.page_allocator.alloc(i16, total_samples);
        defer std.heap.page_allocator.free(samples_buf);

        for (samples_buf) |*s| {
            std.debug.print("{x:04}, ", .{s.*});
        }
        std.debug.print("\n", .{});
        unpack4to16(rom_samples, samples_buf);

        // init audio stream: (sampleRate, sampleSizeInBits, channels)
        const stream = try rl.loadAudioStream(sample_rate, bits, channels);
        // update stream with all samples (samplesCount is number of frames/samples)
        rl.updateAudioStream(stream, samples_buf.ptr, @intCast(total_samples));
        rl.playAudioStream(stream);

        // simple blocking wait while stream plays (or integrate into your game loop)
        // You should check stream state or keep calling UpdateAudioStream for longer sounds/loops.
        std.time.sleep(2 * 10 * (10 ^ 9)); // adjust or implement proper feeding/looping

        rl.stopAudioStream(stream);
        rl.unloadAudioStream(stream);
        rl.closeAudioDevice();
        return;
    }

    pub fn playSine(self: *Pacman, freq: f32, duration_s: u32) !void {
        const sample_rate: u32 = 44100;
        const channels: u32 = 1;
        const bits: u32 = 16;
        const total_samples: usize = @intCast(sample_rate * duration_s);

        const allocator = std.heap.page_allocator;
        const buf = try allocator.alloc(i16, total_samples);
        defer allocator.free(buf);

        const two_pi = 2.0 * std.math.pi;
        for (buf, 0..) |*sample_ptr, i| {
            const t = i / @as(f32, sample_rate);
            const v = std.math.sin(two_pi * freq * t); // -1.0 .. 1.0
            const s = v * 32767.0; // scale to 16-bit signed range
            // convert float to integer sample (may need small cast adjustment depending on Zig version)
            sample_ptr.* = @as(i16, @intCast(s));
        }

        rl.initAudioDevice();
        const stream = try rl.loadAudioStream(@intCast(sample_rate), @intCast(bits), @intCast(channels));
        rl.updateAudioStream(stream, buf.ptr, @intCast(total_samples));
        rl.playAudioStream(stream);

        // simple blocking wait while the sample plays
        std.time.sleep(std.time.seconds(duration_s));

        rl.stopAudioStream(stream);
        rl.unloadAudioStream(stream);
        rl.closeAudioDevice();
    }
};

// to do: sound, load samples, figure how to play them

pub fn main() !void {
    const rom_path = "resources/roms";
    var pm = Pacman{};
    if (pm.init(rom_path) == 0) {
        std.debug.print(("Not initialized, exiting"), .{});
    }

    for (0..32) |i| {
        std.debug.print("{x:02} ", .{pm.ram[Pacman.samples_rom_start + i]});
        if (i % 8 == 7) {
            std.debug.print("\n", .{});
        }
    }

    rl.initWindow(Pacman.screen_width, Pacman.screen_height, "Pac-man");
    defer rl.closeWindow(); // Close window and OpenGL context
    rl.setTargetFPS(60); // Set our game to run at 60 frames-per-second

    // for (0..0x400) |_i| {
    //     const i = @as(u16, @truncate(_i));
    //     pm.ram[Pacman.video_ram_start + i] = @truncate(i % 255);
    //     pm.ram[Pacman.palette_ram_start + i] = 1;
    // }
    for (0..2) |_| {
        try pm.playSine(440.0, 1);
    }
    while (!rl.windowShouldClose()) { // Detect window close button or ESC key
        // try Pacman.playPacked4bit(pm.ram[Pacman.samples_rom_start .. Pacman.samples_rom_start + 32]);

        pm.cpu.execute_instruction(pm.ram[0..], true);

        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(.black);

        // for (0..8) |i_| {
        //     const i = @as(u8, @truncate(i_));
        //     for (0..8) |j_| {
        //         const j = @as(u8, @truncate(j_));
        //         const x: u16 = @as(u16, @intCast(i)) * 16 * @as(u16, @intCast(scale_factor));
        //         const y: u16 = @as(u16, @intCast(j)) * 16 * @as(u16, @intCast(scale_factor));
        //         pm.drawSprite(i * 8 + j, x, y, 1);
        //         std.debug.print("Drawing sprite {}\n", .{i * 8 + j});
        //     }
        // }
        // pm.drawSprite(0, 0, 0, 1, false, false);
        // pm.drawSprite(0, 16 * scale_factor * 1, 0, 1, true, false);
        // pm.drawSprite(0, 16 * scale_factor * 2, 0, 1, false, true);
        // pm.drawSprite(0, 16 * scale_factor * 3, 0, 1, true, true);

        //pm.drawSprite(0, 0, 0, 1);
        // pm.drawSprite(3, 170, 150, 1);
        // pm.drawSprite(4, 230, 170, 1);

        pm.drawTileMap();
        break;
    }
}

test "init" {
    const rom_path = "resources/roms";
    var si = Pacman{};
    if (si.init(rom_path) == 0) {
        std.debug.print(("Not initialized, exiting"), .{});
    }

    try std.testing.expect(si.ram[0] == 0xF3 and si.ram[1] == 0x3E);
    try std.testing.expect(si.ram[0x1000] == 0xAF and si.ram[0x1001] == 0x32);
    try std.testing.expect(si.ram[0x2000] == 0xFD and si.ram[0x2001] == 0x7E);
    try std.testing.expect(si.ram[0x3000] == 0x21 and si.ram[0x3001] == 0x00);
}
