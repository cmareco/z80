const std = @import("std");
const rlz = @import("raylib_zig");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const raylib_dep = b.dependency("raylib_zig", .{
        .target = target,
        .optimize = optimize,
    });

    const raylib = raylib_dep.module("raylib");
    const raylib_artifact = raylib_dep.artifact("raylib");

    //web exports are completely separate
    if (target.query.os_tag == .emscripten) {
        const exe_lib = try rlz.emcc.compileForEmscripten(b, "z80", "src/main.zig", target, optimize);

        exe_lib.linkLibrary(raylib_artifact);
        exe_lib.root_module.addImport("raylib", raylib);

        // Note that raylib itself is not actually added to the exe_lib output file, so it also needs to be linked with emscripten.
        const link_step = try rlz.emcc.linkWithEmscripten(b, &[_]*std.Build.Step.Compile{ exe_lib, raylib_artifact });
        //this lets your program access files like "resources/my-image.png":
        link_step.addArg("--emrun");
        link_step.addArg("--embed-file");
        link_step.addArg("resources/");

        b.getInstallStep().dependOn(&link_step.step);
        const run_step = try rlz.emcc.emscriptenRunStep(b);
        run_step.step.dependOn(&link_step.step);
        const run_option = b.step("run", "Run z80");
        run_option.dependOn(&run_step.step);
        return;
    }

    // exercise z80 executable
    const exercise = b.addExecutable(.{ .name = "exercise_z80", .root_source_file = b.path("src/exercise_z80.zig"), .optimize = optimize, .target = target });
    const exercise_step = b.step("exercise", "Build exercise");
    const install_exercise = b.addInstallArtifact(exercise, .{});
    exercise_step.dependOn(&install_exercise.step);

    const exe = b.addExecutable(.{ .name = "80", .root_source_file = b.path("src/z80.zig"), .optimize = optimize, .target = target });

    exe.linkLibrary(raylib_artifact);
    exe.root_module.addImport("raylib", raylib);

    const run_cmd = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run z80");
    run_step.dependOn(&run_cmd.step);

    b.installArtifact(exe);

    // z80 executable
    const z80_exe = b.addExecutable(.{ .name = "z80", .root_source_file = b.path("./src/z80.zig"), .optimize = optimize, .target = target });
    const z80_run_cmd = b.addRunArtifact(z80_exe);
    const z80_run_step = b.step("run_z80", "z80");
    z80_run_step.dependOn(&z80_run_cmd.step);
    b.installArtifact(z80_exe);

    const z80 = b.addExecutable(.{ .name = "z80", .root_source_file = b.path("src/z80.zig"), .optimize = optimize, .target = target });
    const z80_step = b.step("z80", "Build z80");
    const install_z80 = b.addInstallArtifact(z80, .{});
    z80_step.dependOn(&install_z80.step);
}
