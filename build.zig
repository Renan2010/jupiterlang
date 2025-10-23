const std: type = @import("std");
const Builder: type = std.Build;
const fs: type = std.fs;
const builtin: type = std.builtin;

// Entry point for the build script
pub fn build(b: *Builder) void {
    // Configure target platform (arch, OS) based on standard options
    const target: Builder.ResolvedTarget = b.standardTargetOptions(.{});
    // Set optimization mode (Debug, ReleaseSafe, ReleaseFast, ReleaseSmall)
    const optimize: builtin.OptimizeMode = b.standardOptimizeOption(.{});

    // Create a build module for Jupiter compiler
    const jupiter_module: *Builder.Module = b.createModule(.{
        .target = target, // Target platform
        .optimize = optimize, // Optimization level
        .root_source_file = b.path("source/main.zig"), // Entry point of compiler source
    });

    // Compile the module into an executable
    const jupiter_exe: *Builder.Step.Compile = b.addExecutable(.{
        .name = "jupitercc", // Name of the compiler binary
        .root_module = jupiter_module,
        .use_lld = true, // Use LLD linker
        .use_llvm = true, // Enable LLVM backend
    });

    // Create a "run" step for testing the compiler
    const run_cmd: *Builder.Step.Run = b.addRunArtifact(jupiter_exe);
    // Make sure run depends on install step (so binary exists)
    run_cmd.step.dependOn(b.getInstallStep());

    // Forward CLI arguments to the run step if provided
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // Optional: add a named build step to run the compiler
    const run_step = b.step("run", "run jupiterlang compiler");
    run_step.dependOn(&run_cmd.step);

    // Install the compiler binary to a custom directory
    const install_artifact = b.addInstallArtifact(
        jupiter_exe,
        .{
            .dest_dir = .{
                .override = .{
                    .custom = "../jupitercc/bin/", // custom installation folder
                },
            },
        },
    );

    // Ensure the install step depends on this artifact
    b.getInstallStep().dependOn(&install_artifact.step);
}
