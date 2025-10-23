// Import standard library modules
const std: type = @import("std");
const mem: type = std.mem;
const log: type = std.log;
const fs: type = std.fs;
const Io: type = std.Io;
const process: type = std.process;
const heap: type = std.heap;

// Import compiler components
pub const Lexer: type = @import("lexer/lexer.zig");
pub const Ast: type = @import("ast/ast.zig");
pub const Parser: type = @import("parser/parser.zig");
pub const Codegen: type = @import("codegen/codegen.zig");
pub const colors: type = @import("colors/colors.zig").colors;

// Main entry point for the Jupiter compiler
// @return void
// @error !void - Propagates any errors that occur during compilation
pub fn main() !void {
    // Initialize memory allocator for dynamic memory management
    const allocator: mem.Allocator = heap.page_allocator;

    // Parse command line arguments
    const args: [][:0]u8 = try process.argsAlloc(allocator);
    defer process.argsFree(allocator, args);

    // Validate input: ensure at least one source file is provided
    if (args.len < 2) {
        std.log.err(colors.red ++ "no input files" ++ colors.reset, .{});
        std.debug.print(colors.white ++ "Usage: " ++ colors.reset ++ colors.green ++ "{s} [file.jlang]" ++ colors.reset ++ "\n", .{args[0]});
        return;
    }

    // Extract input file path from command line arguments
    const input_file: [:0]u8 = args[1];

    // Set I/O limit for file reading (1MB)
    const limit: type = Io.Limit;

    // Read source code from file into memory
    const source_code: []u8 = try fs.cwd().readFileAlloc(input_file, allocator, limit.limited64(1024 * 1024));
    defer allocator.free(source_code);

    // Initialize lexer with source code
    var lexer: Lexer.Lexer = Lexer.Lexer.init(allocator, source_code);
    defer lexer.deinit();

    // Perform lexical analysis: convert source code into tokens
    try lexer.tokenize();
    const lexer_tokens: []Lexer.Token = lexer.tokens.items;

    // Initialize parser with tokens from lexer
    var parser: Parser.Parser = try Parser.Parser.init(allocator, lexer_tokens);
    defer parser.deinit();

    // Perform syntactic analysis: build Abstract Syntax Tree from tokens
    const ast: *Ast.Node = try parser.parse();

    // Initialize code generator
    var codegen: Codegen.CodeGen = Codegen.CodeGen.init(allocator);

    // Generate C++ code from AST
    const cpp_code: []const u8 = try codegen.generate(ast);

    // Set default output file name
    var output_name: []const u8 = "a.out.cpp";

    // Check for custom output file name in command line arguments
    if (args.len > 3 and mem.eql(u8, args[2], "-o")) {
        output_name = args[3];
    }

    // Create output file
    const file: fs.File = try fs.cwd().createFile(output_name, .{});
    defer file.close();

    // Write generated C++ code to output file
    try file.writeAll(cpp_code);

    // Log successful compilation
    log.info(colors.green ++ "File {s} generated successfully!" ++ colors.reset, .{output_name});
}
