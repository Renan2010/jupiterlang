// main.cpp

// Include custom headers for the compiler's core components.
#include "include/codegen.hpp" // For the code generation phase (transforming tokens into C++ code).
#include "include/lexer.hpp" // For the lexical analysis phase (turning source code into tokens).

// Include standard C++ library headers.
#include <fstream> // For file input/output operations (e.g., reading the source file).
#include <iostream> // For standard input/output streams (e.g., printing errors to std::cerr).
#include <sstream> // For using string streams, an easy way to read a whole file into a string.

// --- Terminal Color Macros ---
// These macros use ANSI escape codes to format text printed to the terminal.
// This makes error messages more readable.
#define BOLD_WHITE(text)                                                       \
  "\033[1;37m" text "\033[0m"                 // Macro for bold white text.
#define RED(text) "\033[1;31m" text "\033[0m" // Macro for red text.

// The main entry point of the program.
// argc: The number of command-line arguments.
// argv: An array of strings containing the command-line arguments.
int main(int argc, char **argv) {
  // --- Argument Validation ---
  // Check if the user provided an input file.
  // The program name is always the first argument (argv[0]), so we need at
  // least two (argc < 2).
  if (argc < 2) {
    // If no input file is given, print a fatal error message and exit.
    std::cerr << BOLD_WHITE("jupitercc: ") << RED("fatal error:")
              << " no input files\n";
    std::cerr << "compilation terminated.\n";
    return 1; // Return a non-zero exit code to indicate an error.
  }

  // --- File Reading ---
  // Try to open the file specified by the first command-line argument
  // (argv[1]).
  std::ifstream file(argv[1]);
  if (!file) {
    // If the file could not be opened (e.g., it doesn't exist), print an error
    // and exit.
    std::cerr << BOLD_WHITE("jupitercc: ") << RED("fatal error: ")
              << "failed to open file\n";
    return 1; // Return a non-zero exit code.
  }

  // Read the entire content of the file into a string.
  // This is a common and efficient C++ idiom.
  std::stringstream buffer;
  buffer << file.rdbuf(); // Read the file's buffer into the string stream.
  std::string source =
      buffer.str(); // Convert the stream's content to a std::string.

  // --- Compilation Pipeline ---
  // 1. Lexical Analysis: Convert the raw source code string into a sequence of
  // tokens.
  auto tokens = lex(source);

  // 2. Code Generation: Take the tokens and generate the target C++ code as a
  // string.
  auto cpp_code = generate_cpp(tokens);

  // --- Output Generation ---
  // Create an output file named "a.out.cpp" to store the generated code.
  std::ofstream output("a.out.cpp");
  output << cpp_code; // Write the generated C++ code into the file.
  output.close();     // Close the file stream.

  // Indicate that the program completed successfully.
  return 0;
}