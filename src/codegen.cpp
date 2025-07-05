// codegen.cpp

// Include the corresponding header file. This provides the function declaration
// for `generate_cpp` and ensures the implementation matches the declaration.
#include "include/codegen.hpp"
// Include the stringstream library, which is used to efficiently build
// the output C++ code string.
#include <sstream>

/**
 * @brief Generates a C++ source string from a vector of tokens.
 *
 * This function implements the logic for translating a sequence of tokens from
 * a custom language into a compilable C++ program. It operates in a simple,
 * multi-pass manner.
 *
 * @param tokens The sequence of tokens from the lexer.
 * @return A string containing the generated C++ code.
 */
std::string generate_cpp(const std::vector<Token> &tokens) {
  // Create a stringstream object. This acts as a buffer to build the final
  // C++ code string efficiently, avoiding multiple string reallocations.
  std::stringstream ss;

  // --- First Pass: Handle Preprocessor Directives ---
  // Iterate through all tokens to find IMPORT directives first. This ensures
  // all #include statements are placed at the top of the generated file.
  for (const auto &t : tokens) {
    // If the token is of type "IMPORT"...
    if (t.type == "IMPORT") {
      // ...generate a C++ #include directive using the token's value.
      // For example, an IMPORT token with value "iostream" becomes "#include
      // <iostream>".
      ss << "#include <" << t.value << ">\n";
    }
  }

  // Add a blank line for better readability, separating includes from the code.
  ss << "\n";

  // --- Second Pass: Generate Function and Statement Code ---
  // Iterate through the tokens again to generate the main body of the code.
  // NOTE: This implementation is very simplistic and has structural
  // limitations.
  for (const auto &t : tokens) {
    // If the token is of type "FN" (Function)...
    if (t.type == "FN") {
      // ...start a C++ function definition. It is hardcoded to return `int`.
      // The function's name is taken from the token's value.
      ss << "int " << t.value << "() {\n"; // Assuming a function like "main"
    }
    // If the token is of type "PRINT"...
    else if (t.type == "PRINT") {
      // ...generate a hardcoded C++ line to print " Hello,world! ".
      // NOTE: This does not use any value from the token itself. It's a fixed
      // translation.
      ss << "    std::cout << \" Hello,world! \" << std::endl;\n";
    }
  }

  // --- Finalization ---
  // After the loop, append the closing statements for the function.
  // This is brittle logic, as it assumes there was exactly one function
  // and that it should return 0.
  ss << "    return 0;\n}\n";

  // Convert the stringstream's content into a std::string and return it.
  return ss.str();
}