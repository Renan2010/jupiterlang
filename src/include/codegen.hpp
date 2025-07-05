// codegen.hpp

// --- Header Guards ---
// These are standard header guards. They prevent the contents of this file
// from being included more than once in a single compilation unit (.cpp file).
// This avoids "multiple definition" errors during compilation.
#ifndef JUPITERCC_CODEGEN_HPP
#define JUPITERCC_CODEGEN_HPP

// --- Includes ---

// Include the lexer header. This is necessary because this file uses the
// `Token` type, which is defined in "lexer.hpp". This establishes a
// dependency: the code generator needs to know what a token is.
#include "lexer.hpp"

// Include standard C++ library headers.
#include <string> // Required for using the `std::string` type.
#include <vector> // Required for using the `std::vector` container.

// --- Function Declaration ---

/**
 * @brief Generates C++ source code from a vector of tokens.
 *
 * This function takes the sequence of tokens produced by the lexer and
 * transforms it into a string of equivalent C++ code. This is the "code
 * generation" phase of the compiler.
 *
 * @param tokens A constant reference to a vector of `Token` objects.
 *               - It's a `vector` because it represents an ordered sequence of
 * tokens.
 *               - It's passed by reference (`&`) to avoid making an expensive
 * copy of all the tokens.
 *               - It's `const` because the code generation process should not
 * modify the original tokens.
 *
 * @return A `std::string` containing the complete, generated C++ source code.
 */
std::string generate_cpp(const std::vector<Token> &tokens);

#endif // JUPITERCC_CODEGEN_HPP