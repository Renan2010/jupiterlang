// lexer.hpp

// --- Header Guards ---
// Standard C++ header guards to prevent this file from being included multiple
// times in the same compilation unit. This avoids redefinition errors.
#ifndef JUPITERCC_LEXER_HPP
#define JUPITERCC_LEXER_HPP

// --- Includes ---
// Include necessary standard C++ library headers.
#include <string> // For using the std::string class.
#include <vector> // For using the std::vector container.

// --- Data Structure Definition ---

/**
 * @struct Token
 * @brief Represents a single, meaningful unit of code.
 *
 * The lexer scans the raw source code and breaks it down into a sequence of
 * these Token objects. Each token has a type (for classification) and a value
 * (the actual text from the source).
 */
struct Token {
  // `type`: A string that categorizes the token.
  // Examples: "KEYWORD", "IDENTIFIER", "NUMBER", "OPERATOR", "STRING_LITERAL".
  std::string type;

  // `value`: The actual string sequence from the source code that this token
  // represents. For a number token, this might be "123". For an identifier, it
  // could be "my_variable".
  std::string value;
};

// --- Function Declaration ---

/**
 * @brief Performs lexical analysis on a source code string.
 *
 * This function is the main entry point for the lexer. It takes the entire
 * source code as a single string and processes it to produce a list (vector)
 * of tokens.
 *
 * @param source A constant reference to the string containing the source code.
 *               - It's a `string` because it holds the raw text.
 *               - It's passed by reference (`&`) for efficiency, to avoid
 * copying the entire source code.
 *               - It's `const` because the lexer should only read the source,
 * not modify it.
 *
 * @return A `std::vector<Token>` which is an ordered sequence of the tokens
 *         found in the source code. This vector will be used by the next
 *         stage of the compiler (e.g., the parser or code generator).
 */
std::vector<Token> lex(const std::string &source);

#endif // JUPITERCC_LEXER_HPP