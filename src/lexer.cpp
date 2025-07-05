// lexer.cpp

// Include the corresponding header file. This gives us the definition for the
// `Token` struct and the declaration for the `lex` function.
#include "include/lexer.hpp"

// Include the stringstream library to easily parse the source string.
#include <sstream>

/**
 * @brief Implements the lexical analysis logic.
 *
 * This function takes a source code string and splits it into a sequence of
 * tokens based on simple, whitespace-delimited rules.
 *
 * @param source The string containing the entire source code.
 * @return A vector of Token objects representing the source code.
 */
std::vector<Token> lex(const std::string &source) {
  // This vector will store the tokens as they are identified.
  std::vector<Token> tokens;

  // Create an "input string stream" from the source code. This allows us to
  // treat the string `source` like a file or standard input, making it easy
  // to extract "words" separated by whitespace.
  std::istringstream iss(source);

  // A temporary string to hold each word read from the stream.
  std::string word;

  // The main lexing loop.
  // The expression `iss >> word` attempts to extract the next
  // whitespace-separated word from the stream. It returns true if successful,
  // and false if the end of the stream is reached.
  while (iss >> word) {
    // --- Keyword and Token Classification ---
    // Check if the extracted word matches any known keywords.

    if (word == "import") {
      // The "import" keyword is expected to be followed by a value (e.g., a
      // library name). Immediately read the *next* word from the stream.
      iss >> word;
      // Create an "IMPORT" token with the library name as its value.
      tokens.push_back({"IMPORT", word});
    } else if (word == "fn") {
      // The "fn" keyword is expected to be followed by a function name.
      // Immediately read the *next* word.
      iss >> word;
      // Create a "FN" token with the function name as its value.
      tokens.push_back({"FN", word});
    } else if (word == "print") {
      // The "print" keyword does not have an associated value in this language.
      // It's a command on its own.
      // Create a "PRINT" token with an empty value.
      tokens.push_back({"PRINT", ""});
    } else {
      // If the word doesn't match any of the known keywords, classify it as
      // "UNKNOWN". This is a simple way to handle unrecognized text, which
      // could be an error or an unimplemented feature (like variable names).
      tokens.push_back({"UNKNOWN", word});
    }
  }

  // Return the completed list of tokens.
  return tokens;
}