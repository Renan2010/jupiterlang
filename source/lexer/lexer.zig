// Import standard library modules
const std: type = @import("std");
const heap: type = std.heap;
const mem: type = std.mem;
const log: type = std.log;

/// Enumeration of token types for lexical analysis
/// Represents all possible token types in the language
pub const TokenType: type = enum {
    // Keywords
    fn_kw, // Function keyword
    import_kw, // Import keyword
    return_kw, // Return keyword
    let_kw, // Variable declaration keyword
    if_kw, // Conditional keyword
    else_kw, // Alternative branch keyword
    while_kw, // Loop keyword
    struct_kw, // Structure definition keyword
    new_kw, // Object instantiation keyword

    // Types
    int_type, // Integer type
    string_type, // String type
    bool_type, // Boolean type
    float_type, // Floating-point type
    void_type, // Void type

    // Literals
    identifier, // Identifier token
    int_literal, // Integer literal
    string_literal, // String literal
    float_literal, // Floating-point literal
    bool_literal, // Boolean literal

    // Operators
    plus, // Addition operator (+)
    minus, // Subtraction operator (-)
    star, // Multiplication operator (*)
    slash, // Division operator (/)
    eq, // Assignment operator (=)
    eq_eq, // Equality operator (==)
    not_eq, // Inequality operator (!=)
    lt, // Less than operator (<)
    gt, // Greater than operator (>)
    arrow, // Arrow operator (->)

    // Punctuation
    l_brace, // Left brace ({)
    r_brace, // Right brace (})
    l_paren, // Left parenthesis (()
    r_paren, // Right parenthesis ())
    l_bracket, // Left bracket ([)
    r_bracket, // Right bracket (])
    semicolon, // Semicolon (;)
    colon, // Colon (:)
    comma, // Comma (,)
    dot, // Dot (.)

    eof, // End of file token
};

/// Token structure representing a lexical unit
/// Contains token type, lexeme, and position information
pub const Token: type = struct {
    type: TokenType, // Type of the token
    lexeme: []const u8, // Raw text of the token
    line: usize, // Line number where token appears
    column: usize, // Column number where token appears
};

/// Lexer for tokenizing source code
/// Converts source text into a stream of tokens
pub const Lexer: type = struct {
    allocator: mem.Allocator, // Memory allocator for tokens
    source: []const u8, // Source code to tokenize
    tokens: std.ArrayList(Token), // List of generated tokens
    start: usize = 0, // Start position of current token
    current: usize = 0, // Current position in source
    line: usize = 1, // Current line number
    column: usize = 1, // Current column number

    /// Initialize a new Lexer instance
    /// @param allocator Memory allocator for tokens
    /// @param source Source code to tokenize
    /// @return Lexer instance
    pub fn init(allocator: mem.Allocator, source: []const u8) Lexer {
        return .{
            .allocator = allocator,
            .source = source,
            .tokens = std.ArrayList(Token).empty,
        };
    }

    /// Scan an identifier or keyword
    /// @param self Lexer instance
    fn scanIdentifier(self: *Lexer) !void {
        while (isAlphaNumeric(self.peek())) {
            _ = self.advance();
        }

        const lexeme: []const u8 = self.source[self.start..self.current];

        if (mem.eql(u8, lexeme, "fn")) {
            try self.addToken(.fn_kw);
        } else if (mem.eql(u8, lexeme, "let")) {
            try self.addToken(.let_kw);
        } else if (mem.eql(u8, lexeme, "return")) {
            try self.addToken(.return_kw);
        } else {
            try self.addToken(.identifier);
        }
    }

    /// Clean up lexer resources
    /// @param self Lexer instance
    pub fn deinit(self: *Lexer) void {
        self.tokens.deinit(self.allocator);
    }

    /// Tokenize the entire source code
    /// @param self Lexer instance
    pub fn tokenize(self: *Lexer) !void {
        while (!self.isAtEnd()) {
            self.start = self.current;
            try self.scanToken();
        }
    }

    /// Advance to the next character in source
    /// @param self Lexer instance
    /// @return Current character before advancing
    fn advance(self: *Lexer) u8 {
        self.current += 1;
        self.column += 1;
        return self.source[self.current - 1];
    }

    /// Scan a string literal
    /// @param self Lexer instance
    fn scanString(self: *Lexer) !void {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            log.err("String not terminated", .{});
            return;
        }

        _ = self.advance();
    }

    /// Peek at current character without advancing
    /// @param self Lexer instance
    /// @return Current character or null if at end
    fn peek(self: *Lexer) u8 {
        return if (self.isAtEnd()) 0 else self.source[self.current];
    }

    /// Scan a single token from source
    /// @param self Lexer instance
    fn scanToken(self: *Lexer) !void {
        const c = self.advance();
        switch (c) {
            ' ', '\r', '\t' => {}, // Skip whitespace
            '\n' => {
                self.line += 1;
                self.column = 1;
            },
            '#' => try self.scanDirective(),
            'a'...'z', 'A'...'Z', '_' => try self.scanIdentifier(),
            '0'...'9' => try self.scanNumber(),
            '"' => try self.scanString(),
            '+' => try self.addToken(.plus),
            '-' => {
                if (self.match('>')) {
                    try self.addToken(.arrow);
                } else {
                    try self.addToken(.minus);
                }
            },
            '*' => try self.addToken(.star),
            '/' => {
                if (self.match('/')) {
                    while (self.peek() != '\n' and !self.isAtEnd()) {
                        _ = self.advance();
                    }
                } else {
                    try self.addToken(.slash);
                }
            },
            '=' => {
                if (self.match('=')) {
                    try self.addToken(.eq_eq);
                } else {
                    try self.addToken(.eq);
                }
            },
            '!' => {
                if (self.match('=')) {
                    try self.addToken(.not_eq);
                }
            },
            '<' => try self.addToken(.lt),
            '>' => try self.addToken(.gt),
            '{' => try self.addToken(.l_brace),
            '}' => try self.addToken(.r_brace),
            '(' => try self.addToken(.l_paren),
            ')' => try self.addToken(.r_paren),
            '[' => try self.addToken(.l_bracket),
            ']' => try self.addToken(.r_bracket),
            ';' => try self.addToken(.semicolon),
            ':' => try self.addToken(.colon),
            ',' => try self.addToken(.comma),
            '.' => try self.addToken(.dot),
            else => {
                std.log.err("Unexpected character: '{c}' at line {}", .{ c, self.line });
            },
        }
    }

    /// Check if at end of source
    /// @param self Lexer instance
    /// @return True if at end, false otherwise
    fn isAtEnd(self: *Lexer) bool {
        return self.current >= self.source.len;
    }

    /// Add a token to the token list
    /// @param self Lexer instance
    /// @param token_type Type of token to add
    fn addToken(self: *Lexer, token_type: TokenType) !void {
        const allocator: mem.Allocator = heap.page_allocator;
        const lexeme: []const u8 = self.source[self.start..self.current];
        try self.tokens.append(
            allocator,
            Token{
                .type = token_type,
                .lexeme = lexeme,
                .line = self.line,
                .column = self.column,
            },
        );
    }

    /// Check if current character matches expected
    /// @param self Lexer instance
    /// @param expected Character to match
    /// @return True if matched, false otherwise
    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd() or self.source[self.current] != expected) {
            return false;
        }
        self.current += 1;
        self.column += 1;
        return true;
    }

    /// Scan a directive (e.g., #import)
    /// @param self Lexer instance
    fn scanDirective(self: *Lexer) !void {
        _ = self.advance();

        const startDirective = self.current;
        while (isAlpha(self.peek())) {
            _ = self.advance();
        }
        const directive = self.source[startDirective..self.current];

        if (mem.eql(u8, directive, "import")) {
            while (self.peek() == ' ' or self.peek() == '\t') {
                _ = self.advance();
            }
            const startPath = self.current;
            while (!self.isAtEnd() and self.peek() != '\n' and self.peek() != ' ' and self.peek() != '\t') {
                _ = self.advance();
            }
            const path = self.source[startPath..self.current];
            try self.tokens.append(self.allocator, Token{
                .type = .import_kw,
                .lexeme = path,
                .line = self.line,
                .column = self.column,
            });
        }
    }

    /// Check if character is alphanumeric
    /// @param c Character to check
    /// @return True if alphanumeric, false otherwise
    fn isAlphaNumeric(c: u8) bool {
        return isAlpha(c) or isDigit(c);
    }

    /// Check if character is alphabetic
    /// @param c Character to check
    /// @return True if alphabetic, false otherwise
    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    /// Check if character is a digit
    /// @param c Character to check
    /// @return True if digit, false otherwise
    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    /// Scan a number literal (integer or float)
    /// @param self Lexer instance
    fn scanNumber(self: *Lexer) !void {
        self.start = self.current - 1;

        while (isDigit(self.peek())) {
            _ = self.advance();
        }
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance();
            while (isDigit(self.peek())) {
                _ = self.advance();
            }
            try self.addToken(.float_literal);
        } else {
            try self.addToken(.int_literal);
        }
    }

    /// Peek at next character without advancing
    /// @param self Lexer instance
    /// @return Next character or null if at end
    fn peekNext(self: *Lexer) u8 {
        return if (self.current + 1 >= self.source.len) 0 else self.source[self.current + 1];
    }
};
