// Import standard library modules
const std: type = @import("std");
// Import lexer and token types from root module
const Lexer: type = @import("root").Lexer;
const Token: type = Lexer.Token;
const TokenType: type = Lexer.TokenType;
// Import AST module from root
const ast: type = @import("root").Ast;

// Define parser error types
const ParserError: type = error{
    UnexpectedToken,
    Generic,
};

/// Parser structure for building Abstract Syntax Tree (AST)
/// Contains state for parsing tokens into AST nodes
pub const Parser: type = struct {
    allocator: std.mem.Allocator, // Memory allocator for AST nodes
    tokens: []const Token, // Token stream to parse
    current: usize = 0, // Current position in token stream
    nodes: std.ArrayList(*ast.Node), // List of created AST nodes for cleanup

    /// Initialize a new Parser instance
    /// @param allocator Memory allocator for AST nodes
    /// @param tokens Token stream to parse
    /// @return Parser instance or error if allocation fails
    pub fn init(allocator: std.mem.Allocator, tokens: []const Token) !Parser {
        return .{
            .allocator = allocator,
            .tokens = tokens,
            .nodes = std.ArrayList(*ast.Node).empty,
        };
    }

    /// Clean up parser resources
    /// @param self Parser instance
    pub fn deinit(self: *Parser) void {
        self.nodes.deinit(self.allocator);
    }

    /// Parse the entire token stream into a program AST node
    /// @param self Parser instance
    /// @return Pointer to program AST node or error
    pub fn parse(self: *Parser) anyerror!*ast.Node {
        // Initialize containers for different AST node types
        var imports = std.ArrayList(*ast.Node).empty;
        var functions = std.ArrayList(*ast.Node).empty;
        var structs = std.ArrayList(*ast.Node).empty;

        // Process all tokens until end of file
        while (!self.isAtEnd()) {
            if (self.match(.import_kw)) {
                try imports.append(self.allocator, try self.parseImport());
            } else if (self.match(.struct_kw)) {
                try structs.append(self.allocator, try self.parseStruct());
            } else if (self.check(.fn_kw)) {
                try functions.append(self.allocator, try self.parseFunction());
            } else {
                _ = self.advance();
            }
        }

        // Create program node with all parsed components
        const program_node = try self.createNode(ast.Node{
            .type = .program,
            .data = .{
                .program = .{
                    .imports = imports,
                    .functions = functions,
                    .structs = structs,
                },
            },
        });

        return program_node;
    }

    /// Parse an import declaration
    /// @param self Parser instance
    /// @return Pointer to import AST node or error
    fn parseImport(self: *Parser) anyerror!*ast.Node {
        // The lexer already emitted an import_kw token whose lexeme holds the path.
        // Since parse() consumed import_kw (via match), we take the previous token.
        const import_token = self.previous();
        const raw_path = import_token.lexeme;
        // Remove surrounding quotes if present (keeps <...> intact)
        const path = std.mem.trim(u8, raw_path, "\"");

        return try self.createNode(ast.Node{
            .type = .import_decl,
            .data = .{
                .import_decl = .{
                    .path = path,
                },
            },
        });
    }

    /// Parse a function declaration
    /// @param self Parser instance
    /// @return Pointer to function AST node or error
    fn parseFunction(self: *Parser) anyerror!*ast.Node {
        _ = try self.consume(.fn_kw, "Expected 'fn'");
        const name = try self.consume(.identifier, "Expected function name");

        _ = try self.consume(.l_paren, "Expected '('");
        var params = std.ArrayList(ast.Parameter).empty;

        // Parse function parameters
        if (!self.check(.r_paren)) {
            while (true) {
                const param_name = try self.consume(.identifier, "Expected parameter name");
                _ = try self.consume(.colon, "Expected ':'");
                const param_type = try self.parseType();

                try params.append(self.allocator, ast.Parameter{
                    .name = param_name.lexeme,
                    .param_type = param_type,
                });

                if (!self.match(.comma)) {
                    break;
                }
            }
        }

        _ = try self.consume(.r_paren, "Expected ')'");

        // Parse return type (defaults to void if not specified)
        const return_type = if (self.match(.identifier))
            try self.parseTypeFromToken(self.previous())
        else
            ast.Type{ .name = "void" };

        _ = try self.consume(.l_brace, "Expected '{'");
        var body = std.ArrayList(*ast.Node).empty;

        // Parse function body statements
        while (!self.check(.r_brace) and !self.isAtEnd()) {
            try body.append(self.allocator, try self.parseStatement());
        }

        _ = try self.consume(.r_brace, "Expected '}'");

        return try self.createNode(ast.Node{
            .type = .function_decl,
            .data = .{
                .function_decl = .{
                    .name = name.lexeme,
                    .params = params,
                    .return_type = return_type,
                    .body = body,
                },
            },
        });
    }

    /// Parse a struct declaration
    /// @param self Parser instance
    /// @return Pointer to struct AST node or error
    fn parseStruct(self: *Parser) anyerror!*ast.Node {
        _ = try self.consume(.struct_kw, "Expected 'struct'");
        const name = try self.consume(.identifier, "Expected struct name");
        _ = try self.consume(.l_brace, "Expected '{'");

        var fields = std.ArrayList(ast.StructField).empty;

        // Parse struct fields
        while (!self.check(.r_brace) and !self.isAtEnd()) {
            const field_name = try self.consume(.identifier, "Expected field name");
            _ = try self.consume(.colon, "Expected ':'");
            const field_type = try self.parseType();
            _ = try self.consume(.semicolon, "Expected ';'");

            try fields.append(self.allocator, ast.StructField{
                .name = field_name.lexeme,
                .field_type = field_type,
            });
        }

        _ = try self.consume(.r_brace, "Expected '}'");

        return try self.createNode(ast.Node{
            .type = .struct_decl,
            .data = .{
                .struct_decl = .{
                    .name = name.lexeme,
                    .fields = fields,
                },
            },
        });
    }

    /// Parse a statement (return, variable declaration, or expression)
    /// @param self Parser instance
    /// @return Pointer to statement AST node or error
    fn parseStatement(self: *Parser) anyerror!*ast.Node {
        if (self.match(.return_kw)) {
            return try self.parseReturn();
        }
        if (self.match(.let_kw)) {
            return try self.parseVariableDecl();
        }

        return try self.parseExpressionStatement();
    }

    /// Parse an expression statement
    /// @param self Parser instance
    /// @return Pointer to expression AST node or error
    fn parseExpressionStatement(self: *Parser) anyerror!*ast.Node {
        const expr = try self.parseExpression();
        _ = try self.consume(.semicolon, "Expected ';' after expression");
        return expr;
    }

    /// Parse a return statement
    /// @param self Parser instance
    /// @return Pointer to return AST node or error
    fn parseReturn(self: *Parser) anyerror!*ast.Node {
        var value: ?*ast.Node = null;

        if (!self.check(.semicolon)) {
            value = try self.parseExpression();
        }

        _ = try self.consume(.semicolon, "Expected ';'");

        return try self.createNode(ast.Node{
            .type = .return_stmt,
            .data = .{
                .return_stmt = .{
                    .value = value,
                },
            },
        });
    }

    /// Parse a variable declaration
    /// @param self Parser instance
    /// @return Pointer to variable declaration AST node or error
    fn parseVariableDecl(self: *Parser) anyerror!*ast.Node {
        const name = try self.consume(.identifier, "Expected variable name");
        const var_type = if (self.match(.colon))
            try self.parseType()
        else
            null;

        _ = try self.consume(.eq, "Expected '='");
        const value = try self.parseExpression();
        _ = try self.consume(.semicolon, "Expected ';'");

        return try self.createNode(ast.Node{
            .type = .variable_decl,
            .data = .{
                .variable_decl = .{
                    .name = name.lexeme,
                    .var_type = var_type,
                    .value = value,
                },
            },
        });
    }

    /// Parse an expression (starting point for expression parsing)
    /// @param self Parser instance
    /// @return Pointer to expression AST node or error
    fn parseExpression(self: *Parser) anyerror!*ast.Node {
        return try self.parseAssignment();
    }

    /// Parse an assignment expression
    /// @param self Parser instance
    /// @return Pointer to assignment AST node or error
    fn parseAssignment(self: *Parser) anyerror!*ast.Node {
        const expr = try self.parseEquality();

        if (self.match(.eq)) {
            const value = try self.parseAssignment();
            return try self.createNode(ast.Node{
                .type = .binary_expr,
                .data = .{
                    .binary_expr = .{
                        .left = expr,
                        .operator = "=",
                        .right = value,
                    },
                },
            });
        }

        return expr;
    }

    /// Parse an equality expression (== or !=)
    /// @param self Parser instance
    /// @return Pointer to equality AST node or error
    fn parseEquality(self: *Parser) anyerror!*ast.Node {
        var expr = try self.parseComparison();

        while (self.match(.eq_eq) or self.match(.not_eq)) {
            const operator = self.previous().lexeme;
            const right = try self.parseComparison();
            expr = try self.createNode(ast.Node{
                .type = .binary_expr,
                .data = .{
                    .binary_expr = .{
                        .left = expr,
                        .operator = operator,
                        .right = right,
                    },
                },
            });
        }

        return expr;
    }

    /// Parse a comparison expression (< or >)
    /// @param self Parser instance
    /// @return Pointer to comparison AST node or error
    fn parseComparison(self: *Parser) anyerror!*ast.Node {
        var expr = try self.parseTerm();

        while (self.match(.lt) or self.match(.gt)) {
            const operator = self.previous().lexeme;
            const right = try self.parseTerm();
            expr = try self.createNode(ast.Node{
                .type = .binary_expr,
                .data = .{
                    .binary_expr = .{
                        .left = expr,
                        .operator = operator,
                        .right = right,
                    },
                },
            });
        }

        return expr;
    }

    /// Parse a term expression (+ or -)
    /// @param self Parser instance
    /// @return Pointer to term AST node or error
    fn parseTerm(self: *Parser) anyerror!*ast.Node {
        var expr = try self.parseFactor();

        while (self.match(.plus) or self.match(.minus)) {
            const operator = self.previous().lexeme;
            const right = try self.parseFactor();
            expr = try self.createNode(ast.Node{
                .type = .binary_expr,
                .data = .{
                    .binary_expr = .{
                        .left = expr,
                        .operator = operator,
                        .right = right,
                    },
                },
            });
        }

        return expr;
    }

    /// Parse a factor expression (* or /)
    /// @param self Parser instance
    /// @return Pointer to factor AST node or error
    fn parseFactor(self: *Parser) anyerror!*ast.Node {
        var expr = try self.parseUnary();

        while (self.match(.star) or self.match(.slash)) {
            const operator = self.previous().lexeme;
            const right = try self.parseUnary();
            expr = try self.createNode(ast.Node{
                .type = .binary_expr,
                .data = .{
                    .binary_expr = .{
                        .left = expr,
                        .operator = operator,
                        .right = right,
                    },
                },
            });
        }

        return expr;
    }

    /// Parse a unary expression (- or !)
    /// @param self Parser instance
    /// @return Pointer to unary AST node or error
    fn parseUnary(self: *Parser) anyerror!*ast.Node {
        if (self.match(.minus) or self.match(.not_eq)) {
            const operator = self.previous().lexeme;
            const right = try self.parseUnary();
            return try self.createNode(ast.Node{
                .type = .binary_expr,
                .data = .{
                    .binary_expr = .{
                        .left = undefined, // Will be handled in codegen
                        .operator = operator,
                        .right = right,
                    },
                },
            });
        }

        return try self.parsePrimary();
    }

    /// Parse a primary expression (literals, identifiers, function calls, member access, or grouped expressions)
    /// @param self Parser instance
    /// @return Pointer to primary AST node or error
    fn parsePrimary(self: *Parser) anyerror!*ast.Node {
        if (self.match(.int_literal)) {
            return try self.createNode(ast.Node{
                .type = .literal_expr,
                .data = .{
                    .literal = .{
                        .value = self.previous().lexeme,
                        .literal_type = .int_type,
                    },
                },
            });
        } else if (self.match(.float_literal)) {
            return try self.createNode(ast.Node{
                .type = .literal_expr,
                .data = .{
                    .literal = .{
                        .value = self.previous().lexeme,
                        .literal_type = .float_type,
                    },
                },
            });
        } else if (self.match(.string_literal)) {
            return try self.createNode(ast.Node{
                .type = .literal_expr,
                .data = .{
                    .literal = .{
                        .value = self.previous().lexeme,
                        .literal_type = .string_type,
                    },
                },
            });
        } else if (self.match(.bool_literal)) {
            return try self.createNode(ast.Node{
                .type = .literal_expr,
                .data = .{
                    .literal = .{
                        .value = self.previous().lexeme,
                        .literal_type = .bool_type,
                    },
                },
            });
        }

        // Parse identifiers, function calls, or member access
        if (self.match(.identifier)) {
            const name = self.previous().lexeme;

            if (self.match(.l_paren)) {
                // Function call
                var args = std.ArrayList(*ast.Node).empty;

                if (!self.check(.r_paren)) {
                    while (true) {
                        const expr = try self.parseExpression();
                        try args.append(self.allocator, expr);
                        if (!self.match(.comma)) {
                            break;
                        }
                    }
                }

                _ = try self.consume(.r_paren, "Expected ')'");

                const callee = try self.createNode(ast.Node{
                    .type = .identifier_expr,
                    .data = .{
                        .identifier = .{
                            .name = name,
                        },
                    },
                });

                return try self.createNode(ast.Node{
                    .type = .call_expr,
                    .data = .{
                        .call_expr = .{
                            .callee = callee,
                            .args = args,
                        },
                    },
                });
            } else if (self.match(.dot)) {
                // Member access
                const member = try self.consume(.identifier, "Expected member name");
                const object = try self.createNode(ast.Node{
                    .type = .identifier_expr,
                    .data = .{
                        .identifier = .{
                            .name = name,
                        },
                    },
                });

                return try self.createNode(ast.Node{
                    .type = .member_access,
                    .data = .{
                        .member_access = .{
                            .object = object,
                            .member = member.lexeme,
                        },
                    },
                });
            } else {
                // Simple identifier
                return try self.createNode(ast.Node{
                    .type = .identifier_expr,
                    .data = .{
                        .identifier = .{
                            .name = name,
                        },
                    },
                });
            }
        }

        // Parse grouped expressions
        if (self.match(.l_paren)) {
            const expr = try self.parseExpression();
            _ = try self.consume(.r_paren, "Expected ')'");
            return expr;
        }

        self.err("Expected expression");
        return error.UnexpectedToken;
    }

    /// Parse a type from current token
    /// @param self Parser instance
    /// @return Type structure or error
    fn parseType(self: *Parser) anyerror!ast.Type {
        const type_token = try self.consume(.identifier, "Expected type");
        return self.parseTypeFromToken(type_token);
    }

    /// Parse a type from a specific token
    /// @param self Parser instance
    /// @param token Token containing type information
    /// @return Type structure or error
    fn parseTypeFromToken(self: *Parser, token: Token) anyerror!ast.Type {
        const type_name = token.lexeme;
        var is_pointer = false;

        if (self.match(.star)) {
            is_pointer = true;
        }

        return ast.Type{
            .name = type_name,
            .is_pointer = is_pointer,
        };
    }

    /// Create an AST node and track it for cleanup
    /// @param self Parser instance
    /// @param node AST node to create
    /// @return Pointer to created AST node or error
    fn createNode(self: *Parser, node: ast.Node) anyerror!*ast.Node {
        const node_ptr = try self.allocator.create(ast.Node);
        node_ptr.* = node;
        try self.nodes.append(self.allocator, node_ptr);
        return node_ptr;
    }

    /// Check if current token matches expected type and advance if it does
    /// @param self Parser instance
    /// @param token_type Expected token type
    /// @return True if token matched and was consumed, false otherwise
    fn match(self: *Parser, token_type: TokenType) bool {
        if (self.check(token_type)) {
            _ = self.advance();
            return true;
        }
        return false;
    }

    /// Check if current token matches expected type without advancing
    /// @param self Parser instance
    /// @param token_type Expected token type
    /// @return True if token matches, false otherwise
    fn check(self: *Parser, token_type: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().type == token_type;
    }

    /// Advance to the next token
    /// @param self Parser instance
    /// @return Previous token
    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    /// Consume a token of expected type or error
    /// @param self Parser instance
    /// @param token_type Expected token type
    /// @param message Error message if token doesn't match
    /// @return Consumed token or error
    fn consume(self: *Parser, token_type: TokenType, message: []const u8) !Token {
        if (self.check(token_type)) {
            return self.advance();
        }
        std.log.err("[Line {}] Error: {s}", .{ self.peek().line, message });
        return error.UnexpectedToken;
    }

    /// Check if at end of token stream
    /// @param self Parser instance
    /// @return True if at end, false otherwise
    fn isAtEnd(self: *Parser) bool {
        if (self.current >= self.tokens.len) {
            return true;
        }
        return self.peek().type == .eof;
    }

    /// Get current token without advancing
    /// @param self Parser instance
    /// @return Current token
    fn peek(self: *Parser) Token {
        if (self.current >= self.tokens.len) {
            return self.tokens[self.tokens.len - 1];
        }
        return self.tokens[self.current];
    }

    /// Get previous token
    /// @param self Parser instance
    /// @return Previous token
    fn previous(self: *Parser) Token {
        if (self.current == 0) {
            return self.tokens[0];
        }
        return self.tokens[self.current - 1];
    }

    /// Report an error at current token position
    /// @param self Parser instance
    /// @param message Error message
    fn err(self: *Parser, message: []const u8) void {
        const token = self.peek();
        std.log.err("[Line {}] Error: {s}", .{ token.line, message });
    }
};
