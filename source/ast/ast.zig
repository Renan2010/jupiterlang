// Import standard library modules
const std: type = @import("std");
// Import TokenType from root module's Lexer
const TokenType: type = @import("root").Lexer.TokenType;

/// Enumeration of AST node types
/// Represents all possible node types in the Abstract Syntax Tree
pub const NodeType: type = enum {
    program, // Root node of the program
    function_decl, // Function declaration
    variable_decl, // Variable declaration
    return_stmt, // Return statement
    call_expr, // Function call expression
    binary_expr, // Binary operation expression
    literal_expr, // Literal value expression
    identifier_expr, // Identifier reference
    import_decl, // Import declaration
    if_stmt, // Conditional statement
    while_loop, // While loop statement
    struct_decl, // Struct declaration
    member_access, // Member access expression
};

/// Type representation for variables, parameters, and return values
/// Contains type name and pointer information
pub const Type: type = struct {
    name: []const u8, // Name of the type
    is_pointer: bool = false, // Whether the type is a pointer
};

/// Abstract Syntax Tree node structure
/// Represents a node in the AST with type-specific data
pub const Node: type = struct {
    type: NodeType, // Type of the node
    data: Data, // Type-specific data for the node

    /// Union of all possible node data structures
    /// Each variant corresponds to a node type in NodeType enum
    pub const Data: type = union {
        program: struct {
            imports: std.ArrayList(*Node), // List of import declarations
            functions: std.ArrayList(*Node), // List of function declarations
            structs: std.ArrayList(*Node), // List of struct declarations
        },
        function_decl: struct {
            name: []const u8, // Function name
            params: std.ArrayList(Parameter), // Function parameters
            return_type: Type, // Return type
            body: std.ArrayList(*Node), // Function body statements
        },
        variable_decl: struct {
            name: []const u8, // Variable name
            var_type: ?Type, // Variable type (null for type inference)
            value: ?*Node, // Initial value (null if not initialized)
        },
        return_stmt: struct {
            value: ?*Node, // Return value (null for void return)
        },
        call_expr: struct {
            callee: *Node, // Function being called
            args: std.ArrayList(*Node), // Function arguments
        },
        binary_expr: struct {
            left: *Node, // Left operand
            operator: []const u8, // Binary operator
            right: *Node, // Right operand
        },
        literal: struct {
            value: []const u8, // Literal value as string
            literal_type: TokenType, // Type of the literal
        },
        identifier: struct {
            name: []const u8, // Identifier name
        },
        import_decl: struct {
            path: []const u8, // Import path
        },
        if_stmt: struct {
            condition: *Node, // Condition expression
            then_branch: std.ArrayList(*Node), // Statements to execute if condition is true
            else_branch: std.ArrayList(*Node), // Statements to execute if condition is false
        },
        while_loop: struct {
            condition: *Node, // Loop condition
            body: std.ArrayList(*Node), // Loop body statements
        },
        struct_decl: struct {
            name: []const u8, // Struct name
            fields: std.ArrayList(StructField), // Struct fields
        },
        member_access: struct {
            object: *Node, // Object being accessed
            member: []const u8, // Member name
        },
    };
};

/// Function parameter representation
/// Contains parameter name and type information
pub const Parameter: type = struct {
    name: []const u8, // Parameter name
    param_type: Type, // Parameter type
};

/// Struct field representation
/// Contains field name and type information
pub const StructField: type = struct {
    name: []const u8, // Field name
    field_type: Type, // Field type
};
