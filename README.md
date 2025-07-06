<div align="center">
  <h1>Júpiterlang</h1>
  <p><strong>A memory-safe programming language that speaks modern C++.</strong></p>
  
  <p>
    <img src="https://img.shields.io/badge/language-C%2B%2B23-blue.svg" alt="Language C++23"/>
    <a href="https://github.com/Renan2010/jupiterlang/blob/main/LICENSE"><img src="https://img.shields.io/badge/license-MIT-green.svg" alt="License MIT"/></a>
  </p>
</div>

---

Júpiterlang is a modern systems programming language designed to combine the performance of C++ with guaranteed memory safety. It achieves this by transpiling its high-level, clean syntax directly into high-performance, readable C++23.

The core principle is **safety by abstraction**: Júpiterlang automatically generates C++ code that leverages smart pointers (`std::unique_ptr`, `std::shared_ptr`), freeing the developer from manual memory management and eliminating entire classes of bugs like dangling pointers and memory leaks.

## 🤔 Why Júpiterlang?

| Feature | Description |
| :--- | :--- |
| 🔐 **Memory-Safe by Design** | No more `new`/`delete`. The compiler manages memory for you using C++'s robust smart pointers, preventing memory leaks and use-after-free errors. |
| 🚀 **High-Performance C++ Core** | Get the raw speed of C++23 without the overhead of a garbage collector. Ideal for performance-critical applications like game engines, servers, and OS development. |
| ✨ **Clean & Modern Syntax** | With an expressive syntax inspired by Rust and C++, Júpiterlang focuses on readability and an enjoyable developer experience, reducing boilerplate code. |
| 🤝 **Seamless C++ Interoperability** | Since the output is C++ code, you can easily integrate Júpiterlang components into any existing C++ project and leverage the vast ecosystem of C++ libraries. |
| 🌱 **Self-Hosting Ambition** | The ultimate goal is for the `jupitercc` compiler to be written in Júpiterlang itself, proving the language's power and maturity. |

---

## 🏁 Getting Started

Ready to write your first Júpiterlang program? Follow these steps to build the compiler.

### Prerequisites

*   **CMake** (`>=4.0`)
*   A **C++23 compliant compiler** (e.g., GCC 12+, Clang 16+)

### Build Instructions

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/Renan2010/jupiterlang.git
    cd jupiterlang
    ```

2.  **Configure and build the project:**
    ```bash
    cmake -B build
    cd build
    make -j $(nproc)
    ```
    This will create the `jupitercc` executable in the `build/` directory.

### Your First Program

1.  **Write some Júpiterlang code** (e.g., in `examples/hello.jupiter`):
    ```jupiter
    # This is the syntax the current compiler handles
    import iostream

    fn main() {
        print("Hello from Júpiterlang!");
    }
    ```

2.  **Transpile it to C++:**
    ```bash
    ./build/jupitercc ./examples/hello.jupiter
    ```
    This generates a `a.out.cpp` file.

3.  **Compile and run the C++ output:**
    ```bash
    g++ -std=c++23 -o hello a.out.cpp && ./hello
    ```
    **Output:**
    ```
    Hello from Júpiterlang!
    ```

---

## 🗺️ Project Roadmap

Júpiterlang is in its early stages of development. Here is a high-level overview of our goals. Contributions are welcome!

-   [x] Basic Lexer & Parser for simple syntax
-   [x] `import`, `fn main`, and `print` statement transpilation
-   [ ] **Variable Declarations:** `let x = 10;`, `let name = "Jupiter";`
-   [ ] **Core Data Types:** `int`, `string`, `bool`, `float`
-   [ ] **Control Flow:** `if`/`else` statements
-   [ ] **Loops:** `for` and `while` loops
-   [ ] **User-Defined Functions:** With parameters and return values
-   [ ] **Smart Pointer Implementation:** Automatic generation of `std::unique_ptr` for owned types.
-   [ ] **Structs/Classes:** User-defined aggregate types.
-   [ ] **Self-Hosting Compiler:** The grand finale!

---

## 🤝 How to Contribute

We welcome contributions of all kinds! Whether you're fixing a bug, adding a new feature, or improving documentation, your help is greatly appreciated.

1.  **Fork** the repository.
2.  Create a new **branch** (`git checkout -b feature/your-feature-name`).
3.  **Commit** your changes (`git commit -m 'Add some feature'`).
4.  **Push** to the branch (`git push origin feature/your-feature-name`).
5.  Open a **Pull Request**.

Please check the **[Issues](https://github.com/Renan2010/jupiterlang/issues)** tab to find tasks to work on.

---

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
