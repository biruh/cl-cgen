> [!CAUTION]
> This project was originally part of a larger project. I am currently working on separating it and making it reusable as an independent module. It has not yet been tested in its standalone form.

# cl-cgen
### Write C in Lisp-like Syntax and generate human readable C code.

This common lisp library allows you to write C programs using a Lisp-like syntax. It parses structured expressions and emits valid, human-readable C source code, enabling a more expressive and macro-friendly way to write C. Ideal for metaprogramming, code generation, and educational purposes.

## Prior art

[c-mera](https://github.com/kiselgra/c-mera) Next-level syntax for C-like languages 

[cl-cpp-generator2](https://github.com/plops/cl-cpp-generator2.git) ...aimed at leveraging the robust macro system of Common Lisp to enhance languages that bear similarity to C or C++

[LISP/c (Lispsy)](https://github.com/eratosthenesia/lispc.git) ... features access to LISP to write C code both implicitly and explicitly.


## Installation

The easiest way is to
**Clone the repository**
   Clone this project into your Quicklisp local projects directory:

   ```bash
   git clone https://github.com/biruh/cl-cgen.git ~/quicklisp/local-projects
   ```

**Load the library in your Lisp environment**
   Start your Lisp REPL and load the library using Quicklisp:

   ```lisp
   (ql:quickload :cl-cgen)
   ```

---

Let me know if you’d like help adding instructions for ASDF or other Lisp implementations!

