Making an interpreter in Haskell for a simple programming language. The language was originally designed for a foundations of mathematics class (see Chapter 8 part C of notes), but then I added various features to practice programming.

To compile the interpreter, have haskell installed, and then run `ghc --make interpreter.hs`. Then, to evaluate a program on some arguments, in the directory where the compiled interpreter is run
- `./interpreter path_to_program [1,2,3]` on Mac or Linux
- `interpreter.exe path_to_program [1,2,3]` on Windows,
where `path_to_program` is the file path to your program, and `[1,2,3]` should be replaced by your list of arguments, formatted in the same way (open square bracket, natural numbers separated by commas and no spaces, close square bracket).

At the top level, the language is a sequence of routine definitions, one of which should be called `main`. `main` should take only natural numbers and return a natural number. Routine definitions look like `myFunc := \(arg1: type1, arg2: type2) {...} : returnType`, where there can be any number of arguments, all declared types can be either `Nat` or a function type, and the `...` inside the braces is a sequence of statements, one of which should be a return statement.

There are four kinds of statements:
- Return statements are written `return ...;`, where `...` is any valid expression, and do what you'd expect.
- If statements are written `if (e) {...};`, where `e` is any valid expression of type `Nat`, and `...` is a sequence of statements separated by semicolons. If the expression evaluates to something other than zero, the inner block is evaluated, otherwise it isn't. There can also be an else block, which executes if the expression does evaluate to zero: this is written `if (e) {...} else {...};`.
- While statements are written `while (e) {...};`, where `e` is any valid expression of type `Nat`, and `...` is a sequence of statements separated by semicolons. The inner block loops as long as `e` doesn't evaluate to zero.
- Assignments are written `x := ...;`, where `...` is the value that will be assigned to `x`. This can be any arbitrary arithmetic expression involving natural numbers, previously defined variables, and routines defined anywhere in the program. `x`, the variable name, must consist entirely of alphanumeric characters, and start with a lower-case alphabetic character.

All statements must end with a semicolon, except optionally the last statement in a block.

Arithmetic in this language works slightly differently than one might expect:
- All numbers are natural numbers, so 'subtraction' is actually the monus operator: x monus y = x - y if x > y and 0 otherwise.
- All binary operators are right-associative, so `3 - 5 + 7` = `3 - (5 + 7)` = 0.
- You can use parentheses in arithmetic expressions like `(3 - 5) + 7`, which evaluates to 7.
