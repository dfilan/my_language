Making an interpreter in Haskell for a simple programming language. The language was originally designed for a foundations of mathematics class (see Chapter 8 part C of notes), but then I added various features to practice programming.

Note that all data are natural numbers, so 'subtraction' is actually the monus operator: x monus y = x - y if x > y and 0 otherwise.

To compile the interpreter, have haskell installed, and then run `ghc --make interpreter.hs`. Then, to evaluate a program on some arguments, in the directory where the compiled interpreter is run
- `./interpreter path_to_program [1,2,3]` on Mac or Linux
- `interpreter.exe path_to_program [1,2,3]` on Windows,
where `path_to_program` is the file path to your program, and `[1,2,3]` should be replaced by your list of arguments, formatted in the same way (open square bracket, natural numbers separated by commas and no spaces, close square bracket).

Changes to the language as defined in the notes:
- Routines can take other routines as arguments. Routine arguments are separated by a semi-colon from natural-valued arguments, and the definition of a routine does not need to include its local variables. The natural-valued arguments of a routine when called can be any expressions, so `ApplyThree(MyFunc(x+y),3*(y+z),4;AddThree)` is a valid routine call.
- The insides of routines consist of various statements:
  - Return statements are written `return ...;`, where `...` is any valid expression, and do what you'd expect. 
  - If statements are written `if (e) {...};`, where `e` is any valid expression, and `...` is a sequence of statements separated by semicolons. If the expression evaluates to something other than zero, the inner block is evaluated, otherwise it isn't. There can also be an else block, which executes if the expression does evaluate to zero: this is written `if (e) {...} else {...};`.
  - While statements are written `while (e) {...};`, where `e` is any valid expression, and `...` is a sequence of statements separated by semicolons. The inner block loops as long as `e` doesn't evaluate to zero.
  - Assignments are written `x := ...;`, where `...` is the value that will be assigned to `x`. This can be any arbitrary arithmetic expression involving natural numbers, previously defined variables, and routines defined anywhere in the program.
  - Routines can also be defined locally inside routines.
- Every statement in a block must end with a semicolon.
- All binary operators are right-associative, so `3 - 5 + 7` = `3 - (5 + 7)` = 0.
- You can use parentheses in arithmetic expressions like `(3 - 5) + 7`, which evaluates to 7.
- Routine and variable names must consist entirely of alphanumeric characters. Routine names must begin with an upper-case letter, and variable names must begin with a lower-case letter
