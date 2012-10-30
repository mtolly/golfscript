GolfScript interpreter in Haskell
=================================

By Michael Tolly
----------------

More or less completed features:

* All the original implementation's built-in functions and features, except for
  Ruby string interpolation. The original Ruby code has lots of behavior which
  was probably not intended by the developer; I've tried to replicate this
  behavior where practical (that is, where it did not enormously complicate the
  implementation). There are still a few kinks to work out; more testing needed.

* The entire interpreter, including top-level state, data values, and built-in
  functions, are parameterized by a monad. You can fill this with Identity or IO
  (or any other useful monad) to choose between a pure and impure interpreter;
  only the impure interpreter has explicit print commands.

* Just as non-strict as Haskell. This allows more legal programs than the Ruby
  implementation; for example, you can work with infinite stacks.

Planned features:

* Customizable error/warning handling, for situations such as: popping from an
  empty stack, invalid type arguments given to a function, writing to whitespace
  variable names, and reading from undefined variables (other than whitespace).
  This would be massively helpful when debugging a program (also, helpful for me
  in debugging the interpreter's behavior!).
