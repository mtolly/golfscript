GolfScript interpreter in Haskell, by Michael Tolly
===================================================

Under development. Planned features:

* Should support all the official implementation's features, except for Ruby
  string interpolation.

* Pure mode and IO mode, where only IO mode would have things like explicit
  print commands. Easy to implement as a monad parameter in the interpreter
  state, filled by either Identity or IO.

* Warnings mode, where warnings could be printed for undefined function
  applications, writing to whitespace variable names, and reading from undefined
  variables (other than whitespace).
