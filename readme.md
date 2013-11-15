# GolfScript interpreter in Haskell

## By Michael Tolly

An alternate implementation of the terse
[GolfScript](http://www.golfscript.com/golfscript/) language. All features
should be supported except for Ruby string interpolation. The language has many
corner cases, so please notify me if you find a difference in behavior from the
original Ruby implementation!

### Usage

    cabal install
    golf prog.gs < stdin.txt
    golf < prog.gs

### Testing

`cabal test` runs a variety of included programs (taken from the
[Anarchy Golf](http://golf.shinh.org/) server) to verify output against the
official Ruby interpreter (make sure `ruby` is in your path). See the `test`
folder for the required format. A test succeeds if both versions succeed with
the exact same stdout, or if both versions exit with a non-success code (on the
Ruby side, this indicates a Ruby exception).
