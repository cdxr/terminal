### Terminal

`terminal` is a Haskell package for creating interactive command-line
interfaces. It provides an mtl-style MonadTerm typeclass that lifts terminal
operations through a monad transformer stack. The concrete type, TermT, is
implemented with the `haskeline` package.

This package is experimental, and the author recommends that users make use of
`haskeline` instead.
