# Terminal

`terminal` is a Haskell package for creating interactive command-line interfaces.
It is built on [Haskeline](http://hackage.haskell.org/package/haskeline).

Terminal defines TermT, an abstract monad transformer designed as an alternative
to Haskeline's InputT type. You can think of it as a lightweight wrapper around
InputT with [additional functionality](http://endomaton.blogspot.com/2013/05/terminal-alternative-interface-to.html).

Terminal is in very early development and exports a single module,
[System.Console.Terminal](https://github.com/cdxr/terminal/blob/master/System/Console/Terminal.hs).




# Changes

## Version 1.0.0.0

### MonadTerm
* removed MonadIO superclass constraint
* removed MonadPlus superclass constraint
* removed inputState (and all public Haskeline values)
* added tryInputLine
* added tryInputChar
* added outputStr
