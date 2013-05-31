# Terminal

`terminal` is a Haskell package for creating interactive command-line interfaces.

It is in early development, and the API is not yet stable.


## Changes

### Version 1.0.0.0

#### MonadTerm
* removed MonadIO superclass constraint
* removed MonadPlus superclass constraint
* removed inputState (and all public Haskeline values)
* added tryInputLine
* added tryInputChar
* added outputStr
