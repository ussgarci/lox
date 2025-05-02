# lox

A Haskell implementation of the `lox` language from [Crafting Interpreters](https://craftinginterpreters.com/). 

This repo is monitored by [:bee: Beeminder](https://www.beeminder.com/ussgarci/learn-haskell) to keep me accountable for my learning goals.

#### Building
```
cabal build
cabal run lox-exe <absolute path to lox file>
```
#### Testing
```
cabal build lox-test
cabal run lox-test
```
