# lox

This a WIP Haskell implementation of the `lox` programming language from the book [Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom.   

Building
--------

```
cabal build
cabal run lox-exe <absolute path to lox file>
```


Testing
-------

```
cabal build lox-test
cabal run lox-test
```