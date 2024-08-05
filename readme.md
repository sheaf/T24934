Reproducer for [GHC bug #24934](https://gitlab.haskell.org/ghc/ghc/-/issues/24934).

To reproduce, run the following:

```
ghc -O1 T24934a.hs T24934b.hs Main.hs
./Main
```

```
Main: internal error: Oops!  Entered absent arg Arg: $d~
Type: RepDim (F i 2) ~ 2
In module `T24934b'
    Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug
```
