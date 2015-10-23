This repository holds initial library code for type-indexed type representations in Haskell, based on [ideas from the wiki](https://ghc.haskell.org/trac/ghc/wiki/DistributedHaskell).
There is a [wiki page](https://ghc.haskell.org/trac/ghc/wiki/TypeableT) about this implementation, containing a fair few questions!

There are two branches of interest, `master`, and `extensible`.
The difference is that `extensible` has [Cloud Haskell](https://haskell-distributed.github.io/)-like user-extensible static pointer tables, which have to be passed around manually, whilst `master` has just one global SPT defined in the library.
Thus, the API of `master` is roughly what I envision a final API will look like (extensibility being given by GHC magic - the `static` keyword), but `extensible` may be easier to experiment with now.
