# lww-hs

[![Build Status](https://travis-ci.org/jartur/lww-hs.svg?branch=master)](https://travis-ci.org/jartur/lww-hs)

Implementation of LWW-Element-Set (state based) in Haskell & a distributed service built around it.

# Docs

https://jartur.github.io/lww-hs/lww-hs-0.1.0.0/LWWSet.html

# Buildchain

Install [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

On MacOs you can also just `brew install haskell-stack`

# Build

```
stack test
```

# Notes

* QuickCheck testing can be improved by writing an `Arbitrary` instance for the datastructure (e.g. to check for commutativity and associativity).
