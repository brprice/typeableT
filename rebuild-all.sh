#!/bin/bash

set -e

pkgs=(typeableT dynamicT staticptr distributed-closure)

i="${#pkgs[@]}"
while [ "$i" -ne 0 ]; do
	i=$((i-1))
	p="${pkgs[i]}"
	ghc-pkg unregister "$p" || :
done

for p in "${pkgs[@]}"; do
	cd "$p"
	cabal configure
	cabal build
	cabal haddock
	cabal install
	cd ..
done


cd doc

if [[ ! -f Makefile ]]; then
    autoconf
    ./configure
fi
make
