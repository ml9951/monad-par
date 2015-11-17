#!/bin/bash

GHC=/home/ml9951/repos/ghc-wip/bin/ghc

for v in orig fine tl2
do
    if [ $v == "orig" ]; then
	PAR_MONAD="/home/ml9951/repos/orig-monad-par/monad-par/monad-par/monad-par.cabal \
		 /home/ml9951/repos/orig-monad-par/monad-par/monad-par-extras/monad-par-extras.cabal"
	flags=
    elif [ $v == "fine" ]; then
	PAR_MONAD="/home/ml9951/repos/stm-monad-par/monad-par/monad-par/monad-par.cabal \
		 /home/ml9951/repos/stm-monad-par/monad-par/monad-par-extras/monad-par-extras.cabal"
	flags=
    else
	PAR_MONAD="/home/ml9951/repos/stm-monad-par/monad-par/monad-par/monad-par.cabal \
		 /home/ml9951/repos/stm-monad-par/monad-par/monad-par-extras/monad-par-extras.cabal"
	flags=-ftl2
    fi

    echo "cabal sandbox init --sandbox=.cabal-sandbox-$v"
    echo "cabal install --with-ghc=$GHC /home/ml9951/repos/ghc-wip/ghc/libraries/stm"
    echo "cabal install --with-ghc=$GHC /home/ml9951/repos/ghc-wip/ghc/libraries/pastm"
    echo "cabal install --with-ghc=$GHC $PAR_MONAD $flags -j6"
    
    cabal sandbox init --sandbox=.cabal-sandbox-$v
    cabal install --with-ghc=$GHC /home/ml9951/repos/ghc-wip/ghc/libraries/stm
    cabal install --with-ghc=$GHC /home/ml9951/repos/ghc-wip/ghc/libraries/pastm
    cabal install --with-ghc=$GHC $PAR_MONAD $flags -j6
done




