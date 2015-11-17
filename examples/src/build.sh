#!/bin/bash

source environment

for v in orig fine tl2
do
    if [ $v == "orig" ]; then
	PAR_MONAD="$ORIG_PAR_MONAD/monad-par/monad-par.cabal \
		 $ORIG_PAR_MONAD/monad-par-extras/monad-par-extras.cabal"
	flags=
    elif [ $v == "fine" ]; then
	PAR_MONAD="$STM_PAR_MONAD/monad-par/monad-par.cabal \
		 $STM_PAR_MONAD/monad-par-extras/monad-par-extras.cabal"
	flags=
    else
	PAR_MONAD="$STM_PAR_MONAD/monad-par/monad-par.cabal \
		 $STM_PAR_MONAD/monad-par-extras/monad-par-extras.cabal"
	flags=-ftl2
    fi

    echo "cabal sandbox init --sandbox=.cabal-sandbox-$v"
    echo "cabal install --with-ghc=$GHC $GHC_DIR/ghc/libraries/stm"
    echo "cabal install --with-ghc=$GHC $GHC_DIR/ghc/libraries/pastm"
    echo "cabal install --with-ghc=$GHC $PAR_MONAD $flags -j6"
    
    cabal sandbox init --sandbox=.cabal-sandbox-$v
    cabal install --with-ghc=$GHC $GHC_DIR/ghc/libraries/stm
    cabal install --with-ghc=$GHC $GHC_DIR/ghc/libraries/pastm
    cabal install --with-ghc=$GHC $PAR_MONAD $flags -j6
done




