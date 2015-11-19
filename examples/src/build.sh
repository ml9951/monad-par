
#!/bin/bash

source environment

for v in tl2-linked fine-linked
do
    if [ $v == "orig" ]; then
	PAR_MONAD="$ORIG_PAR_MONAD/monad-par/monad-par.cabal \
		 $ORIG_PAR_MONAD/monad-par-extras/monad-par-extras.cabal"
	flags=
    elif [ $v == "fine" ]; then
	PAR_MONAD="$STM_PAR_MONAD/monad-par/monad-par.cabal \
		 $STM_PAR_MONAD/monad-par-extras/monad-par-extras.cabal"
	flags=
    elif [ $v == "chase-lev" ]; then
	PAR_MONAD="$ORIG_PAR_MONAD/monad-par/monad-par.cabal \
		 $ORIG_PAR_MONAD/monad-par-extras/monad-par-extras.cabal"
	flags=-fchaselev
	extras="cabal install --with-ghc=$GHC $ATOMIC_OPS_DIR"
    elif [ $v == "tl2" ]; then 
	PAR_MONAD="$STM_PAR_MONAD/monad-par/monad-par.cabal \
		 $STM_PAR_MONAD/monad-par-extras/monad-par-extras.cabal"
	flags=-ftl2
    elif [ $v == "tl2-linked" ]; then
	PAR_MONAD="$STM_PAR_MONAD/monad-par/monad-par.cabal \
		 $STM_PAR_MONAD/monad-par-extras/monad-par-extras.cabal"
	flags="-ftl2 -flinkeddeque"
    elif [ $v == "fine-linked" ]; then
	PAR_MONAD="$STM_PAR_MONAD/monad-par/monad-par.cabal \
		 $STM_PAR_MONAD/monad-par-extras/monad-par-extras.cabal"
	flags="-flinkeddeque"
    else
	echo "version not recognized!"
	exit 1
    fi

    echo "cabal sandbox init --sandbox=.cabal-sandbox-$v"
    echo "$extras"
    echo "cabal install --with-ghc=$GHC $GHC_DIR/ghc/libraries/stm"
    echo "cabal install --with-ghc=$GHC $GHC_DIR/ghc/libraries/pastm"
    echo "cabal install --with-ghc=$GHC $PAR_MONAD $flags -j6"
    
    cabal sandbox init --sandbox=.cabal-sandbox-$v
    $extras
    cabal install --with-ghc=$GHC $GHC_DIR/ghc/libraries/stm
    cabal install --with-ghc=$GHC $GHC_DIR/ghc/libraries/pastm
    cabal install --with-ghc=$GHC $PAR_MONAD $flags -j6
done




