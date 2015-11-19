
STM = 

GHC = 
PKG_DB = -package-db=.cabal-sandbox-$(STM)/x86_64-linux-ghc-7.11.20151106-packages.conf.d

.PHONY:	fib cabal nbody mandel queens minimax blackscholes sumeuler

blackscholes:
	$(GHC) $(PKG_DB) -fforce-recomp -threaded -rtsopts -O2 -o blackscholes-$(STM) blackscholes/blackscholes.hs

nbody:
	$(GHC) $(PKG_DB) -fforce-recomp -threaded -rtsopts -O2 -o nbody-$(STM) nbody/nbody.hs

mandel:
	$(GHC) $(PKG_DB) -fforce-recomp -threaded -rtsopts -O2 -o mandel-$(STM) mandel/mandel.hs

queens:
	$(GHC) $(PKG_DB) -fforce-recomp -threaded -rtsopts -O2 -o queens-$(STM) queens/queens.hs

minimax:	
	$(GHC) $(PKG_DB) -fforce-recomp -threaded -rtsopts -O2 -o minimax-$(STM) minimax/*.hs

sumeuler:
	$(GHC) $(PKG_DB) -fforce-recomp -threaded -rtsopts -O2 -o sumeuler-$(STM) sumeuler/*.hs

cabal:
	cabal sandbox init --sandbox=.cabal-sandbox-tl2
	cabal install --with-ghc=$(GHC) ../../monad-par/monad-par.cabal -ftl2



