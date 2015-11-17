
STM = 

GHC = /home/ml9951/repos/ghc-wip/bin/ghc
PKG_DB = -package-db=.cabal-sandbox-$(STM)/x86_64-linux-ghc-7.11.20151106-packages.conf.d

.PHONY:	fib cabal nbody mandel

fib:
	$(GHC) $(PKG_DB) -fforce-recomp -threaded -DOLDTYPES -rtsopts -O2 parfib/parfib-monad.hs -o fib-$(STM)

nbody:
	$(GHC) $(PKG_DB) -fforce-recomp -threaded -rtsopts -O2 -o nbody-$(STM) nbody/nbody.hs

mandel:
	$(GHC) $(PKG_DB) -fforce-recomp -threaded -rtsopts -O2 -o mandel-$(STM) mandel/mandel.hs

cabal:
	cabal sandbox init --sandbox=.cabal-sandbox-tl2
	cabal install --with-ghc=$(GHC) ../../monad-par/monad-par.cabal -ftl2



