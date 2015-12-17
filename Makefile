prof:
	stack exec cheapskate-terminal -- +RTS -sstderr -prof -RTS syntax-very-long.md

build-prof:
	stack build --enable-library-profiling --enable-executable-profiling --ghc-options -fprof-auto --ghc-options -caf-all
