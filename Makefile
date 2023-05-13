.PHONY: ghcid_test

ghcid_test:
	stack exec ghcid -- \
		--command="stack ghci --ghc-options='-j -fno-write-ide-info' jpeg-parser:lib jpeg-parser:test:spec" \
		--test="main"

format:
	find * -name '*.hs' | xargs -P0 fourmolu --no-cabal -i
	hlint .

ghcid:
	stack exec ghcid -- \
		--command="stack ghci --ghc-options='-j -fno-write-ide-info -fno-code' jpeg-parser:lib jpeg-parser:exe:jpeg-parser" \
