.PHONY:
ghci:
	stack ghci --package={hxt,split,directory,filepath,text,css-text,time,process,optparse-applicative} kitya.hs

.PHONY:
ghcid:
	ghcid -c 'stack ghci --package={hxt,split,directory,filepath,text,css-text,time,process,optparse-applicative} kitya.hs'

.PHONY:
test:
	./test.hs

.PHONY:
testd:
	ghcid -c 'stack ghci --package={base,hxt,split,directory,filepath,text,css-text,time,process,optparse-applicative,hspec,neat-interpolation} --ghc-options=-hide-all-packages test.hs' -T main

.PHONY:
build:
	@# have to comment the module declaration, otherwise the executable is not created
	@# https://stackoverflow.com/questions/67543484/how-to-compile-a-haskell-stack-script-into-an-executable#comment129664591_67543484
	sed -i '' 's/^module Kitya where/--&/' kitya.hs
	stack script --optimize --no-run --resolver lts-19.31 --package base,hxt,split,directory,filepath,text,css-text,time,process,optparse-applicative --ghc-options=-hide-all-packages kitya.hs
