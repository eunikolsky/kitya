.PHONY:
ghci:
	stack ghci --package={hxt,split,directory,filepath,text,css-text,time,process} kitya.hs

.PHONY:
ghcid:
	ghcid -c 'stack ghci --package={hxt,split,directory,filepath,text,css-text,time,process} kitya.hs'

.PHONY:
test:
	./test.hs

.PHONY:
testd:
	ghcid -c 'stack ghci --package={base,hxt,split,directory,filepath,text,css-text,time,process,hspec} --ghc-options=-hide-all-packages test.hs' -T main
