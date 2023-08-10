.PHONY:
ghci:
	stack ghci --package={hxt,split,directory,filepath,text,css-text,time,process} kitya.hs

.PHONY:
ghcid:
	ghcid -c 'stack ghci --package={hxt,split,directory,filepath,text,css-text,time,process} kitya.hs'
