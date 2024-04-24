.PHONY: run ghcid build

run:
	@./map.hs

ghcid:
	@ghcid -c "stack ghci $$( rg -F -m1 'stack script' az.hs | sed -nE 's/.*stack script (.*)/\1/p' ) map.hs"

build:
	@stack script --no-run --optimize $$( rg -F -m1 'stack script' az.hs | sed -nE 's/.*stack script (.*)/\1/p' ) map.hs
