default: main.js

# https://github.com/truqu/real-world-elm/blob/2f6f083c631f4461a5b782d51822ae20450d6d2e/elm/Makefile
ELM_FILES = $(shell find . -path ./elm-stuff -prune -o -type f -name '*.elm')

.PHONY: clean clean-deps watch

main.js: $(ELM_FILES)
	elm-make --yes --warn src/Main.elm --output $@

clean-deps:
	rm -rf elm-stuff

clean:
	rm -f *.js
	rm -rf elm-stuff/build-artifacts

watch:
	@make
	@echo Waiting for changes…
	@fswatch -o -0 src | xargs -0 -n1 -I{} make
