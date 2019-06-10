.PHONY: watch

getting-started.html: getting-started.markdown index.html
	mmark -i getting-started.markdown -o getting-started.html --template index.html --ext-skylighting --ext-ghc-highlighter --ext-punctuation --ext-toc 1-6

watch:
	ls * | entr make

