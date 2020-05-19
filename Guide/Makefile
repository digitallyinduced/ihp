.PHONY: watch all clean

HTML_FILES=
HTML_FILES+= recipes.html
HTML_FILES+= architecture.html
HTML_FILES+= scripts.html
HTML_FILES+= your-first-project.html
HTML_FIELS+= installation.html
HTML_FILES+= intro.html
HTML_FILES+= database.html
HTML_FILES+= debugging.html
HTML_FILES+= querybuilder.html
HTML_FILES+= view.html
HTML_FILES+= form.html

all: $(HTML_FILES)

%.html: %.markdown layout.html
	mmark -i $< -o $@ --template layout.html --ext-skylighting --ext-ghc-highlighter --ext-punctuation --ext-toc 2-6

watch: all
	open $$PWD/intro.html
	ls *.markdown layout.html | entr make

clean:
	rm $(HTML_FILES)