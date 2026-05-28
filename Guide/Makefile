.PHONY: all clean

# Every *.markdown file becomes a *.html page (rendered through layout.html).
MARKDOWN_FILES := $(wildcard *.markdown)
HTML_FILES := $(MARKDOWN_FILES:.markdown=.html)

all: $(HTML_FILES) bootstrap.css instantclick.js search.js search.css

%.html: %.markdown layout.html
	mmark -i $< -o $@ --template layout.html --ext-skylighting --ext-ghc-highlighter --ext-punctuation --ext-toc 2-6

bootstrap.css:
	curl -fsSL https://cdn.jsdelivr.net/npm/bootstrap@4.5.0/dist/css/bootstrap.min.css --output $@

instantclick.js:
	curl -fsSL https://cdn.jsdelivr.net/npm/instantclick@3.1.0-2/instantclick.min.js --output $@

node_modules:
	npm install

search.js: node_modules search.jsx
	./node_modules/.bin/esbuild search.jsx --bundle --minify-whitespace --outfile=search.js

search.css: node_modules
	./node_modules/.bin/esbuild node_modules/@docsearch/css/dist/style.css --minify-whitespace --outfile=search.css

guide.tar.gz: all
	tar -czf guide.tar.gz *.html *.css *.js images

clean:
	rm -f $(HTML_FILES) bootstrap.css instantclick.js search.js search.css guide.tar.gz
