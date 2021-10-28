.PHONY: watch all clean

HTML_FILES=
HTML_FILES+= recipes.html
HTML_FILES+= architecture.html
HTML_FILES+= scripts.html
HTML_FILES+= your-first-project.html
HTML_FILES+= installation.html
HTML_FILES+= index.html
HTML_FILES+= database.html
HTML_FILES+= debugging.html
HTML_FILES+= testing.html
HTML_FILES+= querybuilder.html
HTML_FILES+= view.html
HTML_FILES+= form.html
HTML_FILES+= hsx.html
HTML_FILES+= validation.html
HTML_FILES+= session.html
HTML_FILES+= naming-conventions.html
HTML_FILES+= controller.html
HTML_FILES+= routing.html
HTML_FILES+= authentication.html
HTML_FILES+= authorization.html
HTML_FILES+= relationships.html
HTML_FILES+= updating.html
HTML_FILES+= package-management.html
HTML_FILES+= editors.html
HTML_FILES+= deployment.html
HTML_FILES+= auto-refresh.html
HTML_FILES+= mail.html
HTML_FILES+= design-goals.html
HTML_FILES+= troubleshooting.html
HTML_FILES+= modal.html
HTML_FILES+= database-migrations.html
HTML_FILES+= tailwindcss.html
HTML_FILES+= npm.html
HTML_FILES+= elm.html
HTML_FILES+= jobs.html
HTML_FILES+= logging.html
HTML_FILES+= purescript.html
HTML_FILES+= websockets.html
HTML_FILES+= helpful-tips.html
HTML_FILES+= assets.html
HTML_FILES+= file-storage.html
HTML_FILES+= server-side-components.html
HTML_FILES+= oauth.html
HTML_FILES+= ihp-pro.html
HTML_FILES+= examples.html
HTML_FILES+= stripe.html

all: $(HTML_FILES) bootstrap.css instantclick.js search.js search.css

%.html: %.markdown layout.html
	mmark -i $< -o $@ --template layout.html --ext-skylighting --ext-ghc-highlighter --ext-punctuation --ext-toc 2-6

watch: all
	@echo "Server is starting at: http://localhost:3000/index.html"
	@nix-shell -p haskellPackages.wai-app-static --command "warp" &
	@ls *.markdown layout.html | entr make

clean:
	rm $(HTML_FILES)

bootstrap.css:
	curl https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/css/bootstrap.min.css --output $@

instantclick.js:
	curl http://instantclick.io/v3.1.0/instantclick.min.js --output $@

search/node_modules:
	cd search && npm install

search.js: search/node_modules
	./search/node_modules/.bin/esbuild search/search.jsx --bundle --minify-whitespace --outfile=search.js

search.css: search/node_modules
	./search/node_modules/.bin/esbuild search/node_modules/@docsearch/css/dist/style.css --minify-whitespace --outfile=search.css

guide.tar.gz: all
	tar -czvf guide.tar.gz $(HTML_FILES) bootstrap.css instantclick.js search.js search.css