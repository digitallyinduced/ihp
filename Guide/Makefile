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

all: $(HTML_FILES) bootstrap.css instantclick.js

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
