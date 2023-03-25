.PHONY: clean

tarball.tar.gz: default.nix bin
	mkdir tarball
	cp default.nix tarball
	cp -R bin tarball
	tar -czvf tarball.tar.gz tarball
	rm -rf tarball

clean:
	rm -rf tarball tarball.tar.gz
