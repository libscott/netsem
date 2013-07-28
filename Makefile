dev:
	cabal-dev install

interact:
	cabal-dev ghci

init-linux:
	vagrant up
	vagrant ssh -c "cd /vagrant
	                sudo apt-get install -y build-essential
					make _init-linux"

build-linux:
	vagrant up
	vagrant ssh -c "cd /vagrant; make _build-linux"

_init-linux:
	sudo apt-get update
	sudo apt-get install -y build-essential
	sudo apt-get install -y haskell-platform
	cabal update

_build-linux:
	cabal install --only-dependencies
	cabal configure --builddir=dist-linux
	cabal build --ghc-options='-static -optl-static -optl-pthread' \
	            --builddir=dist-linux
	echo "All done; binary is at: dist-linux/build"
