FROM base

RUN apt-get update
RUN apt-get install -y vim git wget
RUN wget http://www.haskell.org/ghc/dist/7.8.2/ghc-7.8.2-x86_64-unknown-linux-deb7.tar.bz2
RUN tar -xvjpf ghc-7.8.2-x86_64-unknown-linux-deb7.tar.bz2
RUN apt-get install -y build-essential
RUN cd ghc-7.8.2; ./configure; make install
RUN cd ~/; wget http://www.haskell.org/cabal/release/cabal-install-1.20.0.2/cabal-install-1.20.0.2.tar.gz
RUN tar -xzf cabal-install-1.20.0.2.tar.gz
RUN apt-get install -y libgmp10 libgmp3-dev
RUN cd cabal-install-1.20.0.2; ./bootstrap.sh
