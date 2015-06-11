FROM haskell:7.10

RUN mkdir /usr/src/app
WORKDIR /usr/src/app

COPY engine.cabal /usr/src/app/engine.cabal
RUN cabal update
RUN cabal install --dependencies-only

COPY main.hs /usr/src/app/main.hs
RUN cabal build

ENTRYPOINT ["/usr/src/app/dist/build/engine/engine"]
