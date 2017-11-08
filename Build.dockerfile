FROM haskell:7.10
MAINTAINER Pat Bribin <pat@codeclimate.com>

WORKDIR /home/app

RUN cabal update
RUN cabal install \
  Glob \
  aeson \
  bytestring \
  containers \
  directory \
  haskell-src-exts \
  hlint \
  text

COPY engine.cabal /home/app/engine.cabal
RUN cabal install --dependencies-only

COPY LICENSE /home/app
COPY src /home/app/src
COPY main.hs /home/app/main.hs
RUN cabal configure -fstatic
RUN cabal build
