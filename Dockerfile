FROM haskell:7.10
MAINTAINER Pat Bribin <pat@codeclimate.com>

RUN useradd -u 9000 -d /home/app -m app
USER app
WORKDIR /home/app

RUN cabal update
RUN cabal install \
  Glob \
  aeson \
  aeson \
  bytestring \
  containers \
  directory \
  haskell-src-exts \
  hlint \
  text

COPY engine.cabal /home/app/engine.cabal
RUN cabal install --dependencies-only

COPY src /home/app/src
COPY main.hs /home/app/main.hs
COPY LICENSE /home/app/LICENSE
RUN cabal build

VOLUME /code
WORKDIR /code

ENTRYPOINT ["/home/app/dist/build/engine/engine"]
