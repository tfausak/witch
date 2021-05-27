FROM i386/ubuntu:20.04

RUN apt-get update && apt-get install --assume-yes curl gcc git libgmp-dev libtinfo-dev libtinfo5 make sudo zlib1g-dev

ARG GHCUP_VERSION=0.1.14.1
RUN curl --output /usr/local/bin/ghcup "https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/i386-linux-ghcup-$GHCUP_VERSION"
RUN chmod +x /usr/local/bin/ghcup

ARG USER=haskell
RUN useradd --create-home --shell "$( which bash )" "$USER"
RUN echo "$USER ALL=(ALL) NOPASSWD: ALL" | tee "/etc/sudoers.d/$USER"
USER "$USER"
ENV PATH="/home/$USER/.cabal/bin:/home/$USER/.ghcup/bin:$PATH"

ARG GHC_VERSION=9.0.1
RUN ghcup install ghc "$GHC_VERSION"
RUN ghcup set ghc "$GHC_VERSION"

ARG CABAL_VERSION=3.4.0.0
RUN ghcup install cabal "$CABAL_VERSION"
RUN ghcup set cabal "$CABAL_VERSION"
