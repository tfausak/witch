FROM haskell:8.10.2

ARG LOGIN=haskell
ARG UID=1000
ARG GID=$UID

RUN \
  groupadd --gid "$GID" "$LOGIN" && \
  useradd --create-home --gid "$GID" --uid "$UID" "$LOGIN" && \
  chsh --shell "$( which bash )" "$LOGIN"

USER $LOGIN
