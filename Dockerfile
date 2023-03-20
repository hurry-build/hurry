FROM ubuntu:jammy-20230308 AS runner

# Install runtime dependencies.
RUN apt-get update && \
  apt-get install --yes --no-install-recommends \
  # We install libnuma to work around a GHC bug. See also:
  #
  # - https://github.com/compiler-explorer/compiler-explorer/issues/3418#issuecomment-1056102097
  # - https://gitlab.haskell.org/ghc/ghc/-/issues/15444#note_388256
  #
  # We can remove this once we upgrade to GHC >= 9.2.2
  libnuma1=2.0.14-3ubuntu2 libnuma-dev=2.0.14-3ubuntu2 \
  \
  && rm -rf /var/lib/apt/lists/*

# Create non-root user.
RUN adduser --uid 1000 hurry

# Create runtime file structure.
RUN mkdir -p /var/lib/hurry/cache && chown --recursive hurry:hurry /var/lib/hurry

# Drop in to non-root user.
WORKDIR /home/hurry
USER hurry

FROM haskell:9.0.2-buster AS builder

# Build application dependencies.
RUN cabal update

COPY hurry.lock hurry.lock
COPY hurry.cabal hurry.cabal
COPY cabal.project cabal.project

RUN cabal build --dependencies-only

# Build application.
COPY cmd cmd
COPY src src

RUN cabal build && cp $(cabal list-bin hurry-api) /tmp/hurry-api

FROM runner

COPY --from=builder /tmp/hurry-api /usr/local/bin/hurry-api

CMD ["/usr/local/bin/hurry-api"]
