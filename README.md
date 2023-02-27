# hurry

:rotating_light: **This software is not yet usable.** :rotating_light:

----

Hurry is a build tool for Haskell applications that's fast and easy to use. It integrates with the `cabal` tool and provides additional functionality for common developer tasks and build caching needs.

<!-- Commands include:

- `hurry lock` generates a `hurry.lock` lockfile based on the project's currently installed dependencies. This lock file should be committed and shared among developers to enable reproducible builds, build caching, and Docker layer caching.
- `hurry save` uploads the project's currently built dependencies to a shared dependency cache.
- `hurry restore` installs dependencies from the `hurry.lock` file, loading them from a shared dependency cache when available.
- `hurry verify` checks whether the solved dependency installation plan from running `cabal build` conflicts with the project's `hurry.lock`.

-->

## Use cases

<!-- TODO: Provide example snippets -->

### Dockerfile layer caching

Using only `cabal`, Dockerfile layer caching is very difficult. While you could run `cabal build --dependencies-only`, doing so requires `COPY`ing your `PROJECT_NAME.cabal` file. Unfortunately, Cabal files can change for many reasons (e.g. adding new modules), so the Dockerfile layer cache will often be needlessly invalidated.

```dockerfile
COPY hurry.cabal # This will be invalidated whenever Cabal file changes (e.g. new modules)
RUN cabal build --dependencies-only
COPY .
RUN cabal build
```

With Hurry, you can simply `COPY` your `hurry.lock` and then run `hurry restore` to load compiled dependencies from cache. This layer will never be invalidated unless your actual dependencies have changed, and you can check whether they've changed using `hurry verify`.

```dockerfile
COPY hurry.lock # This only invalidates if dependencies change
RUN hurry restore
COPY .
RUN cabal build
```

## Installation

<!-- TODO: Use `cabal install hurry` to install the latest version of Hurry. -->
```
git clone https://github.com/elldritch/hurry
cd hurry
cabal install
```
