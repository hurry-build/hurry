# hurry

:warning: **This is experimental, work-in-progress software.** Expect breaking changes until 1.0. Use in production at your own risk. :warning:

----

Hurry is a build tool for Haskell applications that's fast and easy to use. It integrates with the `cabal` tool and provides additional functionality for common developer tasks and build caching needs.

Commands include:

- `hurry lock` generates a `hurry.lock` lockfile based on the project's currently installed dependencies. This lock file should be committed and shared among developers to enable reproducible builds, build caching, and Docker layer caching.
- `hurry restore` installs dependencies from the `hurry.lock` file.
- `hurry verify` checks whether the solved dependency installation plan from running `cabal build` conflicts with the project's `hurry.lock`.



<!--

TODO:
- `hurry restore` installs dependencies from the `hurry.lock` file, loading them from a shared dependency cache when available.
- `hurry cache` uploads the project's currently built dependencies to a shared dependency cache.
- `hurry build` does the right thing, and you can build the deps from just a lockfile (for Docker layer caching)

-->
## Installation

```
git clone https://github.com/elldritch/hurry
cd hurry
cabal install
```

<!--

TODO:
Use `cabal install hurry` to install the latest version of Hurry.

-->


<!--

TODO: Usage section

- Usage in raw Cabal builds
- Usage in GitHub Actions and using GHA caching
- Usage in Docker and Dockerfile and layer cahcing

-->
