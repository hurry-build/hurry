https://hackage.haskell.org/package/cabal-plan-0.7.2.3/docs/Cabal-Plan.html#t:Unit
https://cabal.readthedocs.io/en/3.6/cabal-commands.html#cabal-v2-install
https://downloads.haskell.org/ghc/latest/docs/users_guide/packages.html#package-environments
https://github.com/haskell/cabal/issues/6481 from `cabal install --help`

----

do i get a different unit ID if i install using different flags?
- yes, i think so
- see `.cabal/store/ghc-9.0.2/aeson-1.5.4.1-e8ff9f99d71b8343ae6cb687e83b022f0dc26dcc869ba2e7589aa8d6227ec307/cabal-hash.txt`
- see also:
  - `cabal install aeson-1.5.4.1` vs. `cabal install aeson-1.5.4.1 --constraint="attoparsec==0.13.2.4"`

what if i use --constraints to force a different transitive dependency version?

what's the right way to add everything to the .cabal/store while making sure transitive deps solve correctly?
- make a fake project?
- use a fake cabal.project to force transitive version dependencies?
  - can we do this by just shelling out using `--constraints` instead of building a whole fake project on disk?
- cabal build the fake project?

hmm, will different platforms have different unit IDs?
- can i still cache/restore on unit IDs? or do i need to do something extra with pkg IDs?
  - build some sort of "cross-platform ID"
- this will impact "verify" - cannot just verify on unit IDs because unit IDs will change across devices
  - from cabal-hash.txt:
    - constant:
      - pkgid
      - component
      - src
      - pkg-config-deps?
      - deps
      - compilerid
      - flags
      - shared-lib
      - stripped-exe
      - debug-info
    - differing:
      - platform
  - how do cabal.project flags like `enable-documentation` interact with this?
    - `enable-profiling` does not seem to make a difference
    - `cabal install aeson-1.5.4.1 --enable-documentation` did
      - did this result in a different unit ID?
        - yes: produced `aeson-1.5.4.1-bd6352ebfecf22544d39b5e40d0d57476514dbd7639c6cc1d6a16533d83f9ac0`
        - added `documentation: True` to cabal-hash
          - what is this? is this a `cabal.project`? seems similar but not quite
            - here's how it gets generated: https://github.com/haskell/cabal/blob/c024065b2104fab366ea14775ca4191764d382bd/cabal-install/src/Distribution/Client/PackageHash.hs

is `.cabal/store/ghc-9.0.2/packages.db/foo.conf` interesting?
  - it contains some neat dir paths
  - https://github.com/haskell/cabal/issues/5125

https://github.com/haskell/cabal/issues/5116 ghc --info in nix hash, only needed for ghc devs


https://github.com/haskell/cabal/issues/5582 remote cache RFC
- https://github.com/haskell/cabal/issues/4097 - relocatable nix-store packages
- https://github.com/buchgr/bazel-remote/
- https://hackage.haskell.org/package/cabal-cache
  - https://github.com/haskell-works/cabal-cache/blob/main/app/App/Commands/SyncToArchive.hs


see https://github.com/haskell/cabal/search?q=cabal-hash.txt&type=issues


does `--enable-documentation` show up in the `plan.json`? will need this in order to `cabal install` the libs
- test using cabal.project constraint?
  - aeson
    - did do a rebuild
    - don't see it reflected in the install plan
      - looks like the unit ID changed though - i bet the new unit ID has a different cabal-hash.txt
        - confirmed
          - original: aeson-2.1.1.0-598a8ea407b0797100ab634ac387264cac5e0afca93b45f0dabc3f2fc29bd7cc
          - new: aeson-2.1.1.0-23a158a0c7be265323ccd7723da6e116ad70369c9cbb5adade418da2ea86aa37


why did cabal-cache use the `fd75f...` prefix for some packages? what's special about them?
- `tree` and `cabal-hash` don't seem very different
  - comparing
    - `ansi-terminal-0.11.3-2205f4773d2e59e2448d596f431cc607f8116c8f541354ed50478a8ac53e51c8`
    - `assoc-1.0.2-a14313f93a3bc55e3801c98d46e6b4065d332c4e9b361847c890baaa053b1f06`
  - `cabal-cache plan | tail -n +5 | jq  | less`

does source hash change depending on template haskell or cpp conditionals?

create a debug command that dumps a bunch of debugging shit to JSON
- the whole store
  - unit IDs
  - cabal hashes
- the local install plan

does there exist a cabal-hash parser lib? can i rip one out of Cabal-the-library?

can i use `cabal install` as a library rather than shelling out?
- _should_ i, for compat reasons?
- can i construct a fake install plan from lockfile restore and `cabal build` it?
  - trace the code path

can i use `cabal freeze` and parse that ouput for my lockfile?
- or use it to guide what i need for my lockfile?
  - it only appears to have versions, repos, index state, and flags
    - not even `documentation:` etc. configurations - probably relying on those being already in `cabal.project`
- flags are also in `plan.json`
  - use cabal-hash to get configurations (like `documentation:`)
