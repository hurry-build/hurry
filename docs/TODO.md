- [x] Prune unused dependencies (e.g. lib deps of unused exes) from lockfile
- [ ] Actually save dependencies in upload handler
- [ ] Restore cached dependencies from lockfile

----

- [ ] `lock` and `restore` working on just this project on one platform
  - [ ] For now, we can just store using unit IDs - I don't think we need to do anything special here
    - [ ] Do we need special `Paths_` handling?
  - [ ] Make restore _only_ work from cache to start, and then later have it do installs
- [ ] `verify` working on one platform
- [ ] `restore` working cross-platform
  - [ ] Will need to parse cabal-hash in order to do cross-platform reinstall/restore (rather than just download from binary cache)

----

- Later, we should have `hurry build` that implicitly runs `verify`, `lock`, `restore`, and `save` as needed and those specific actions can be moved to subcommands

----

- [ ] `cabal install hurry`

- [ ] Actionable error handling
  - When we can't parse plan.json files: "are you sure this is a cabal installation dependency install plan? If so, submit a bug report at $REPO and attach the plan.json file"

- [ ] CLI analytics
  - What GHC and cabal versions are being used? Which are well-supported? Which are erroring out?

- [ ] `hurry cache`
  - Add some sort of exclude mechanism for dependencies that aren't cacheable?
    - Example: non-relocatable native C libraries

- [ ] `hurry restore`
