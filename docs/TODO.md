- [ ] `cabal install hurry`

- [ ] Actionable error handling
  - When we can't parse plan.json files: "are you sure this is a cabal installation dependency install plan? If so, submit a bug report at $REPO and attach the plan.json file"

- [ ] CLI analytics
  - What GHC and cabal versions are being used? Which are well-supported? Which are erroring out?

- [ ] `hurry cache`
  - Add some sort of exclude mechanism for dependencies that aren't cacheable?
    - Example: non-relocatable native C libraries

- [ ] `hurry restore`

