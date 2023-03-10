# To do

- Add Postgres to save store path out-of-band
  - Alternatively, maybe save cached packages in rewrite-friendly format?

----

- [x] Fix bug: `however the given installed package instance does not exist`
  - Probably because we don't currently save the `.conf` package configuration file

- [x] Compiled units are properly restored on systems with the same Cabal store path
  - Today, we don't take Cabal store path into account.

- [ ] When units are relocatable, allow them to be restored even when the Cabal store path is different
  - Will need to rewrite the `.conf` file for relocation
  - `cabal-cache` does this by checking whether `/share` in component contains any non-relocatable data files
    - Note that to be relocatable, all transitive dependencies must also be relocatable
    - Is there a way we can rewrite the module instead to allow relocatability?
    - Can we detect whether a `Paths_` module is actually used?
    - Or allow package authors to whitelist for caching?
      - Detect a `.hurry.yml` `extra-source-file`? Or allow manual whitelisting from operator?

- [ ] Emit warning when lockfile is out-of-sync from current installation plan
  - Maybe make this into a `hurry verify` subcommand?

- [ ] Create a cross-platform lockfile
  - I'm not sure we can use unit IDs here, because unit IDs might vary between platforms
  - Can we just enumerate supported platforms?
  - Alternatively, should we lock platform-agnostic descriptions of packages? For example: name, package, flags, constraints (using cabal-hash and .conf info?)
    - Given these descriptions, how do we restore packages?
