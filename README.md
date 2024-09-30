# Tahoe-LAFS with S3 Storage

This is a monorepo for building `tahoe-s3`. The local packages are:

```
$ tree -P '*.cabal' --prune
.
├── capabilities
│   └── tahoe-capabilities.cabal
├── s3
│   └── tahoe-s3.cabal
└── swamp
    ├── swamp
    │   └── tahoe-great-black-swamp.cabal
    ├── testing
    │   └── tahoe-great-black-swamp-testing.cabal
    └── types
        └── tahoe-great-black-swamp-types.cabal
```

The project builds with `ghc-8.10.7`, `ghc-9.6.6` and `ghc-9.8.2`.

To build the default project:

* with Cabal:

    ```
    $ cabal build all --enable-tests --enable-benchmarks
    Resolving dependencies...
    Build profile: -w ghc-8.10.7 -O1
    In order, the following will be built (use -v for more details):
    - tahoe-capabilities-0.6.0.0 (lib) (first run)
    - tahoe-great-black-swamp-types-0.6.0.0 (lib) (first run)
    - tahoe-capabilities-0.6.0.0 (exe:tahoe-chk-encrypt) (first run)
    - tahoe-capabilities-0.6.0.0 (test:tahoe-capabilities-test) (first run)
    - tahoe-capabilities-0.6.0.0 (exe:make-keypairs) (first run)
    - tahoe-capabilities-0.6.0.0 (exe:encode-ssk) (first run)
    - tahoe-great-black-swamp-testing-0.6.0.0 (lib) (first run)
    - tahoe-great-black-swamp-0.6.0.0 (lib) (first run)
    - tahoe-s3-0.1.0.0 (lib) (first run)
    - tahoe-great-black-swamp-0.6.0.0 (test:http-tests) (first run)
    - tahoe-great-black-swamp-0.6.0.0 (exe:gbs-lafs-storage-server) (first run)
    - tahoe-great-black-swamp-0.6.0.0 (exe:gbs-generate-clients) (first run)
    - tahoe-great-black-swamp-0.6.0.0 (exe:client-test) (first run)
    - tahoe-s3-0.1.0.0 (test:tahoe-s3-test) (first run)
    - tahoe-s3-0.1.0.0 (exe:tahoe-s3) (first run)
    ...
    ```

* with Stack:

    ```
    $ stack build --test --no-run-tests --bench --no-run-benchmarks
    ```

> [!WARNING]
> The test suites rely on an environment previously set up using nix. The nix
> flakes of each package are retained but not yet updated for the monorepo.

## Default Projects

Generate default projects, `cabal.project` and `stack.yaml`, with:

```
$ make -f project-files.mk
```

> [!NOTE]
> These projects as well as the upgrade projects (see below) have already been
> generated and commited so you should not see a change. You may like to delete
> them beforehand to be sure that the above command does indeed regenerate them.

## Updo Configuration

Updo configuration is available for these versions:

```make
$ cat project-versions.mk
# Versions of GHC and stackage resolver, the ones we're on and the next ones
# we're upgrading to.
GHC_VERSION ?= 8.10.7
STACKAGE_VERSION ?= lts-18.28

# For the upgrade, pick a matching pair of ghc-version and stack resolver.
# GHC_UPGRADE ?= 9.6.6
# STACKAGE_UPGRADE ?= lts-22.36
GHC_UPGRADE ?= 9.8.2
STACKAGE_UPGRADE ?= nightly-2024-09-30
```

The configuration is in `project-dhall` and `project-stackage` folders.

```
$ tree -P '*.dhall' --prune
.
└── project-dhall
    ├── ghc-8.10.7
    │   ├── constraints.dhall
    │   └── text-templates
    │       ├── cabal-snippet.dhall
    │       ├── dhall2cabal.dhall
    │       ├── dhall2stack.dhall
    │       └── stack-snippet.dhall
    ├── ghc-9.6.6
    │   ├── constraints.dhall
    │   └── text-templates
    │       ├── cabal-snippet.dhall
    │       ├── dhall2cabal.dhall
    │       ├── dhall2stack.dhall
    │       └── stack-snippet.dhall
    ├── ghc-9.8.2
    │   ├── constraints.dhall
    │   ├── deps-external.dhall
    │   └── text-templates
    │       ├── cabal-snippet.dhall
    │       ├── dhall2cabal.dhall
    │       ├── dhall2stack.dhall
    │       └── stack-snippet.dhall
    ├── pkg-groups.dhall
    ├── pkgs
    │   └── tahoe.dhall
    └── pkgs-upgrade-todo.dhall
```

```
$ tree -P '*.config' --prune
.
└── project-stackage
    ├── lts-18.28.config
    └── lts-22.36.config
```

## Upgrade Projects

Generate the configured upgrade projects, `cabal.upgrade.project` and
`stack.upgrade.yaml`, with:

```
$ make -f project-files.mk upgrade-projects
```

It is also possible to generate upgrade projects more explicitly:

```
$ GHC_VERSION=9.8.2 STACKAGE_VERSION=nightly-2024-09-20 \
  make -f project-files.mk upgrade-projects

$ GHC_VERSION=9.6.6 STACKAGE_VERSION=lts-22.36 \
  make -f project-files.mk upgrade-projects
```

To build the upgrade projects:

* with Cabal:

    ```
    $ cabal build all --enable-tests --enable-benchmarks --project-file=cabal.upgrade.project
    Cloning into '/.../tahoe/dist-newstyle/src/amazonka-94165020b73e5e01'...
    ...
    Resolving dependencies...
    Build profile: -w ghc-9.8.2 -O1
    ...
    ```

* with Stack:

    ```
    $ stack build --test --no-run-tests --bench --no-run-benchmarks --stack-yaml=stack.upgrade.yaml
    ```
