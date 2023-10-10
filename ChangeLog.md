# Changelog for tahoe-great-black-swamp

## 0.3.1.0

* ``TahoeLAFS.Storage.Client.runGBS`` is a new high-level API for performing an interaction with a GBS server.
* Transitive ``http2`` dependency is now constrained to ``<4`` for compatibility with warp 3.3.25.

## 0.3.0.1

* Package metadata improvements.

## 0.3.0.0

* Support for ``GET /storage/v1/mutable/:storage-index/shares`` to list *mutable* shares on a server.
* Support for ``GET /storage/v1/mutable/:storage-index/:share-number`` to read *mutable* share bytes from a server.

## 0.2.0.2

* Initial release.
* Support for ``GET /storage/v1/immutable/:storage-index/shares`` to list shares on a server.
* Support for ``GET /storage/v1/immutable/:storage-index/:share-number`` to read share bytes from a server.
