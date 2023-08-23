Great Black Swamp LAFS
======================

This is a preliminary implementation of the ``Great Black Swamp`` storage protocol.

Requirements
------------

tahoe-great-black-swamp uses stack and GHC.
Most dependencies should be handled automatically by stack.
If you have nix installed then you can use ``nix-shell`` to set up a build environment containing the non-Haskell dependencies.
If you don't have nix then you should install these some other way:

  * zlib-dev
  * autoconf

Run Unit Tests
--------------

::

   stack test

Write a Client
--------------

Here's a client program that shows two things:

* a server's version response
* all of the share numbers it claims to hold for a particular storage index

The server details are encoded in the NURL and the storage index is hard-coded in another string.

::

   import TahoeLAFS.Storage.Client (parseNURL, version, getImmutableShareNumbers)

   main :: IO ()
   main =
     let
       Right nURL = parseNURL "pb://..."
       storageIndex = "aaabbbcccdddeeefffggg"
     in
       runGBS nURL $ do
       version >>= liftIO . print
       getImmutableShareNumbers storageIndex >>= liftIO . print

Generate GBS Clients
--------------------

Several client libraries can be automatically derived from the GBS interface definition.
These are written as source code to the working directory.

::

   mkdir generated-clients
   cd generated-clients
   stack build
   stack exec -- gbs-generate-clients

Generate GBS API Docs
---------------------

GBS API documentation can be generated from the GBS interface definition.

::

   stack build
   stack exec -- gbs-generate-apidocs

Run
---

::

   mkdir some-storage-dir
   stack build
   stack exec -- tahoe-great-black-swamp --storage-path some-storage-dir

Inspect the version of your new GBS server::

  curl --header 'Content-Type: application/json' --header 'Accept: application/json' http://localhost:8888/v1/version

You should expect a response something like::

  {
    "application-version": "tahoe-lafs (gbs) 0.1.0",
    "parameters": {
      "http-protocol-available": true,
      "fills-holes-with-zero-bytes": true,
      "available-space": 6215475200,
      "tolerates-immutable-read-overrun": true,
      "maximum-immutable-share-size": 6215475200,
      "prevents-read-past-end-of-share-data": true,
      "maximum-mutable-share-size": 69105000000000000,
      "delete-mutable-shares-with-zero-length-writev": true
    }
  }
