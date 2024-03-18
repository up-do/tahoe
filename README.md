# Tahoe-Capabilities

## What is it?

Tahoe-LAFS LIT, CHK, SDMF, and MDMF capabilities offer varying functionality for interacting with encrypted data.
This library provides abstractions for functionality common to all of these capability types.

All responsibility for implementing these abstractions is left to the capability-specific libraries.

### What is the current state?

* The `ConfidentialShowable` type class provides a uniform way for losslessly converting a capability to a string.

### What is planned?

Sufficient abstraction so that general encoding, decoding, upload, and download functions could be implemented independent of the specific capability type in use.

## Why does it exist?

Each specific Tahoe-LAFS capability type supports certain operations and offers certain security properties.
At a high level,
an application will often want to select among the available capability types to satisfy its particular security and/or privacy requirements.
A lower level exists encompassing such details of a capability type as which hash function is used or the size or alignment of certain fields.
Applications should typically not be required to make choices at this lower level.
By using a set of operations that abstract over these details,
application code can be kept simpler and more general-purpose.
By allowing some centralization of the choice of which concrete capability type to use,
applications can also be made more secure over time at a lower maintenance cost.


#Tahoe-CHK

## What is it?

Tahoe-CHK is a Haskell implementation of the [Tahoe-LAFS](https://tahoe-lafs.org/) CHK crytographic protocol.
It aims for bit-for-bit compatibility with the original Python implementation.

It will not include an implementation of any network protocol for transferring CHK shares.
However, its APIs are intended to be easy to integrate with such an implementation.

### What is the current state?

* Convergent encryption is supported and compatible with Tahoe-LAFS.
* CHK encoding is implemented but some cases are unsupported:
  * It is not allowed that k == 1 or k == n.
* CHK decoding is implemented with the same limitations as for encoding.
  * The decoding process:
	* Authenticates the data being decoded using the capability.
	* Ensures the integrity of the data being decoded using the embedded hashes.

## Why does it exist?

A Haskell implementation can be used in places the original Python implementation cannot be
(for example, runtime environments where it is difficult to have a Python interpreter).
Additionally,
with the benefit of the experience gained from creating and maintaining the Python implementation,
a number of implementation decisions can be made differently to produce a more efficient, more flexible, simpler implementation and API.
Also,
the Python implementation claims no public library API for users outside of the Tahoe-LAFS project itself.

# tahoe-ssk

## What is it?

Tahoe-SSK is a Haskell implementation of the [Tahoe-LAFS](https://tahoe-lafs.org/) SSK crytographic protocols.
This includes (S)mall (D)istributed (M)utable (F)iles (SDMF) and (M)edium (D)istributed (M)utable (F)iles (MDMF).
It aims for bit-for-bit compatibility with the original Python implementation.

It will not include an implementation of any network protocol for transferring SSK shares.
However, its APIs are intended to be easy to integrate with such an implementation.

### What is the current state?

* SDMF write, read, and verify capabilities can be parsed and serialized.
* SDMF shares can be deserialized, decoded, and decrypted.
  * The cryptographic integrity is not verified:
	* a share's block hashes are not checked against the share's block hash tree
	* the root of the computed share hash tree is not checked against share's pre-computed share hash tree root
  * The cryptographic authenticity is not verified:
	* signatures on the data are not checked

* Plaintext can be encrypted, encoded into shares, and the shares serialized to bytes.
  * Not all fields of the shares contain correctly initialized values.
  * Enough fields are correctly populated to recover the original plaintext.

## Why does it exist?

A Haskell implementation can be used in places the original Python implementation cannot be
(for example, runtime environments where it is difficult to have a Python interpreter).
Additionally,
with the benefit of the experience gained from creating and maintaining the Python implementation,
a number of implementation decisions can be made differently to produce a more efficient, more flexible, simpler implementation and API.
Also,
the Python implementation claims no public library API for users outside of the Tahoe-LAFS project itself.

## Cryptographic Library Choice

This library uses cryptonite for cryptography,
motivated by the following considerations.

SDMF uses
* SHA256 for tagged hashes for key derivation and for integrity (XXX right word?) checks on some data.
* AES128 for encryption of the signature key and the application plaintext data.
* RSA for signatures proving write authority.

There are a number of Haskell libraries that provide all of these:

* Crypto
  * Does not support the AES mode we require (CTR).

* HsOpenSSL
  * Bindings to a C library, OpenSSL, which may complicate the build process.
  * OpenSSL's security and reliability track record also leaves something to be desired.

* cryptonite
  * Has all of the primitive cryptographic functionality we need.

We want a library that:

* Can be used with reflex-platform
  * ghc 8.6.5 compatible
* Can be cross-compiled to different targets from x86_64-linux
  * Mainly armeabi and armv7
* Is suitable for real-world security purposes
  * not a demo or a toy library
	* documents its limitations
  * is well-tested
  * avoids real-world pitfalls (side-channel attacks, etc), not just textbook issues
  * has more than a handful of other users
  * is well-maintained
	* developers are responsive to security reports
	* has a channel for security-related disclosures
	* has sound documentation for proper, safe usage

And,
of course,
implements the required functionality.

### SHA256

There are a number of Haskell libraries that provide this primitive:

* Crypto
* HsOpenSSL
* SHA
* cryptohash
* cryptonite
* dhall
* hashing
* nettle
* sbv
* tls

### AES128

* Crypto
* HsOpenSSL
* cipher-aes
* cipher-aes128
* crypto
* cryptocipher
* cryptonite
* cryptostore
* nettle

### RSA

SDMF depends on RSA for signatures proving write authority.

* Crypto
* HsOpenSSL
* RSA
* cryptonite
