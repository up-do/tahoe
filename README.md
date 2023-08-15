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
