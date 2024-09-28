# Tested on Python 3.9.15 against Tahoe-LAFS bc79cf0a11f06bbdc02a5bb41c6f41fcff727ea5
#

from allmydata.crypto import rsa
from allmydata.mutable.common import derive_mutable_keys
from allmydata.util import base32
from allmydata.util import hashutil

# Arbitrarily select an IV.
iv = b"\x42" * 16
# And "peer id"
peerid = b"\x42" * 20

with open("data/rsa-privkey-0.der", "rb") as f:
    (priv, pub) = rsa.create_signing_keypair_from_string(f.read())

writekey, encprivkey, fingerprint = derive_mutable_keys((pub, priv))
readkey = hashutil.ssk_readkey_hash(writekey)
datakey = hashutil.ssk_readkey_data_hash(iv, readkey)
storage_index = hashutil.ssk_storage_index_hash(readkey)
write_enabler_master = hashutil.ssk_write_enabler_master_hash(writekey)
write_enabler = hashutil.ssk_write_enabler_hash(writekey, peerid)

print("SDMF")
print("writekey: ", base32.b2a(writekey))
print("readkey: ", base32.b2a(readkey))
print("datakey (iv = \x42 * 16): ", base32.b2a(datakey))
print("storage index: ", base32.b2a(storage_index))
print("encrypted private key: ", base32.b2a(encprivkey))
print("signature key hash: ", base32.b2a(fingerprint))
print("write enabler master: ", base32.b2a(write_enabler_master))
print("write enabler (peerid = \x42 * 20): ", base32.b2a(write_enabler))

(priv, pub) = rsa.create_signing_keypair(2048)
priv_bytes = rsa.der_string_from_signing_key(priv)
with open("data/tahoe-lafs-generated-rsa-privkey.der", "wb") as f:
    f.write(priv_bytes)
