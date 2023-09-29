package js7.common.auth

import js7.base.test.OurTestSuite
import js7.common.auth.Hasher.sha512

final class HasherTest extends OurTestSuite:
  "SHA-512" in:
    assert(sha512.apply("") ==
      "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e")
    assert(sha512.apply("A") ==
      "21b4f4bd9e64ed355c3eb676a28ebedaf6d8f17bdc365995b319097153044080516bd083bfcce66121a3072646994c8430cc382b8dc543e84880183bf856cff5")
