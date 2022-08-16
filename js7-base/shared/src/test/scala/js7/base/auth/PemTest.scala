package js7.base.auth

import js7.base.data.ByteArray
import js7.base.problem.Problem
import js7.base.test.OurTestSuite

final class PemTest extends OurTestSuite
{
  private val pem = Pem("TEST")
  private val byteArray = ByteArray("ЙС7 · and more and more text to get two base64 lines ...")
  private val pemString =
    "-----BEGIN TEST-----\r\n" +
    "0JnQoTcgwrcgYW5kIG1vcmUgYW5kIG1vcmUgdGV4dCB0byBnZXQgdHdvIGJhc2U2NCBsaW5lcyAu\r\n" +
    "Li4=\r\n" +
    "\r\n" +
    "-----END TEST-----\r\n"

  "fromPem" in {
    assert(pem.fromPem(pemString) == Right(byteArray))
  }

  "toPem" in {
    assert(pem.toPem(byteArray) == pemString)
  }

  "pemTypeOf" in {
    assert(Pem.pemTypeOf("") == Left(Problem.pure("PEM format expected")))
    assert(Pem.pemTypeOf("-----BEGIN MY TYPE") == Left(Problem.pure("PEM format expected")))
    assert(Pem.pemTypeOf("-----BEGIN MY TYPE-----\nbla") == Right("MY TYPE"))
  }
}
