package js7.core.crypt.x509

import js7.base.data.ByteArray
import org.scalatest.freespec.AnyFreeSpec

class PemTest extends AnyFreeSpec
{
  private val pem = Pem("TEST")
  private val byteArray = ByteArray("ЙС7 · and more and more text to get two BASE64 lines ...")
  private val pemString =
    "-----BEGIN TEST-----\r\n" +
    "0JnQoTcgwrcgYW5kIG1vcmUgYW5kIG1vcmUgdGV4dCB0byBnZXQgdHdvIEJBU0U2NCBsaW5lcyAu\r\n" +
    "Li4=\r\n" +
    "\r\n" +
    "-----END TEST-----\r\n"

  "fromPem" in {
    assert(pem.fromPem(pemString) == Right(byteArray))
  }

  "toPem" in {
    assert(pem.toPem(byteArray) == pemString)
  }
}
