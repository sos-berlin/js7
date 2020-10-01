package js7.base.crypt

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class SignedStringTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(SignedString(
      "STRING",
      GenericSignature("SIGNATURE TYPE", "SIGNATURE", Some("SIGNERS CERTIFICATE"))),
      json"""{
        "string": "STRING",
        "signature": {
          "TYPE": "SIGNATURE TYPE",
          "signatureString": "SIGNATURE",
          "signerCertificate": "SIGNERS CERTIFICATE"
        }
      }""")
  }
}
