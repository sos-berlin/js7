package js7.base.crypt

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.testJson

final class SignedStringTest extends OurTestSuite:
  "JSON" in:
    testJson(SignedString(
      "STRING",
      GenericSignature("SIGNATURE TYPE", "SIGNATURE")),
      json"""{
        "string": "STRING",
        "signature": {
          "TYPE": "SIGNATURE TYPE",
          "signatureString": "SIGNATURE"
        }
      }""")

    testJson(SignedString(
      "STRING",
      GenericSignature("SIGNATURE TYPE", "SIGNATURE",
        algorithm = Some("ALGORITHM"),
        Some(SignerId("SIGNER")),
        signerCertificate = Some("SIGNERS CERTIFICATE"))),
      json"""{
        "string": "STRING",
        "signature": {
          "TYPE": "SIGNATURE TYPE",
          "signatureString": "SIGNATURE",
          "algorithm": "ALGORITHM",
          "signerId": "SIGNER",
          "signerCertificate": "SIGNERS CERTIFICATE"
        }
      }""")
