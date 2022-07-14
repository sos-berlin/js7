package js7.base.crypt

import js7.base.circeutils.CirceUtils.*
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GenericSignatureTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(GenericSignature("SIGNATURE-TYPE", "SIGNATURE"), json"""
      {
        "TYPE": "SIGNATURE-TYPE",
        "signatureString": "SIGNATURE"
      }""")

    testJson(
      GenericSignature("SIGNATURE-TYPE", "SIGNATURE",
        Some("ALGORITHM"), Some(SignerId("SIGNER")), Some("SIGNER CERTIFICATE")),
      json"""{
        "TYPE": "SIGNATURE-TYPE",
        "signatureString": "SIGNATURE",
        "algorithm": "ALGORITHM",
        "signerId": "SIGNER",
        "signerCertificate": "SIGNER CERTIFICATE"
      }""")
  }
}
