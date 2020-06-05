package js7.base.crypt

import js7.base.circeutils.CirceUtils._
import js7.tester.CirceJsonTester
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GenericSignatureTest extends AnyFreeSpec
{
  CirceJsonTester.testJson(GenericSignature("SIGNATURE-TYPE", "SIGNATURE"), json"""
    {
      "TYPE": "SIGNATURE-TYPE",
      "signatureString": "SIGNATURE"
    }""")
}
