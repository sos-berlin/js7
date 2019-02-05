package com.sos.jobscheduler.data.crypt

import com.sos.jobscheduler.tester.CirceJsonTester
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GenericSignatureTest extends FreeSpec
{
  CirceJsonTester.testJson(GenericSignature("SIGNATURE-TYPE", "SIGNATURE"), """
    {
      "TYPE": "SIGNATURE-TYPE",
      "string": "SIGNATURE"
    }""")
}
