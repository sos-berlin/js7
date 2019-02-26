package com.sos.jobscheduler.data.crypt

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.tester.CirceJsonTester
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GenericSignatureTest extends FreeSpec
{
  CirceJsonTester.testJson(GenericSignature("SIGNATURE-TYPE", "SIGNATURE"), json"""
    {
      "TYPE": "SIGNATURE-TYPE",
      "string": "SIGNATURE"
    }""")
}
