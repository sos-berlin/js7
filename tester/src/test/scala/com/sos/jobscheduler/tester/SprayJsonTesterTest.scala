package com.sos.jobscheduler.tester

import com.sos.jobscheduler.tester.CirceJsonTesterTest.{A, JsonString, a}
import com.sos.jobscheduler.tester.SprayJsonTester.testSprayJson
import org.scalatest.FreeSpec
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final class SprayJsonTesterTest extends FreeSpec {

  "Case class" in {
    implicit val jsonFormat = jsonFormat9(A)
    testSprayJson(a, JsonString)
  }
}
