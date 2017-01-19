package com.sos.scheduler.engine.base.system

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class SystemInformationTest extends FreeSpec {

  "JSON 1" in {
    val obj = SystemInformation.ForTest
    val json = s"""{
      "hostname": "HOSTNAME",
      "mxBeans": {}
    }""".parseJson
    assert(obj.toJson == json)
  }
}
