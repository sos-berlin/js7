package com.sos.scheduler.engine.base.system

import org.scalatest.FreeSpec
import spray.json._

/**
  * @author Joacim Zschimmer
  */
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
