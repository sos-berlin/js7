package com.sos.scheduler.engine.common.system

import com.sos.scheduler.engine.base.system.SystemInformation
import org.scalatest.FreeSpec
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class SystemInformationsTest extends FreeSpec {

  // See also com.sos.scheduler.engine.base.system.SystemInformationTest

  "JSON" in {
    val o = SystemInformations.systemInformation()
    assert(o.toJson.convertTo[SystemInformation] == o)
  }
}
