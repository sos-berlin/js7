package com.sos.jobscheduler.common.system

import com.sos.jobscheduler.base.system.SystemInformation
import org.scalatest.FreeSpec
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class SystemInformationsTest extends FreeSpec {

  // See also com.sos.jobscheduler.base.system.SystemInformationTest

  "JSON" in {
    val o = SystemInformations.systemInformation()
    assert(o.toJson.convertTo[SystemInformation] == o)
  }
}
