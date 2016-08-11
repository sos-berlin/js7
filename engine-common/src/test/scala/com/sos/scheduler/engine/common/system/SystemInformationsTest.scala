package com.sos.scheduler.engine.common.system

import com.sos.scheduler.engine.base.system.SystemInformation
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class SystemInformationsTest extends FreeSpec {

  // See also com.sos.scheduler.engine.base.system.SystemInformationTest

  "JSON" in {
    val o = SystemInformations.systemInformation()
    assert(o.toJson.convertTo[SystemInformation] == o)
  }
}
