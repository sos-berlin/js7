package com.sos.jobscheduler.common.system

import com.sos.jobscheduler.base.system.SystemInformation
import io.circe.syntax.EncoderOps
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SystemInformationsTest extends AnyFreeSpec {

  // See also com.sos.jobscheduler.base.system.SystemInformationTest

  "JSON" in {
    val o = SystemInformations.systemInformation()
    assert(o.asJson.as[SystemInformation] == Right(o))
  }
}
