package js7.common.system

import io.circe.syntax.EncoderOps
import js7.base.system.SystemInformation
import js7.base.test.Test

/**
  * @author Joacim Zschimmer
  */
final class SystemInformationsTest extends Test {

  // See also js7.base.system.SystemInformationTest

  "JSON" in {
    val o = SystemInformations.systemInformation()
    assert(o.asJson.as[SystemInformation] == Right(o))
  }
}
