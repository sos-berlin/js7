package js7.common.system

import js7.base.system.SystemInformation
import io.circe.syntax.EncoderOps
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SystemInformationsTest extends AnyFreeSpec {

  // See also js7.base.system.SystemInformationTest

  "JSON" in {
    val o = SystemInformations.systemInformation()
    assert(o.asJson.as[SystemInformation] == Right(o))
  }
}
