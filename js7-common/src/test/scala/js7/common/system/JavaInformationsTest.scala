package js7.common.system

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.RichJson
import js7.data.system.JavaInformation
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JavaInformationsTest extends AnyFreeSpec {

  "JSON" in {
    val javaInformation = JavaInformations.javaInformation
    val json = javaInformation.asJson
    assert(json.as[JavaInformation] == Right(javaInformation))
    assert(json.jsonObjectOrThrow.toMap("systemProperties").jsonObjectOrThrow.toMap contains "java.version")
    assert(json.jsonObjectOrThrow.toMap("systemProperties").jsonObjectOrThrow.toMap contains "os.arch")
  }
}
