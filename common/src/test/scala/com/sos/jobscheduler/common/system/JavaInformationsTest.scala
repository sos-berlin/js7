package com.sos.jobscheduler.common.system

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.data.system.JavaInformation
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JavaInformationsTest extends FreeSpec {

  "JSON" in {
    val javaInformation = JavaInformations.javaInformation
    val json = javaInformation.asJson
    assert(json.as[JavaInformation] == Right(javaInformation))
    assert(json.forceObject.toMap("systemProperties").forceObject.toMap contains "java.version")
    assert(json.forceObject.toMap("systemProperties").forceObject.toMap contains "os.arch")
  }
}
