package com.sos.jobscheduler.base.circeutils

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import org.scalatest.FreeSpec
import scala.collection.JavaConverters._

/**
  * @author Joacim Zschimmer
  */
final class CirceUtilsTest extends FreeSpec {

  "JavaMapJsonEncoder, JavaMapJsonDecoder" in {
    pending // TODO
  }
}
