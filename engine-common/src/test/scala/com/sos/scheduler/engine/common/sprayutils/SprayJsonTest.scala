package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.common.sprayutils.SprayJson.implicits._
import com.sos.scheduler.engine.common.sprayutils.SprayJsonTest._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json.DefaultJsonProtocol._
import spray.json.{pimpAny, pimpString}

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class SprayJsonTest extends FreeSpec {

  "Map JSON" in {
    implicit val jsonWriter = jsonFormat2(A.apply)
    val obj = A(111, Map("string" → "STRING", "int" → 333))
    val json =
      """{
        "int": 111,
        "map": {
          "string": "STRING",
          "int": 333
        }
      }""".parseJson
    assert(obj.toJson == json)
    intercept[UnsupportedOperationException] { json.convertTo[A] }
  }
}

object SprayJsonTest {
  private case class A(int: Int, map: Map[String, Any])
}
