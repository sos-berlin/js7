package com.sos.scheduler.engine.base.sprayjson

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json.DefaultJsonProtocol._
import spray.json.{JsObject, JsString, pimpAny}

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class JavaTimeJsonFormatsTest extends FreeSpec {

  private case class A(instant: Instant)
  private implicit val aJsonFormat = jsonFormat1(A.apply)

  "Instant" in {
    val t = "2015-06-09T12:22:33Z"
    val a = A(Instant.parse(t))
    val j = JsObject("instant" → JsString(t))
    assert(a.toJson == j)
    assert(a == j.convertTo[A])
  }

  "Instant with milliseconds" in {
    val t = "2015-06-09T12:22:33.987Z"
    val a = A(Instant.parse(t))
    val j = JsObject("instant" → JsString(t))
    assert(a.instant.getNano == 987000000)
    assert(a.toJson == j)
    assert(a == j.convertTo[A])
  }

  "Instant with microseconds" in {
    val t = "2015-06-09T12:22:33.987654Z"
    val a = A(Instant.parse(t))
    val j = JsObject("instant" → JsString(t))
    assert(a.instant.getNano == 987654000)
    assert(a.toJson == j)
    assert(a == j.convertTo[A])
  }

  "Instant with nanoseconds" in {
    val t = "2015-06-09T12:22:33.987654321Z"
    val a = A(Instant.parse(t))
    val j = JsObject("instant" → JsString(t))
    assert(a.instant.getNano == 987654321)
    assert(a.toJson == j)
    assert(a == j.convertTo[A])
  }
}
