package com.sos.scheduler.engine.data.compounds

import com.sos.scheduler.engine.data.event.EventId
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._
import spray.json.DefaultJsonProtocol._
import scala.collection.immutable

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class SchedulerResponseTest extends FreeSpec {

  "JSON with object" in {
    case class A(number: Int)
    implicit val aJsonFormat = jsonFormat1(A)
    val o = SchedulerResponse(A(111))(EventId(777))
    val json = """{
      "eventId": 777,
      "number": 111
    }""".parseJson
    assert(o.toJson == json)
    assert(o == json.convertTo[SchedulerResponse[A]])
  }

  "JSON with array" in {
    val o = SchedulerResponse(List(111, 222))(EventId(777))
    val json = """{
      "eventId": 777,
      "schedulerResponseContent": [111, 222]
    }""".parseJson
    assert(o.toJson == json)
    assert(o == json.convertTo[SchedulerResponse[immutable.Seq[Int]]])
  }
}

