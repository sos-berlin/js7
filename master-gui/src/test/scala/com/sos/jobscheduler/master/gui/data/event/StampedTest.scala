package com.sos.jobscheduler.master.gui.data.event

import com.sos.jobscheduler.master.gui.data.event.StampedTest._
import io.circe.generic.auto._
import io.circe.parser._
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class StampedTest extends FreeSpec {

  "map" in {
    val eventId = EventId(100)
    assert((Stamped(eventId, 3) map { _.toString }) == Stamped(eventId, "3"))
  }

  "Pattern matching ignores EventId" in {
    assertResult(3) {
      Stamped(EventId(100), 3) match {
        case Stamped(_, o) ⇒ o
      }
    }
  }

  "JSON with object" in {
    val o = Stamped(EventId(777), A(111))
    val json = parse("""{
      "eventId": 777,
      "number": 111
    }""").toTry.get
    //assert(o.asJson == json)
    assert(o == json.as[Stamped[A]].toTry.get)
  }

  "JSON with array" in {
    val o = Stamped(EventId(777), List(111, 222))
    val json = parse("""{
      "eventId": 777,
      "elements": [111, 222]
    }""").toTry.get
    //assert(o.asJson == json)
    assert(o == json.as[Stamped[Seq[Int]]].toTry.get)
  }
}

object StampedTest {
  final case class A(number: Int)
}
