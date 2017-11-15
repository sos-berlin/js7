package com.sos.jobscheduler.master.gui.data.event

import com.sos.jobscheduler.master.gui.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.master.gui.data.event.KeyedEventTest._
import io.circe.generic.JsonCodec
import io.circe.parser._
import io.circe.{Decoder, HCursor}
import org.scalatest.FreeSpec
import scala.language.higherKinds

final class KeyedEventTest extends FreeSpec {

  "KeyedEvent apply 1" in {
    val e = KeyedEvent[StringEvent](100, StringEvent("HUNDRED"))
    assert(e.key == 100)
    assert(e.event.string == "HUNDRED")
  }

  "KeyedEvent apply 2 without type parameter" in {
    val e = KeyedEvent(StringEvent("HUNDRED"))(100)
    assert(e.key == 100)
    assert(e.event.string == "HUNDRED")
  }

  "StringEvent" in {
    check(KeyedEvent(StringEvent("HUNDRED"))(100),
      """{
        "TYPE": "StringEvent",
        "key": 100,
        "string": "HUNDRED"
      }""")
  }

  "IntEvent" in {
    check(KeyedEvent(IntEvent(111))(100),
      """{
        "TYPE": "IntEvent",
        "key": 100,
        "int": 111
      }""")
  }

  "SimpleEvent" in {
    check(KeyedEvent(SimpleEvent)(100),
      """{
        "TYPE": "SimpleEvent",
        "key": 100
      }""")
  }

  "NoKeySimpleEvent" in {
    checkSingletonKey(KeyedEvent(NoKeyIntEvent(111)),
      """{
        "TYPE": "NoKeyIntEvent",
        "int": 111
      }""")
  }
}

private object KeyedEventTest {
  sealed trait TestEvent extends Event {
    type Key = Int
  }

  @JsonCodec final case class StringEvent(string: String) extends TestEvent

  @JsonCodec final case class IntEvent(int: Int) extends TestEvent

  final case object SimpleEvent extends TestEvent

  @JsonCodec final case class NoKeyIntEvent(int: Int) extends NoKeyEvent

  private def check(event: KeyedEvent[TestEvent], json: String): Unit = {
    implicit val testEventDecoder: Decoder[TestEvent] = (c: HCursor) ⇒
      Decoder.decodeJsonObject map { obj ⇒
        obj("TYPE") flatMap (_.asString) getOrElse sys.error("Missing or invalid JSON field 'TYPE'") match {
          case "StringEvent" ⇒ c.value.as[StringEvent].toTry.get
          case "IntEvent" ⇒ c.value.as[IntEvent].toTry.get
          case "SimpleEvent" ⇒ SimpleEvent
        }
      } apply c
    val jsValue = parse(json).toTry.get
    //assert(event.toJson == jsValue)
    assert(event == jsValue.as[KeyedEvent[TestEvent]].toTry.get)
  }

  private def checkSingletonKey(event: KeyedEvent[NoKeyEvent], json: String): Unit = {
    implicit val noKeyEventDecoder: Decoder[NoKeyEvent] = (c: HCursor) ⇒
      for (typ ← c.downField("TYPE").as[String]) yield typ match {
        case "NoKeyIntEvent" ⇒ c.as[NoKeyIntEvent].toTry.get
      }
    implicit val noKeyDecoder: Decoder[NoKey] = (_: HCursor) ⇒ Right(NoKey)

    val jsValue = parse(json).toTry.get
    println("NoKeyEvent=" + jsValue.as[NoKeyEvent])
    //assert(event.toJson == jsValue)
    assert(event == jsValue.as[KeyedEvent[NoKeyEvent]].toTry.get)
  }
}
