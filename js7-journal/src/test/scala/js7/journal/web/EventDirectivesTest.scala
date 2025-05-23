package js7.journal.web

import cats.syntax.option.*
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, EventId, EventRequest, KeyedEvent, KeyedEventTypedJsonCodec}
import js7.journal.web.EventDirectives.eventRequest
import js7.journal.web.EventDirectivesTest.*
import org.apache.pekko.http.scaladsl.server.Directive1
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.testkit.ScalatestRouteTest
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class EventDirectivesTest extends OurTestSuite, ScalatestRouteTest:

  override def testConfig = config"pekko.loglevel = warning"
    .withFallback(super.testConfig)

  private implicit val myKeyedEventJsonFormat: KeyedEventTypedJsonCodec[MyEvent] =
    KeyedEvent.typedJsonCodec(
      KeyedSubtype.singleEvent[AEvent],
      KeyedSubtype.singleEvent[BEvent])

  private def route =
    path("test"):
      val x: Directive1[EventRequest[MyEvent]] =
        eventRequest[MyEvent](minimumDelay = 500.ms)
      x.apply((eventReq: EventRequest[MyEvent]) =>
        if eventReq == EventRequest[MyEvent](
          Set(classOf[AEvent]),
          after = EventId(1), delay = 500.ms, timeout = None)
        then
          complete("DEFAULT")
        else if eventReq == EventRequest[MyEvent](
          Set(classOf[AEvent]),
          after = EventId(66), delay = 770.millis, timeout = 88.s.some, limit = 99,
          tornOlder = Some(10.s))
        then
          complete("A")
        else if eventReq == EventRequest[MyEvent](
          Set(classOf[AEvent], classOf[BEvent]),
          after = EventId(666), delay = 777.millis, timeout = 888.s.some, limit = 999)
        then
          complete("B")
        else if eventReq == EventRequest[MyEvent](
          Set(classOf[AEvent], classOf[BEvent]),
          after = EventId(3), delay = 500.ms, timeout = None)
        then
          complete("C")
        else
          println(eventReq)
          reject)

  "eventRequest" in:
    Get("/test?return=AEvent&after=1") ~> route ~> check:
      assert(responseAs[String] == "DEFAULT")
    Get("/test?return=AEvent&delay=0.77&timeout=88&limit=99&after=66&tornOlder=10") ~> route ~> check:
      assert(responseAs[String] == "A")
    Get("/test?return=AEvent,BEvent&delay=0.777&timeout=888&limit=999&after=666") ~> route ~> check:
      assert(responseAs[String] == "B")
    Get("/test?return=AEvent,BEvent&timeout=infinite&after=3") ~> route ~> check:
      assert(responseAs[String] == "C")


object EventDirectivesTest:
  sealed trait MyEvent extends Event.IsKeyBase[MyEvent]:
    val keyCompanion: MyEvent.type = MyEvent
  object MyEvent extends Event.CompanionForKey[String, MyEvent]:
    implicit val implicitSelf: MyEvent.type = MyEvent

  final case class AEvent() extends MyEvent
  object AEvent:
      implicit val jsonCodec: Codec.AsObject[AEvent] = deriveCodec

  final case class BEvent() extends MyEvent
  object BEvent:
      implicit val jsonCodec: Codec.AsObject[BEvent] = deriveCodec
