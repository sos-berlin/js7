package js7.controller.client

import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.data.event.{EventRequest, JournalPosition}
import js7.data.order.{Order, OrderEvent, OrderId}
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class ControllerUrisTest extends AnyFreeSpec
{
  private val controllerUris = ControllerUris(Uri("http://example.com/controller"))

  "overview" in {
    assert(controllerUris.overview == Uri("http://example.com/controller/api"))
  }

  "command" in {
    assert(controllerUris.command == Uri("http://example.com/controller/api/command"))
  }

  "events" in {
    assert(controllerUris.events(EventRequest.singleClass[OrderEvent](after = 7, timeout = Some(1230.millis))) ==
      Uri("http://example.com/controller/api/event?return=OrderEvent&delay=0&timeout=1.23&after=7"))
    assert(controllerUris.events(EventRequest.singleClass[OrderEvent](after = 7, timeout = Some(1230.millis), limit = 333)) ==
      Uri("http://example.com/controller/api/event?return=OrderEvent&delay=0&timeout=1.23&limit=333&after=7"))

    // return EventId only
    assert(controllerUris.eventIds(timeout = Some(1.s)) ==
      Uri("http://example.com/controller/api/event?onlyAcks=true&timeout=1"))
  }

  "clusterState" in {
    assert(controllerUris.clusterState == Uri("http://example.com/controller/api/cluster"))
  }

  "journal" in {
    assert(controllerUris.journal(JournalPosition(100, 111), timeout = Some(50.s)) ==
      Uri("http://example.com/controller/api/journal?timeout=50&file=100&position=111"))
    assert(controllerUris.journal(JournalPosition(100, position = 111), heartbeat = Some(22.s), markEOF = true, returnAck = true) ==
      Uri("http://example.com/controller/api/journal?return=ack&heartbeat=22&markEOF=true&file=100&position=111"))
  }

  "order" - {
    "overview" in {
      assert(controllerUris.order.overview == Uri("http://example.com/controller/api/order"))
    }

    "single" in {
      assert(controllerUris.order(OrderId("ORDER-ID")) == Uri("http://example.com/controller/api/order/ORDER-ID"))
      assert(controllerUris.order(OrderId("/å")) == Uri("http://example.com/controller/api/order/%2F%C3%A5"))
    }

    "list" in {
      assert(controllerUris.order.list[Order[Order.State]] == Uri("http://example.com/controller/api/order/?return=Order"))
    }
  }

  "snapshot" - {
    "list" in {
      assert(controllerUris.snapshot.list == Uri("http://example.com/controller/api/snapshot/"))
    }
  }
}
