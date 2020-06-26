package js7.controller.client

import js7.base.web.Uri
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.event.{Event, EventRequest}
import js7.data.fatevent.OrderFatEvent
import js7.data.filebased.RepoEvent.FileBasedEvent
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
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
    assert(controllerUris.events(EventRequest.singleClass[Event](after = 7, timeout = Some(1.seconds)), eventIdOnly = true) ==
      Uri("http://example.com/controller/api/event?eventIdOnly=true&return=Event&delay=0&timeout=1&after=7"))
    assert(controllerUris.events(EventRequest[Event](Set(classOf[OrderEvent], classOf[FileBasedEvent]), after = 7, Some(1.second)), eventIdOnly = true) ==
      Uri("http://example.com/controller/api/event?eventIdOnly=true&return=OrderEvent,FileBasedEvent&delay=0&timeout=1&after=7"))
  }

  "clusterState" in {
    assert(controllerUris.clusterState == Uri("http://example.com/controller/api/cluster"))
  }

  "journal" in {
    assert(controllerUris.journal(fileEventId = 100, position = 111, timeout = Some(50.seconds)) ==
      Uri("http://example.com/controller/api/journal?timeout=50&file=100&position=111"))
    assert(controllerUris.journal(fileEventId = 100, position = 111, heartbeat = Some(22.seconds), markEOF = true, returnLength = true) ==
      Uri("http://example.com/controller/api/journal?return=length&heartbeat=22&markEOF=true&file=100&position=111"))
  }

  "fatEvent" in {
    assert(controllerUris.fatEvents(EventRequest.singleClass[OrderFatEvent](after = 7, timeout = Some(1230.millis))) ==
      Uri("http://example.com/controller/api/fatEvent?return=OrderFatEvent&delay=0&timeout=1.23&after=7"))
    assert(controllerUris.fatEvents(EventRequest.singleClass[OrderFatEvent](after = 7, timeout = Some(1230.millis), limit = 333)) ==
      Uri("http://example.com/controller/api/fatEvent?return=OrderFatEvent&delay=0&timeout=1.23&limit=333&after=7"))
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

  "workflow" - {
    "single" in {
      assert(controllerUris.workflow(WorkflowPath("/A/B")) == Uri("http://example.com/controller/api/workflow/A%2FB"))
    }

    "list" in {
      assert(controllerUris.workflow.list[Workflow] == Uri("http://example.com/controller/api/workflow/?return=Workflow"))
    }
  }

  "agent" - {
    "single" in {
      assert(controllerUris.agent(AgentRefPath("/A/B")) == Uri("http://example.com/controller/api/agent/A%2FB"))
    }

    "list" in {
      assert(controllerUris.agent.list[AgentRef] == Uri("http://example.com/controller/api/agent/?return=AgentRef"))
    }
  }

  "snapshot" - {
    "list" in {
      assert(controllerUris.snapshot.list == Uri("http://example.com/controller/api/snapshot/"))
    }
  }
}
