package js7.master.client

import js7.base.web.Uri
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.event.{Event, EventRequest}
import js7.data.fatevent.OrderFatEvent
import js7.data.filebased.RepoEvent.FileBasedEvent
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import scala.concurrent.duration._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterUrisTest extends AnyFreeSpec
{
  private val masterUris = MasterUris(Uri("http://example.com/master"))

  "overview" in {
    assert(masterUris.overview == Uri("http://example.com/master/api"))
  }

  "command" in {
    assert(masterUris.command == Uri("http://example.com/master/api/command"))
  }

  "events" in {
    assert(masterUris.events(EventRequest.singleClass[OrderEvent](after = 7, timeout = Some(1230.millis))) ==
      Uri("http://example.com/master/api/event?return=OrderEvent&delay=0&timeout=1.23&after=7"))
    assert(masterUris.events(EventRequest.singleClass[OrderEvent](after = 7, timeout = Some(1230.millis), limit = 333)) ==
      Uri("http://example.com/master/api/event?return=OrderEvent&delay=0&timeout=1.23&limit=333&after=7"))

    // return EventId only
    assert(masterUris.events(EventRequest.singleClass[Event](after = 7, timeout = Some(1.seconds)), eventIdOnly = true) ==
      Uri("http://example.com/master/api/event?eventIdOnly=true&return=Event&delay=0&timeout=1&after=7"))
    assert(masterUris.events(EventRequest[Event](Set(classOf[OrderEvent], classOf[FileBasedEvent]), after = 7, Some(1.second)), eventIdOnly = true) ==
      Uri("http://example.com/master/api/event?eventIdOnly=true&return=OrderEvent,FileBasedEvent&delay=0&timeout=1&after=7"))
  }

  "clusterState" in {
    assert(masterUris.clusterState == Uri("http://example.com/master/api/cluster"))
  }

  "journal" in {
    assert(masterUris.journal(fileEventId = 100, position = 111, timeout = Some(50.seconds)) ==
      Uri("http://example.com/master/api/journal?timeout=50&file=100&position=111"))
    assert(masterUris.journal(fileEventId = 100, position = 111, heartbeat = Some(22.seconds), markEOF = true, returnLength = true) ==
      Uri("http://example.com/master/api/journal?return=length&heartbeat=22&markEOF=true&file=100&position=111"))
  }

  "fatEvent" in {
    assert(masterUris.fatEvents(EventRequest.singleClass[OrderFatEvent](after = 7, timeout = Some(1230.millis))) ==
      Uri("http://example.com/master/api/fatEvent?return=OrderFatEvent&delay=0&timeout=1.23&after=7"))
    assert(masterUris.fatEvents(EventRequest.singleClass[OrderFatEvent](after = 7, timeout = Some(1230.millis), limit = 333)) ==
      Uri("http://example.com/master/api/fatEvent?return=OrderFatEvent&delay=0&timeout=1.23&limit=333&after=7"))
  }

  "order" - {
    "overview" in {
      assert(masterUris.order.overview == Uri("http://example.com/master/api/order"))
    }

    "single" in {
      assert(masterUris.order(OrderId("ORDER-ID")) == Uri("http://example.com/master/api/order/ORDER-ID"))
      assert(masterUris.order(OrderId("/å")) == Uri("http://example.com/master/api/order/%2F%C3%A5"))
    }

    "list" in {
      assert(masterUris.order.list[Order[Order.State]] == Uri("http://example.com/master/api/order/?return=Order"))
    }
  }

  "workflow" - {
    "single" in {
      assert(masterUris.workflow(WorkflowPath("/A/B")) == Uri("http://example.com/master/api/workflow/A%2FB"))
    }

    "list" in {
      assert(masterUris.workflow.list[Workflow] == Uri("http://example.com/master/api/workflow/?return=Workflow"))
    }
  }

  "agent" - {
    "single" in {
      assert(masterUris.agent(AgentRefPath("/A/B")) == Uri("http://example.com/master/api/agent/A%2FB"))
    }

    "list" in {
      assert(masterUris.agent.list[AgentRef] == Uri("http://example.com/master/api/agent/?return=AgentRef"))
    }
  }

  "snapshot" - {
    "list" in {
      assert(masterUris.snapshot.list == Uri("http://example.com/master/api/snapshot/"))
    }
  }
}
