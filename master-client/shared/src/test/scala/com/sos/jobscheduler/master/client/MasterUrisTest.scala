package com.sos.jobscheduler.master.client

import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.data.event.{Event, EventRequest}
import com.sos.jobscheduler.data.fatevent.OrderFatEvent
import com.sos.jobscheduler.data.filebased.RepoEvent.FileBasedEvent
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class MasterUrisTest extends FreeSpec
{
  "Relative browser URI" in {
    assert(MasterUris("").overview == "api")
  }

  private val masterUris = MasterUris("http://example.com/master")

  "overview" in {
    assert(masterUris.overview == "http://example.com/master/api")
  }

  "command" in {
    assert(masterUris.command == "http://example.com/master/api/command")
  }

  "events" in {
    assert(masterUris.events(EventRequest.singleClass[OrderEvent](after = 7, timeout = Some(1230.millis))) ==
      "http://example.com/master/api/event?return=OrderEvent&delay=0&timeout=1.23&after=7")
    assert(masterUris.events(EventRequest.singleClass[OrderEvent](after = 7, timeout = Some(1230.millis), limit = 333)) ==
      "http://example.com/master/api/event?return=OrderEvent&delay=0&timeout=1.23&limit=333&after=7")

    // return EventId only
    assert(masterUris.events(EventRequest.singleClass[Event](after = 7, timeout = Some(1.seconds)), eventIdOnly = true) ==
      "http://example.com/master/api/event?eventIdOnly=true&return=Event&delay=0&timeout=1&after=7")
    assert(masterUris.events(EventRequest[Event](Set(classOf[OrderEvent], classOf[FileBasedEvent]), after = 7, Some(1.second)), eventIdOnly = true) ==
      "http://example.com/master/api/event?eventIdOnly=true&return=OrderEvent,FileBasedEvent&delay=0&timeout=1&after=7")
  }

  "journal" in {
    assert(masterUris.journal(fileEventId = 100, position = 111, timeout = Some(50.seconds)) ==
      "http://example.com/master/api/journal?timeout=50&file=100&position=111")
    assert(masterUris.journal(fileEventId = 100, position = 111, heartbeat = Some(22.seconds), markEOF = true, returnLength = true) ==
      "http://example.com/master/api/journal?return=length&heartbeat=22&markEOF=true&file=100&position=111")
  }

  "fatEvent" in {
    assert(masterUris.fatEvents(EventRequest.singleClass[OrderFatEvent](after = 7, timeout = Some(1230.millis))) ==
      "http://example.com/master/api/fatEvent?return=OrderFatEvent&delay=0&timeout=1.23&after=7")
    assert(masterUris.fatEvents(EventRequest.singleClass[OrderFatEvent](after = 7, timeout = Some(1230.millis), limit = 333)) ==
      "http://example.com/master/api/fatEvent?return=OrderFatEvent&delay=0&timeout=1.23&limit=333&after=7")
  }

  "order" - {
    "overview" in {
      assert(masterUris.order.overview == "http://example.com/master/api/order")
    }

    "single" in {
      assert(masterUris.order(OrderId("ORDER-ID")) == "http://example.com/master/api/order/ORDER-ID")
      assert(masterUris.order(OrderId("/å")) == "http://example.com/master/api/order/%2F%C3%A5")
    }

    "list" in {
      assert(masterUris.order.list[Order[Order.State]] == "http://example.com/master/api/order/?return=Order")
    }
  }

  "workflow" - {
    "single" in {
      assert(masterUris.workflow(WorkflowPath("/A/B")) == "http://example.com/master/api/workflow/A%2FB")
    }

    "list" in {
      assert(masterUris.workflow.list[Workflow] == "http://example.com/master/api/workflow/?return=Workflow")
    }
  }

  "agent" - {
    "single" in {
      assert(masterUris.agent(AgentRefPath("/A/B")) == "http://example.com/master/api/agent/A%2FB")
    }

    "list" in {
      assert(masterUris.agent.list[AgentRef] == "http://example.com/master/api/agent/?return=AgentRef")
    }
  }

  "snapshot" - {
    "list" in {
      assert(masterUris.snapshot.list == "http://example.com/master/api/snapshot/")
    }
  }
}
