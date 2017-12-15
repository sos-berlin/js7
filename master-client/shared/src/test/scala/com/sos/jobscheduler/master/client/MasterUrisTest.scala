package com.sos.jobscheduler.master.client

import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class MasterUrisTest extends FreeSpec {

  "Relative browser URI" in {
    assert(MasterUris("").overview == "api")
  }

  private val masterUris = MasterUris("http://example.com/master")

  "overview" in {
    assert(masterUris.overview == "http://example.com/master/api")
  }

  "order" - {
    "overview" in {
      assert(masterUris.order.overview == "http://example.com/master/api/order")
    }
    "single" in {
      assert(masterUris.order(OrderId("ORDER-ID")) == "http://example.com/master/api/order/ORDER-ID")
      assert(masterUris.order(OrderId("/å")) == "http://example.com/master/api/order/%2F%C3%A5")
    }

    "event" in {
      assert(masterUris.order.events[OrderEvent](after = 7, timeout = 1230.millis ) ==
        "http://example.com/master/api/order?return=OrderEvent&timeout=1.23&after=7")
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
}
