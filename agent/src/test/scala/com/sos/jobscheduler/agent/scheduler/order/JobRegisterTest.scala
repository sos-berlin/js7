package com.sos.jobscheduler.agent.scheduler.order

import com.sos.jobscheduler.common.scalautil.DuplicateKeyException
import com.sos.jobscheduler.data.order.OrderId
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JobRegisterTest extends FreeSpec {

  "OrderQueue" in {
    val q = new JobRegister.OrderQueue
    assert(q.dequeue().isEmpty)

    q += OrderId("A")
    assert(q.dequeue() contains OrderId("A"))
    assert(q.dequeue().isEmpty)

    q += OrderId("X")
    assert(q.dequeue() contains OrderId("X"))

    intercept[DuplicateKeyException] {
      q += OrderId("A")
    }

    q -= OrderId("A")
    q += OrderId("A")
    assert(q.dequeue() contains OrderId("A"))

    q += OrderId("B")
    assert(q.dequeue() contains OrderId("B"))
    q += OrderId("C")
    assert(q.dequeue() contains OrderId("C"))
    assert(q.dequeue().isEmpty)
  }
}
