package js7.agent.scheduler.order

import js7.base.utils.DuplicateKeyException
import js7.data.order.OrderId
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JobRegisterTest extends AnyFreeSpec {

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
