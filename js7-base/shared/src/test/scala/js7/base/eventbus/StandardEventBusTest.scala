package js7.base.eventbus

import js7.base.eventbus.StandardEventBusTest.*
import monix.execution.Scheduler.Implicits.traced
import js7.base.test.OurAsyncTestSuite
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class StandardEventBusTest extends OurAsyncTestSuite
{
  "subscribe" in {
    val events = mutable.Buffer.empty[AnyRef]
    val eventBus = new StandardEventBus[Any]
    eventBus.publish("ignored")

    val stringSubscription = eventBus.subscribeToClass(classOf[String])(events += _)
    val bStringSubscription = eventBus.subscribe[String] { events += _ + "-B" }
    val aSubscription = eventBus.subscribe[A] { events += _ }
    eventBus.publish("1")
    eventBus.publish(A("2"))
    bStringSubscription.close()
    eventBus.publish("3")
    bStringSubscription.close()
    eventBus.publish("4")
    eventBus.publish(A("5"))

    assert(events.toList == List("1", "1-B", A("2"), "3", "4", A("5")))

    assert(!eventBus.isEmpty)
    stringSubscription.close()
    aSubscription.close()
    assert(eventBus.isEmpty)
  }

  "oneShot" in {
    type Event = String
    val events = mutable.Buffer.empty[Event]
    val eventBus = new StandardEventBus[Event]
    eventBus.oneShot[Event]()(events += _)
    assert(!eventBus.isEmpty)
    eventBus.publish("ONE")
    assert(eventBus.isEmpty)
    eventBus.publish("TWO")
    assert(events.toList == List("ONE"))
  }

  "when" in {
    val eventBus = new StandardEventBus[Any]
    val future = eventBus.when[String].runToFuture
    assert(!future.isCompleted)
    assert(!eventBus.isEmpty)
    eventBus.publish("ONE")
    eventBus.publish("ONE")
    for (string <- future) yield {
      assert(string == "ONE")
      assert(eventBus.isEmpty)
    }
  }
}

private object StandardEventBusTest
{
  private case class A(string: String)
}
