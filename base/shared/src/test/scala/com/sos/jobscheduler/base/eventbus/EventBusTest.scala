package com.sos.jobscheduler.base.eventbus

import com.sos.jobscheduler.base.eventbus.EventBusTest._
import scala.collection.mutable
import scala.util.Success
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class EventBusTest extends AnyFreeSpec
{
  "subscribe" in {
    val events = mutable.Buffer[AnyRef]()
    val eventBus = new EventBus
    eventBus.publish("ignored")

    val stringSubscription = eventBus.subscribeToClass(classOf[String])(events += _)
    val bStringSubscription = eventBus.subscribe[String] { events += _ + "-B" }
    val aSubscription = eventBus.subscribe { a: A => events += a }
    eventBus.publish("1")
    eventBus.publish(A("2"))
    bStringSubscription.close()
    eventBus.publish("3")
    bStringSubscription.close()
    eventBus.publish("4")
    eventBus.publish(A("5"))

    assert(events.toList == List("1", "1-B", A("2"), ("3"), "4", A("5")))

    assert(!eventBus.isEmpty)
    stringSubscription.close()
    aSubscription.close()
    assert(eventBus.isEmpty)
  }

  "oneShot" in {
    val events = mutable.Buffer[AnyRef]()
    val eventBus = new EventBus
    eventBus.oneShot[String](events += _)
    assert(!eventBus.isEmpty)
    eventBus.publish("ONE")
    assert(eventBus.isEmpty)
    eventBus.publish("TWO")
    assert(events.toList == List("ONE"))
  }

  "when" in {
    val eventBus = new EventBus
    val future = eventBus.when[String]
    assert(!future.isCompleted)
    assert(!eventBus.isEmpty)
    eventBus.publish("ONE")
    assert(future.isCompleted)
    assert(future.value == Some(Success("ONE")))
    assert(eventBus.isEmpty)
  }
}

private object EventBusTest
{
  private case class A(string: String)
}
