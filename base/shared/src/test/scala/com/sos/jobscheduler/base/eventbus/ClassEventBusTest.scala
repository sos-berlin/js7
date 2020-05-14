package com.sos.jobscheduler.base.eventbus

import com.sos.jobscheduler.base.eventbus.ClassEventBusTest._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AsyncFreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class ClassEventBusTest extends AsyncFreeSpec
{
  "subscribe" in {
    val events = mutable.Buffer[AnyRef]()
    val eventBus = new TestEventBus

    val aSubscription = eventBus.subscribeToClass(classOf[AClassifier])(events += _)
    val bSubscription = eventBus.subscribe[BClassifier] { events += _.toString + "-B" }
    eventBus.publish(Event(AClassifier("1"), "ONE"))
    eventBus.publish(Event(BClassifier, "TWO"))

    assert(events.toList == List(Event(AClassifier("1"), "ONE"), "Event(BClassifier,TWO)-B"))

    assert(!eventBus.isEmpty)
    aSubscription.close()
    bSubscription.close()
    assert(eventBus.isEmpty)
  }

  "oneShot" in {
    val events = mutable.Buffer[Event[_ <: Classifier]]()
    val eventBus = new TestEventBus
    eventBus.oneShot[BClassifier]((event: Event[BClassifier]) => events += event)
    eventBus.publish(Event(AClassifier("1"), "IGNORE"))
    eventBus.publish(Event(BClassifier, "MORE"))
    assert(events.toList == List(Event(BClassifier, "MORE")))
  }

  "when" in {
    val eventBus = new TestEventBus
    val future = eventBus.when[BClassifier].runToFuture
    assert(!future.isCompleted)
    assert(!eventBus.isEmpty)
    eventBus.publish(Event(AClassifier("1"), "IGNORE"))
    eventBus.publish(Event(BClassifier, "MORE"))
    for (event <- future) yield {
      assert(event == Event(BClassifier, "MORE"))
    }
  }
}

private object ClassEventBusTest
{
  private final class TestEventBus extends ClassEventBus[Event[Classifier]]
  {
    protected type Classifier = ClassEventBusTest.this.Classifier
    protected type ClassifierToEvent[C <: Classifier] = Event[C]

    protected def classify(event: Event[Classifier]) =
      event.classifier.getClass.asInstanceOf[Class[_ <: Classifier]]
  }
  private case class Event[+C <: Classifier](classifier: C, more: String)
  private sealed trait Classifier
  private final case class AClassifier(string: String) extends Classifier
  private type BClassifier = BClassifier.type
  private case object BClassifier extends Classifier
}
