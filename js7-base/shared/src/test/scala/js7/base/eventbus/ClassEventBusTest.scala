package js7.base.eventbus

import js7.base.eventbus.ClassEventBusTest.*
import monix.execution.Scheduler.Implicits.traced
import js7.base.test.OurAsyncTestSuite
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class ClassEventBusTest extends OurAsyncTestSuite
{
  "subscribe" in {
    val events = mutable.Buffer.empty[AnyRef]
    val eventBus = new TestEventBus

    val aSubscription = eventBus.subscribeToClass(classOf[AClassifier])(events += _)
    val bSubscription = eventBus.subscribe[BClassifier] { events += _.toString + "-B" }
    eventBus.publish(Event(AClassifier("1"), "ONE"))
    eventBus.publish(Event(BClassifier(""), "TWO"))

    assert(events.toList == List(Event(AClassifier("1"), "ONE"), "Event(BClassifier(),TWO)-B"))

    assert(!eventBus.isEmpty)
    aSubscription.close()
    bSubscription.close()
    assert(eventBus.isEmpty)
  }

  "oneShot" in {
    val events = mutable.Buffer.empty[Event[? <: Classifier]]
    val eventBus = new TestEventBus
    eventBus.oneShot[BClassifier]()((event: Event[BClassifier]) => events += event)
    eventBus.publish(Event(AClassifier("1"), "IGNORE"))
    eventBus.publish(Event(BClassifier(""), "MORE"))
    assert(events.toList == List(Event(BClassifier(""), "MORE")))
  }

  "when" in {
    val eventBus = new TestEventBus
    val future = eventBus.when[BClassifier].runToFuture
    assert(!future.isCompleted)
    assert(!eventBus.isEmpty)
    eventBus.publish(Event(AClassifier("1"), "IGNORE"))
    eventBus.publish(Event(BClassifier(""), "MORE"))
    for (event <- future) yield {
      assert(event == Event(BClassifier(""), "MORE"))
    }
  }

  "superclass" in {
    val events = mutable.Buffer.empty[(Classifier, String)]
    val eventBus = new TestEventBus
    eventBus.subscribe[Classifier](event => events += event.classifier -> "")
    eventBus.subscribe[AClassifier](event => events += event.classifier -> "A")
    eventBus.subscribe[BClassifier](event => events += event.classifier -> "B")
    val ab = eventBus.subscribe[ABClassifier](event => events += event.classifier -> "AB")
    eventBus.publish(Event(AClassifier("1")))
    eventBus.publish(Event(BClassifier("2")))
    eventBus.publish(Event(CClassifier("3")))
    eventBus.publish(Event(AClassifier("4")))
    assert(events.toSet == Set(
      AClassifier("1") -> "",
      AClassifier("1") -> "AB",
      AClassifier("1") -> "A",

      BClassifier("2") -> "",
      BClassifier("2") -> "AB",
      BClassifier("2") -> "B",

      CClassifier("3") -> "",

      AClassifier("4") -> "",
      AClassifier("4") -> "AB",
      AClassifier("4") -> "A"))

    ab.close()
    events.clear()
    eventBus.publish(Event(AClassifier("X")))
    assert(events.toSet == Set(
      AClassifier("X") -> "",
      AClassifier("X") -> "A"))
  }
}

private object ClassEventBusTest
{
  private final class TestEventBus extends ClassEventBus[Event[Classifier]]
  {
    protected type Classifier = ClassEventBusTest.this.Classifier
    protected type ClassifierToEvent[C <: Classifier] = Event[C]

    protected def classifierSuperclass = classOf[ClassEventBusTest.this.Classifier]

    protected def classify(event: Event[Classifier]) =
      event.classifier.getClass.asInstanceOf[Class[? <: Classifier]]
  }
  private case class Event[+C <: Classifier](classifier: C, more: String = "")
  private sealed trait Classifier
  private sealed trait ABClassifier extends Classifier
  private final case class AClassifier(string: String) extends ABClassifier
  private case class BClassifier(string: String) extends ABClassifier
  private case class CClassifier(string: String) extends Classifier
}
