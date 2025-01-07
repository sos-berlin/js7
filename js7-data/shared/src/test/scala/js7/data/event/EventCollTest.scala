package js7.data.event

import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.event.EventDrivenState.EventNotApplicableProblem
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.TestEvent.{Added, InvalidEvent}

final class EventCollTest extends OurTestSuite:

  "addEvent" in:
    val coll =
      val coll = EventColl[TestState, TestEvent](TestState(""))
      locally:
        for
          coll <- coll.addEvent(Added("A"))
          coll <- coll.addEvent(Added(",B"))
        yield
          coll
      .orThrow
    assert(coll.aggregate == TestState("A,B"))
    assert(coll.keyedEvents == Vector(
      NoKey <-: Added("A"),
      NoKey <-: Added(",B")))

  "addEvent with failing event" in :
    val checked =
      val coll = EventColl[TestState, TestEvent](TestState(""))
      locally:
        for
          coll <- coll.addEvent(Added("A"))
          coll <- coll.addEvent(InvalidEvent)
          coll <- coll.addEvent(Added(",B"))
        yield
          coll
    assert(checked == Left(EventNotApplicableProblem(InvalidEvent, TestState("A"))))

  "addEvents" in:
    val coll =
      val coll = EventColl[TestState, TestEvent](TestState(""))
      locally:
        for
          coll <- coll.addEvents(Seq(Added("A"), Added(",B")))
        yield
          coll
      .orThrow
    assert(coll.aggregate == TestState("A,B"))
    assert(coll.keyedEvents == Vector(
      NoKey <-: Added("A"),
      NoKey <-: Added(",B")))

  "append" in:
    val aColl =
      val coll = EventColl[TestState, TestEvent](TestState("START:"))
      locally:
        for
          coll <- coll.addEvent(Added("A"))
        yield
          coll
      .orThrow
    val bColl =
      val coll = EventColl[TestState, TestEvent](aColl.aggregate)
      locally:
        for
          coll <- coll.addEvent(Added(",B"))
        yield
          coll
      .orThrow
    val abColl = aColl.append(bColl).orThrow
    assert(abColl.originalAggregate == TestState("START:"))
    assert(abColl.aggregate == TestState("START:A,B"))
    assert(abColl.keyedEvents == Vector(
      NoKey <-: Added("A"),
      NoKey <-: Added(",B")))
    assert(abColl.keyedEvents == aColl.keyedEvents ++ bColl.keyedEvents)
