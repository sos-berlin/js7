package js7.data.event

import js7.base.crypt.silly.SillySigner
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.controller.ControllerState
import js7.data.event.EventDrivenState.EventNotApplicableProblem
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.TestEvent.{Added, InvalidEvent}
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded}
import js7.data.item.{ItemSigner, VersionId}
import js7.data.order.OrderEvent.{OrderAdded, OrderFinished, OrderStarted}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}

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

  "addWithKey" in:
    val orderId = OrderId("ORDER")
    val workflow = Workflow.of(WorkflowPath("W") ~ "1")
    val coll =
      val coll = EventColl[ControllerState, Event](ControllerState.empty)
      val signer = ItemSigner(SillySigner.Default, ControllerState.signableItemJsonCodec)
      locally:
        for
          coll <- coll.addNoKey:
            Vector(
              VersionAdded(VersionId("1")),
              VersionedItemAdded(signer.sign(workflow)))
          coll <- coll.addWithKey(orderId):
            Vector(
              OrderAdded(workflow.id),
              OrderStarted)
          coll <- coll.addWithKey(orderId):
            OrderFinished() :: Nil
        yield
          coll
      .orThrow
    assert(coll.aggregate.idToOrder.values.toSeq == Seq:
      Order(orderId, workflow.id /: Position(0), Order.Finished))

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
