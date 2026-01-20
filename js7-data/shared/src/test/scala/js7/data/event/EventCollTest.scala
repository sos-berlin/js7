package js7.data.event

import js7.base.crypt.silly.SillySigner
import js7.base.problem.Checked
import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.controller.ControllerState
import js7.data.event.EventDrivenState.EventNotApplicableProblem
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.TestEvent.{Added, InvalidEvent}
import js7.data.item.VersionId
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded}
import js7.data.order.OrderEvent.{OrderAdded, OrderFinished, OrderStarted}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}

final class EventCollTest extends OurTestSuite:

  "addEvent" in:
    val coll =
      val coll = EventCollCtx[TestState, TestEvent, Unit](TestState(""), ())
      locally:
        for
          coll <- coll.addEvent(Added("A"))
          coll <- coll.addEvent(Added(",B"))
        yield
          coll
      .orThrow
    assert(coll.aggregate == TestState("A,B"))
    assert(coll.keyedEvents.toVector == Vector(
      NoKey <-: Added("A"),
      NoKey <-: Added(",B")))

  "addEvent with failing event" in :
    val checked =
      val coll = EventCollCtx[TestState, TestEvent, Unit](TestState(""), ())
      locally:
        for
          coll <- coll.addEvent(Added("A"))
          coll <- coll.addEvent(InvalidEvent)
          coll <- coll.addEvent(Added(",B"))
        yield
          coll
    assert(checked == Left(EventNotApplicableProblem(InvalidEvent, TestState("A"))))

  "addEvents" in:
    val coll = EventCollCtx[TestState, TestEvent, Unit](TestState(""), ())
      .addEvents(Seq(Added("A"), Added(",B")))
      .orThrow
    assert(coll.aggregate == TestState("A,B"))
    assert(coll.keyedEvents.toVector == Vector(
      NoKey <-: Added("A"),
      NoKey <-: Added(",B")))

  "addWithKey" in:
    val orderId = OrderId("ORDER")
    val workflow = Workflow.of(WorkflowPath("W") ~ "1")
    val coll =
      val coll = EventCollCtx[ControllerState, Event, Unit](ControllerState.empty, ())
      val signer = ControllerState.toItemSigner(SillySigner.Default)
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

  "addEventCalc" in:
    val coll =
      locally:
        for
          _ <- Checked.unit
          coll = EventCollCtx[TestState, TestEvent, Unit](TestState(""), ())
          coll <- coll.addEvent(Added("A"))
          coll <- coll.addEventCalc(EventCalcCtx.pure(Added(",B")))
        yield
          coll
      .orThrow
    assert(coll.aggregate == TestState("A,B"))
    assert(coll.keyedEvents.toVector == Vector(
      NoKey <-: Added("A"),
      NoKey <-: Added(",B")))

  "addColl" in:
    val aColl =
      val coll = EventCollCtx[TestState, TestEvent, Unit](TestState("START:"), ())
      locally:
        for
          coll <- coll.addEvent(Added("A"))
        yield
          coll
      .orThrow
    val bColl =
      val coll = EventCollCtx[TestState, TestEvent, Unit](aColl.aggregate, ())
      locally:
        for
          coll <- coll.addEvent(Added(",B"))
        yield
          coll
      .orThrow
    val abColl = aColl.addColl(bColl).orThrow
    assert(abColl.originalAggregate == TestState("START:"))
    assert(abColl.aggregate == TestState("START:A,B"))
    assert(abColl.keyedEvents.toVector == Vector(
      NoKey <-: Added("A"),
      NoKey <-: Added(",B")))
    assert(abColl.keyedEvents.toVector == (aColl.keyedEvents ++ bColl.keyedEvents).toVector)
