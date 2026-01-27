package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.implicitClass
import js7.data.agent.AgentPath
import js7.data.controller.ControllerState
import js7.data.event.EventColl.extensions.now
import js7.data.event.{EventCalc, EventColl, KeyedEvent}
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.OrderEventSource.fail
import js7.data.order.OrderEvent.{OrderAttachable, OrderCoreEvent, OrderDetachable, OrderStarted}
import js7.data.order.{Order, OrderEvent, OrderId, OrderOutcome}
import js7.data.state.EngineEventColl.extensions.{order, orderAndWorkflow}
import js7.data.state.EngineState_
import js7.data.workflow.{Instruction, Workflow}
import scala.reflect.ClassTag

private trait EventInstructionExecutor extends InstructionExecutor:

  type Instr <: Instruction
  val instructionClass: Class[Instr]

  def toEventCalc[S <: EngineState_[S]](instr: Instr, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent]

  override def onReturnFromSubworkflow[S <: EngineState_[S]](instr: Instr, order: Order[Order.State])
  : EventCalc[S, OrderCoreEvent] =
    EventCalc.empty

  protected final def start[S <: EngineState_[S]](
    coll: EventColl[S, OrderCoreEvent],
    orderId: OrderId)
    (body: (EventColl[S, OrderCoreEvent], Order[Order.State]) =>
      Checked[EventColl[S, OrderCoreEvent]])
  : Checked[EventColl[S, OrderCoreEvent]] =
    coll.order(orderId).flatMap: order =>
      if order.isStarted then
        body(coll, order)
      else if order.isStartable(coll.now) then
        for
          coll <- coll:
            order.id <-: OrderStarted
          order <- coll.order(orderId)
          coll <- body(coll, order)
        yield
          coll
      else
        coll.nix

  protected final def attach[S <: EngineState_[S]](
    order: Order[Order.State],
    agentPath: AgentPath)
    (body: => EventCalc[S, OrderCoreEvent])
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      if order.isDetached then
        coll:
          order.id <-: OrderAttachable(agentPath)
      else
        coll:
          body

  protected final def detachOrder[S <: EngineState_[S]](orderId: OrderId)
    (body: (EventColl[ControllerState, OrderCoreEvent], Order[Order.State]) =>
      Checked[EventColl[ControllerState, OrderCoreEvent]])
  : EventCalc[S, OrderCoreEvent] =
    useOrder(orderId): (coll, order) =>
      detach(coll, order): coll =>
        body(coll, order)

  protected final def detach[S <: EngineState_[S]](
    coll: EventColl[S, OrderCoreEvent],
    order: Order[Order.State])
    (body: EventColl[ControllerState, OrderCoreEvent] =>
      Checked[EventColl[ControllerState, OrderCoreEvent]])
  : Checked[EventColl[S, OrderCoreEvent]] =
    if order.isAttached then
      coll:
        order.id <-: OrderDetachable
    else if order.isDetached then
      coll.narrowAggregate[ControllerState]
        .flatMap(body)
        .map(_.widen)
    else
      coll.nix

  protected final def useOrderAndWorkflow[S <: EngineState_[S]](orderId: OrderId)
    (body: (EventColl[S, OrderCoreEvent], Order[Order.State], Workflow) => Checked[EventColl[S, OrderCoreEvent]])
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      coll.orderAndWorkflow(orderId).flatMap: (order, workflow) =>
        body(coll, order, workflow)

  protected final def useOrder[S <: EngineState_[S]](orderId: OrderId)
    (body: (EventColl[S, OrderCoreEvent], Order[Order.State]) => Checked[EventColl[S, OrderCoreEvent]])
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      coll.order(orderId).flatMap: order =>
        body(coll, order)

  protected final def ifReadyOrStartable(order: Order[Order.State], now: Timestamp)
  : Option[Order[Order.IsFreshOrReady]] =
    order.ifState[Order.Ready]
      .orElse:
        order.ifState[Order.Fresh]
          .filterNot(_.isDelayed(now))

  /** On problem, let the order properly fail, and continue execution.<p>
    * Normally, a failed `EventColl` would result in a `OutcomeDisrupted`.
    * `catchProblemAsFailure` emits a `Outcome.Failed` instead.
    */
  protected final def catchProblemAsFailure[S <: EngineState_[S]](
    coll: EventColl[S, OrderCoreEvent],
    orderId: OrderId)
    (checked: Checked[EventColl[S, OrderCoreEvent]])
  : Checked[EventColl[S, OrderCoreEvent]]  =
    checked.recoverProblem: problem =>
      coll:
        fail(orderId, Some(OrderOutcome.Failed.fromProblem(problem)))


private trait EventInstructionExecutor_[Instr_ <: Instruction](using ClassTag[Instr_])
extends EventInstructionExecutor:

  final type Instr = Instr_

  final val instructionClass = implicitClass[Instr]
