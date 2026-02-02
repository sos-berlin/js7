package js7.data.execution.workflow.instructions

import cats.instances.either.*
import cats.instances.vector.*
import cats.syntax.flatMap.*
import cats.syntax.traverse.*
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.{AgentPath, AtController, AtControllerOrAgent}
import js7.data.event.{EventCalc, EventColl, KeyedEvent}
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.OrderEventSource.{fail, moveOrderToNextInstruction}
import js7.data.execution.workflow.instructions.EventInstructionExecutor.predictNextAgent
import js7.data.order.Order.{Cancelled, Forked}
import js7.data.order.OrderEvent.{OrderAttachable, OrderCoreEvent, OrderDetachable, OrderForked, OrderJoined}
import js7.data.order.{Order, OrderEvent, OrderId, OrderOutcome}
import js7.data.state.EngineEventColl.extensions.order
import js7.data.state.{EngineState, EngineState_}
import js7.data.value.Value
import js7.data.value.expression.Expression
import js7.data.workflow.instructions.ForkInstruction
import scala.reflect.ClassTag

trait ForkInstructionExecutor[Instr <: ForkInstruction]
extends EventInstructionExecutor_[Instr]:
  import ForkInstructionExecutor.*

  protected def toForkedEvent(
    instr: Instr,
    order: Order[Order.IsFreshOrReady],
    engineState: EngineState,
    now: Timestamp)
  : Checked[OrderForked]

  protected def forkResult(
    instr: Instr,
    order: Order[Order.Forked],
    engineState: EngineState,
    now: Timestamp)
  : OrderOutcome.Completed

  def toEventCalc[S <: EngineState_[S]](instr: Instr, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    useOrder(orderId): (coll, order) =>
      ifReadyOrStartable(order, coll.now).map: order =>
        coll:
          forkOrder(instr, order)
      .orElse:
        order.ifState[Order.Forked].map: order =>
          coll:
            toJoined(order, instr, coll.aggregate, coll.now)
      .orElse:
        order.ifState[Order.Processed].map: order =>
          if order.lastOutcome.isSucceeded then
            coll:
              moveOrderToNextInstruction(order)
          else
            coll:
              fail(order.id)
      .getOrElse:
        coll.nix

  private def forkOrder[S <: EngineState_[S]](instr: Instr, order: Order[Order.IsFreshOrReady])
  : EventCalc[S, OrderCoreEvent] =
    // toForkedEvent may be called three times:
    // 1) to predict the Agent and before OrderAttachable
    // 2) if Order.Fresh then OrderStart
    // 3) OrderForked
    // Call toForkedEvent not before needed, because it may require an Agent
    // (for subagentIds function)
    val orderId = order.id
    EventCalc: coll =>
      def checkedOrderForked(coll: EventColl[S, OrderCoreEvent]) =
        toForkedEvent(instr, order, coll.aggregate, coll.now).flatTap: orderForked =>
          checkOrderIdCollisions(orderForked, coll.aggregate)

      for
        maybeAttachOrDetach <-
          instr.agentPath.map: agentPath =>
            Right(Some(agentPath))
          .getOrElse:
            /// Predict Controller or Agent for execution of the fork instruction. ///
            // To achieve this, we try-run the fork instruction
            for
              orderForked <- checkedOrderForked(coll)
              coll <- start(coll, orderId): (coll, order) =>
                coll:
                  order.id <-: orderForked
              order <- coll.order(order.id).flatMap(_.checkedState[Forked])
              maybeControllerOrAgent <- predictControllerOrAgent(order, orderForked, coll)
              // The resulting coll is thrown away
            yield maybeControllerOrAgent
          .map:
            _.flatMap: controllerOrAgent =>
              attachOrDetach(order, controllerOrAgent)
        coll <-
          maybeAttachOrDetach.map: event =>
            coll:
              orderId <-: event
          .getOrElse:
            start(coll, orderId): (coll, order) =>
              for
                orderForked <- checkedOrderForked(coll)
                coll <- coll:
                  orderId <-: orderForked
              yield
                coll
      yield
        coll

  private def attachOrDetach(
    order: Order[Order.IsFreshOrReady],
    atControllerOrAgent: AtControllerOrAgent)
  : Option[OrderCoreEvent] =
    atControllerOrAgent match
      case AtController =>
        // Order must be at Controller
        order.isAttached ? OrderDetachable

      case agentPath: AgentPath =>
        // Order must be at agentPath
        order.attachedState match
          case Some(Order.Attached(`agentPath`)) =>
            None
          case Some(Order.Attached(_)) =>
            Some(OrderDetachable)
          case None =>
            Some(OrderAttachable(agentPath))
          case x =>
            throw new IllegalStateException(s"${order.id} Fork: Invalid attachedState=$x")

  override final def onReturnFromSubworkflow[S <: EngineState_[S]](fork: Instr, order: Order[Order.State])
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      coll:
        tryJoinChildOrder(fork, order, coll.aggregate, coll.now)

  private def tryJoinChildOrder(fork: Instr, childOrder: Order[Order.State], state: EngineState, now: Timestamp)
  : Option[KeyedEvent[OrderCoreEvent]] =
    if childOrder.isAttached then
      Some(childOrder.id <-: OrderDetachable)
    else
      childOrder.parent
        .flatMap(state.idToOrder.get)
        .flatMap(_.ifState[Order.Forked])
        .flatMap: parentOrder =>
          parentOrder.isDetached thenMaybe:
            toJoined(parentOrder, fork, state, now)

  private final def toJoined(
    order: Order[Order.Forked],
    fork: Instr,
    state: EngineState,
    now: Timestamp)
  : Option[KeyedEvent[OrderCoreEvent]] =
    if order.isAttached then
      Some(order.id <-: OrderDetachable)
    else
      order.state.children.forall(o => childOrderIsJoinable(state, order, o.orderId)) thenSome:
        val failedChildren = order.state.children.view
          .map(child => state.idToOrder(child.orderId))
          .filter(order => !order.lastOutcome.isSucceeded || order.isState[Cancelled])
          .toVector
        order.id <-: OrderJoined(
          if failedChildren.nonEmpty then
            OrderOutcome.Failed(Some(toJoinFailedMessage(failedChildren)))
          else
            forkResult(fork, order, state, now))

  private def childOrderIsJoinable(state: EngineState, parentOrder: Order[Order.Forked], childOrderId: OrderId): Boolean =
    state.idToOrder.get(childOrderId).exists: childOrder =>
     state.childOrderIsJoinable(childOrder, parentOrder)

  protected def calcResult(
    resultExpr: Map[String, Expression],
    childOrderId: OrderId,
    state: EngineState,
    now: Timestamp)
  : Checked[Vector[(String, Value)]] =
    state.idToOrder.checked(childOrderId).flatMap: childOrder =>
      resultExpr.toVector.traverse: (name, expr) =>
        for
          scope <- state.toImpureOrderExecutingScope(childOrder, now)
          value <- expr.eval(using scope)
        yield name -> value

  private def toJoinFailedMessage(failedChildren: Seq[Order[Order.State]]): String =
    failedChildren.view
      .take(3)  // Avoid huge error message
      .flatMap(order =>
        if order.isState[Cancelled] then
          Some(s"${order.id} has been cancelled")
        else
          Some(s"${order.id} ${order.lastOutcome.show}"))
      .mkString(";\n")

  /** Predict where to execute a Fork instruction.
    * <p>
    * Forking many children at Controller and then attached all to their first agent is inefficient.
    * Here we decide where to attach the forking order before generating child orders.
    * @return Right(None): No prediction, leave Order where it is
    */
  protected[instructions] final def predictControllerOrAgent[S <: EngineState_[S]](
    order: Order[Order.Forked],
    orderForked: OrderForked,
    coll: EventColl[S, OrderCoreEvent])
  : Checked[Option[AtControllerOrAgent]] =
    if orderForked.children.isEmpty then
      Right(None) // No children, Order can stay where it is
    else
      order.newForkedOrders(orderForked).traverse: order =>
        predictNextAgent(order, coll)
      .map(_.toSet)
      .map: controllerOrAgents =>
        // None means Controller or the location is irrelevant (not distinguishable for now)
        if controllerOrAgents.sizeIs == 1 then
          val controllerOrAgent = controllerOrAgents.head
          // All children start at the Controller or the same Agent
          val enoughChildren = orderForked.children.sizeIs >= MinimumChildCountForParentAttachment
          (order.attachedState, controllerOrAgent.maybeAgentPath) match
            case (Some(Order.Attached(agentPath)), Some(childrensAgentPath)) =>
              // If parent order's attachment and the orders first agent differs,
              // detach the parent order!
              (agentPath != childrensAgentPath) ?
                AtController // Transfer back to Controller

            case (Some(Order.Attached(_)), None) =>
              // If the orders first location is the Controller or irrelevant,
              // then we prefer to detach the parent order.
              enoughChildren ? AtController

            case (None, Some(childrensAgentPath)) =>
              // Attach the forking order to the children's agent
              enoughChildren ? childrensAgentPath

            case _ =>
              None
        else // Child orders may start at different agents
          // Transfer back to Controller
          // Inefficient if only one of many children changes the agent !!!
          order.isAttached ? AtController


object ForkInstructionExecutor:
  val MinimumChildCountForParentAttachment = 3
  private val logger = Logger[this.type]

  // Paranoid check. Will not work properly on Agent because Agent does not know all orders.
  // The Order child syntax is based on the reserved character '|'.
  // It is not possible to generate a child order but with Fork.
  // So we can safely generate child orders because only Fork can do this.
  private def checkOrderIdCollisions(orderForked: OrderForked, state: EngineState): Checked[Unit] =
    val duplicates = orderForked.children.map(_.orderId).flatMap(state.idToOrder.get)
    if duplicates.nonEmpty then
      // Internal error, maybe a lost OrderDetached event
      val problem = Problem.pure(s"Forked OrderIds duplicate existing ${duplicates mkString ", "}")
      logger.error(problem.toString)
      Left(problem)
    else
      Checked.unit


  //final class Cache:
  //  private val forkedToJoinableChildren = mutable.Map.empty[OrderId, mutable.Set[OrderId]]
  //
  //  private[ForkInstructionExecutor] def ensureEntry(
  //    parentOrderId: OrderId,
  //    init: => Iterable[OrderId])
  //  : this.Access =
  //    forkedToJoinableChildren.getOrElseUpdate(parentOrderId, init.to(mutable.Set))
  //    new Access(parentOrderId)
  //
  //  final class Access(parentOrderId: OrderId):
  //    private[ForkInstructionExecutor] def onChildBecameJoinable(childOrderId: OrderId): Unit =
  //      forkedToJoinableChildren(parentOrderId) += childOrderId
  //
  //    private[ForkInstructionExecutor] def numberOfJoinables: Int =
  //      forkedToJoinableChildren(parentOrderId).size
  //
  //    private[ForkInstructionExecutor] def onJoined(): Unit =
  //      forkedToJoinableChildren -= parentOrderId
