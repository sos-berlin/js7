package js7.data.execution.workflow.instructions

import cats.instances.either.*
import cats.instances.vector.*
import cats.syntax.traverse.*
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.ForkInstructionExecutor.*
import js7.data.order.Order.Cancelled
import js7.data.order.OrderEvent.{OrderActorEvent, OrderAttachable, OrderDetachable, OrderFailedIntermediate_, OrderForked, OrderJoined, OrderMoved, OrderStarted}
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.state.StateView
import js7.data.value.Value
import js7.data.value.expression.Expression
import js7.data.workflow.instructions.ForkInstruction
import scala.collection.mutable

trait ForkInstructionExecutor extends EventInstructionExecutor:
  type Instr <: ForkInstruction

  protected val service: InstructionExecutorService
  private implicit val implicitService: InstructionExecutorService = service

  protected def toForkedEvent(fork: Instr, order: Order[Order.IsFreshOrReady], state: StateView)
  : Checked[OrderForked]

  protected def forkResult(fork: Instr, order: Order[Order.Forked], state: StateView,
    now: Timestamp): Outcome.Completed

  final def toEvents(fork: Instr, order: Order[Order.State], state: StateView) =
    readyOrStartable(order)
      .map(order =>
        for event <- forkOrder(fork, order, state) yield
          (order.id <-: event) :: Nil)
      .orElse(
        for
          order <- order.ifState[Order.Forked]
          joined <- toJoined(order, fork, state)
        yield Right(joined :: Nil))
      .orElse(
        for order <- order.ifState[Order.Processed] yield {
          val event = if order.lastOutcome.isSucceeded then
            OrderMoved(order.position.increment)
          else
            OrderFailedIntermediate_()
          Right((order.id <-: event) :: Nil)
        })
      .getOrElse(Right(Nil))

  private def forkOrder(fork: Instr, order: Order[Order.IsFreshOrReady], state: StateView)
  : Checked[OrderActorEvent] =
    // toForkedEvent may be called three times:
    // 1) to predict the Agent and before OrderAttachable
    // 2) if Order.Fresh then OrderStart
    // 3) OrderForked
    // Call toForkedEvent not before needed, because it may require an Agent
    // (for subagentIds function)
    lazy val checkedOrderForked = toForkedEvent(fork, order, state)
    for
      maybe <- fork.agentPath
        .map(agentPath => Right(Some(Some(agentPath))))
        .getOrElse(
          checkedOrderForked.flatMap(
            predictControllerOrAgent(order, _, state)))
        .map(_.flatMap(controllerOrAgent =>
          attachOrDetach(order, controllerOrAgent)))
      event <- maybe
        .map(event => Right(event))
        .getOrElse(
          for
            orderForked <- checkedOrderForked
            _ <- checkOrderIdCollisions(orderForked, state)
          yield
            if order.isState[Order.Fresh] then
              OrderStarted
            else
              orderForked)
    yield event

  private def attachOrDetach(
    order: Order[Order.IsFreshOrReady],
    agentPath: Option[AgentPath])
  : Option[OrderActorEvent] =
    agentPath match
      case None =>
        // Order must be at Controller
        order.isAttached ? OrderDetachable

      case Some(agentPath) =>
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

  override final def onReturnFromSubworkflow(
    fork: Instr,
    childOrder: Order[Order.State],
    state: StateView) =
    Right(
      tryJoinChildOrder(fork, childOrder, state)
        .toList)

  private def tryJoinChildOrder(fork: Instr, childOrder: Order[Order.State], state: StateView)
  : Option[KeyedEvent[OrderActorEvent]] =
    if childOrder.isAttached then
      Some(childOrder.id <-: OrderDetachable)
    else
      childOrder.parent
        .flatMap(state.idToOrder.get)
        .flatMap(_.ifState[Order.Forked])
        .flatMap(parentOrder =>
          if !parentOrder.isDetached then
            None
          else
            withCacheAccess(parentOrder, state) { cache =>
              cache.onChildBecameJoinable(childOrder.id)
              toJoined(parentOrder, fork, state)
            })

  protected[instructions] final def toJoined(
    order: Order[Order.Forked],
    fork: Instr,
    state: StateView)
  : Option[KeyedEvent[OrderActorEvent]] =
    if order.isAttached then
      Some(order.id <-: OrderDetachable)
    else
      withCacheAccess(order, state) { cache =>
        (order.state.children.sizeIs == cache.numberOfJoinables) ? {
          cache.onJoined()
          val failedChildren = order.state.children.view
            .map(child => state.idToOrder(child.orderId))
            .filter(order => !order.lastOutcome.isSucceeded || order.isState[Cancelled])
            .toVector
          val now = clock.now()
          order.id <-: OrderJoined(
            if failedChildren.nonEmpty then
              Outcome.Failed(Some(toJoinFailedMessage(failedChildren)))
            else
              forkResult(fork, order, state, now))
        }
      }

  protected def calcResult(resultExpr: Map[String, Expression], childOrderId: OrderId,
    state: StateView, now: Timestamp)
  : Checked[Vector[(String, Value)]] =
    state.idToOrder.checked(childOrderId)
      .flatMap(childOrder =>
        resultExpr
          .toVector
          .traverse { case (name, expr) =>
            for
              scope <- state.toImpureOrderExecutingScope(childOrder, now)
              value <- expr.eval(scope)
            yield name -> value
          })

  private def toJoinFailedMessage(failedChildren: Seq[Order[Order.State]]): String =
    failedChildren.view
      .take(3)  // Avoid huge error message
      .flatMap(order =>
        if order.isState[Cancelled] then
          Some(s"${order.id} has been cancelled")
        else
          Some(s"${order.id} ${order.lastOutcome.show}"))
      .mkString(";\n")

  private def withCacheAccess[A](order: Order[Order.Forked], state: StateView)
    (body: service.forkCache.Access => A)
  : A =
    service.forkCache.synchronized:
      val entry = service.forkCache.ensureEntry(
        order.id,
        order.state.children
          .view
          .map(o => state.idToOrder(o.orderId))
          .filter(state.childOrderEnded(_, parent = order))
          .map(_.id))
      body(entry)

  // Forking many children at Controller and then attached all to their first agent is inefficient.
  // Here we decide where to attach the forking order before generating child orders.
  // Right(None): No prediction
  // Right(Some(None)): Controller
  protected[instructions] final def predictControllerOrAgent(
    order: Order[Order.IsFreshOrReady],
    orderForked: OrderForked,
    state: StateView)
  : Checked[Option[Option[AgentPath]]] =
    if orderForked.children.sizeIs == 0 then
      Right(None) // No children, Order can stay where it is
    else
      val eventSource = new OrderEventSource(state)
      order
        .newForkedOrders(orderForked)
        .toVector
        .traverse(eventSource.nextAgent)
        .map(_.toSet)
      .map(controllerOrAgents =>
        // None means Controller or the location is irrelevant (not distinguishable for now)
        if controllerOrAgents.sizeIs == 1 then {
          // All children start at the Controller or the same Agent
          val enoughChildren = orderForked.children.sizeIs >= MinimumChildCountForParentAttachment
          (order.attachedState, controllerOrAgents.head) match {
            case (Some(Order.Attached(agentPath)), Some(childrensAgentPath)) =>
              // If parent order's attachment and the orders first agent differs,
              // detach the parent order!
              (agentPath != childrensAgentPath) ?
                None // Transfer back to Controller

            case (Some(Order.Attached(_)), None) =>
              // If the orders first location is the Controller or irrelevant,
              // then we prefer to detach the parent order.
              enoughChildren ? None

            case (None, Some(childrensAgentPath)) =>
              // Attach the forking order to the children's agent
              enoughChildren ? Some(childrensAgentPath)

            case _ =>
              None
          }
        } else // Child orders may start at different agents
          // Transfer back to Controller
          // Inefficient if only one of many children change the agent !!!
          order.isAttached ? None)


object ForkInstructionExecutor:
  val MinimumChildCountForParentAttachment = 3
  private val logger = Logger[this.type]

  // Paranoid check. Will not work properly on Agent because Agent does not know all orders.
  // The Order child syntax is based on the reserved character '|'.
  // It is not possible to generate a child order but with Fork.
  // So we can safely generate child orders because only Fork can do this.
  private def checkOrderIdCollisions(orderForked: OrderForked, state: StateView): Checked[Unit] =
    val duplicates = orderForked.children.map(_.orderId).flatMap(state.idToOrder.get)
    if duplicates.nonEmpty then
      // Internal error, maybe a lost OrderDetached event
      val problem = Problem.pure(s"Forked OrderIds duplicate existing ${duplicates mkString ", "}")
      logger.error(problem.toString)
      Left(problem)
    else
      Checked.unit

  final class Cache:
    private val forkedToJoinableChildren = mutable.Map.empty[OrderId, mutable.Set[OrderId]]

    private[ForkInstructionExecutor] def ensureEntry(
      parentOrderId: OrderId,
      init: => Iterable[OrderId])
    : this.Access =
      forkedToJoinableChildren.getOrElseUpdate(parentOrderId, init.to(mutable.Set))
      new Access(parentOrderId)

    final class Access(parentOrderId: OrderId):
      private[ForkInstructionExecutor] def onChildBecameJoinable(childOrderId: OrderId): Unit =
        forkedToJoinableChildren(parentOrderId) += childOrderId

      private[ForkInstructionExecutor] def numberOfJoinables: Int =
        forkedToJoinableChildren(parentOrderId).size

      private[ForkInstructionExecutor] def onJoined(): Unit =
        forkedToJoinableChildren -= parentOrderId
