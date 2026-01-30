package js7.data.state

import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.KeyedEvent
import js7.data.lock.LockState
import js7.data.order.OrderEvent.{OrderDetachable, OrderForked, OrderLocksReleased, OrderOrderAdded}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.workflow.Instruction
import scala.reflect.ClassTag

object EngineStateExtensions:

  private val logger = Logger[this.type]

  extension (engineState: EngineState)

    def instruction_[A <: Instruction : ClassTag](orderId: OrderId): Checked[A] =
      for
        order <- engineState.idToOrder.checked(orderId)
        instr <- engineState.instruction_[A](order.workflowPosition)
      yield instr

    def atController[E <: OrderEvent](orderId: OrderId)(events: => E)
    : KeyedEvent[OrderDetachable | E] =
      if engineState.isAgent then
        orderId <-: OrderDetachable
      else
        orderId <-: events

    def atController[E <: OrderEvent](orderId: OrderId)(events: => List[E])
    : List[KeyedEvent[OrderDetachable | E]] =
      if engineState.isAgent then
        (orderId <-: OrderDetachable) :: Nil
      else
        events.map(orderId <-: _)

    def atController[E <: OrderEvent](orderId: OrderId)(events: => Checked[List[KeyedEvent[E]]])
    : Checked[List[KeyedEvent[OrderDetachable | E]]] =
      if engineState.isAgent then
        Right:
          (orderId <-: OrderDetachable) :: Nil
      else
        events

    def atController[E <: OrderEvent](event: => E): OrderDetachable | E =
      if engineState.isAgent then
        OrderDetachable
      else
        event

    def atController[E <: OrderEvent](events: => List[E]): List[OrderDetachable | E] =
      if engineState.isAgent then
        OrderDetachable :: Nil
      else
        events

    def atController[E <: OrderEvent](events: => Checked[List[E]])
    : Checked[List[OrderDetachable | E]] =
      if engineState.isAgent then
        Right(OrderDetachable :: Nil)
      else
        events

    def orderToInstruction[A <: Instruction : ClassTag](orderId: OrderId): Checked[A] =
      for
        order <- engineState.idToOrder.checked(orderId)
        instr <- engineState.instruction_[A](order.workflowPosition)
      yield
        instr

    //def keyedEventToPendingOrderIds(keyedEvents: Iterable[KeyedEvent[OrderEvent]]): Seq[OrderId] =
    //  keyedEvents.toVector.flatMap(keyedEventToPendingOrderIds(_))

    /** Return the OrderIds which should continue after the given Event. */
    def keyedEventToPendingOrderIds(keyedEvent: KeyedEvent[OrderEvent]): Seq[OrderId] =
      // Only ControllerState events, for now
      keyedEvent.event.match
        case OrderLocksReleased(lockPaths) => // Controller only
          // TODO Returning ALL queued OrderIds may not be efficient,
          //  if only one or some of them will get the lock.
          keyedEvent.key +:
            lockPaths
              .flatMap: lockPath =>
                engineState.keyTo(LockState).get(lockPath)
              .flatMap(_.queue /*OrderIds queuing for this lock*/)

        case OrderForked(children) =>
          children.map(_.orderId) :+ keyedEvent.key /*add forking order in case of empty Fork*/

        case orderOrderAdded: OrderOrderAdded => // Controller only
          keyedEvent.key :: orderOrderAdded.orderId :: Nil

        case _ =>
          keyedEvent.key :: Nil
