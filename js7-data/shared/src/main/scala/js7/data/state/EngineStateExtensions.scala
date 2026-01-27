package js7.data.state

import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.KeyedEvent
import js7.data.order.OrderEvent.OrderDetachable
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
