package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.event.KeyedEvent
import js7.data.order.OrderEvent.OrderActorEvent
import js7.data.order.OrderId
import js7.data.state.EngineState
import scala.reflect.ClassTag

object InstructionExecuterTester:
  object syntax:
    implicit final class EventInstructionExecutorForTest(
      private val instructionExecutor: EventInstructionExecutor)
    extends AnyVal:
      def testToEvents(orderId: OrderId, engineState: EngineState)
      : Checked[List[KeyedEvent[OrderActorEvent]]] =
        engineState
          .idToOrder.checked(orderId)
          .flatMap(order => engineState
            .instruction_(order.workflowPosition)(using ClassTag(instructionExecutor.instructionClass))
            .flatMap(instructionExecutor.toEvents(_, order, engineState)))
