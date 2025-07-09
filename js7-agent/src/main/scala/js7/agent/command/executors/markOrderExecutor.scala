package js7.agent.command.executors

import js7.agent.command.AgentCommandToEventCalc.CommandEventConverter
import js7.agent.data.commands.AgentCommand.MarkOrder
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.EventCalc
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.InstructionExecutorService

private[command] def markOrderExecutor =
  CommandEventConverter.checked[MarkOrder]: (cmd, agentState) =>
    import cmd.orderId
    agentState.idToOrder.checked(orderId).flatMap: order =>
      if order.isDetaching then
        Right(Nil)
      else
        OrderEventSource(agentState)(using InstructionExecutorService(EventCalc.context.clock))
          .markOrder(orderId, cmd.mark).map: events =>
            events.map(orderId <-: _)
