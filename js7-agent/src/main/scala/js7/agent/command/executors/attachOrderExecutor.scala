package js7.agent.command.executors

import js7.agent.command.AgentCommandToEventCalc.CommandEventConverter
import js7.agent.data.commands.AgentCommand.AttachOrder
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.agent.Problems.AgentDuplicateOrder

private[command] def attachOrderExecutor =
  CommandEventConverter.checked[AttachOrder]: (cmd, agentState) =>
    import cmd.order
    order.attached.flatMap: agentPath =>
      if agentPath != agentState.meta.agentPath then
        Left(Problem(s"Wrong $agentPath"))
      else
        agentState.idToWorkflow.checked(order.workflowId).flatMap: workflow =>
          if !workflow.isDefinedAt(order.position) then
            Left(Problem.pure(s"Unknown Position ${order.workflowPosition}"))
          else if agentState.idToOrder.contains(order.id) then
            Left(AgentDuplicateOrder(order.id))
          else
            order.toOrderAttachedToAgent.map: event =>
              Some(order.id <-: event)
