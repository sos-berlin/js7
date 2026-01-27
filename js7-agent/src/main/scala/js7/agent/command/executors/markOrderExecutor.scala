package js7.agent.command.executors

import js7.agent.command.AgentCommandToEventCalc.CommandEventConverter
import js7.agent.data.commands.AgentCommand.MarkOrder
import js7.data.execution.workflow.OrderEventSource
import js7.data.state.EngineEventColl.extensions.order

private[command] def markOrderExecutor =
  CommandEventConverter.coll[MarkOrder]: (cmd, coll) =>
    import cmd.orderId
    for
      order <- coll.order(orderId)
      coll <-
        if order.isDetaching then
          coll.nix
        else
          coll:
            OrderEventSource.markOrder(orderId, cmd.mark)
    yield coll