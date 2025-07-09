package js7.agent.command.executors

import js7.agent.command.AgentCommandToEventCalc.CommandEventConverter
import js7.agent.data.commands.AgentCommand.DetachOrder
import js7.data.order.OrderEvent.OrderDetached

private[command] def detachOrderExecutor =
  CommandEventConverter.checked[DetachOrder]: (cmd, agentState) =>
    import cmd.orderId
    agentState.idToOrder.get(orderId).fold(Right(Nil)): order =>
      order.detaching.map: _ =>
        (orderId <-: OrderDetached) :: Nil
