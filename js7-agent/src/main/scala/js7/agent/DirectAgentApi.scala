package js7.agent

import js7.agent.command.CommandHandler
import js7.agent.data.AgentApi
import js7.agent.data.commands.AgentCommand
import js7.agent.data.views.AgentOverview
import js7.agent.scheduler.AgentHandle
import js7.base.problem.Checked
import js7.core.command.CommandMeta
import js7.data.command.{CommandHandlerDetailed, CommandHandlerOverview}
import js7.data.order.{Order, OrderId}
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
final class DirectAgentApi(commandHandler: CommandHandler, agentHandle: AgentHandle, meta: CommandMeta)
extends AgentApi
{
  def commandExecute(command: AgentCommand): Task[Checked[command.Response]] =
    commandHandler.typedExecute(command, meta)

  def commandOverview: Task[CommandHandlerOverview] =
    commandHandler.overview

  def commandDetailed: Task[CommandHandlerDetailed[AgentCommand]] =
    commandHandler.detailed

  def overview: Task[AgentOverview] =
    agentHandle.overview

  def order(orderId: OrderId): Task[Checked[Order[Order.State]]] =
    commandHandler.typedExecute(AgentCommand.GetOrder(orderId), meta)
      .map(_.map(_.order))

  def orderIds: Task[Checked[Seq[OrderId]]] =
    commandHandler.typedExecute(AgentCommand.GetOrderIds, meta)
      .map(_.map(_.orderIds))

  def orders: Task[Checked[Seq[Order[Order.State]]]] =
    commandHandler.typedExecute(AgentCommand.GetOrders, meta)
      .map(_.map(_.orders))
}
