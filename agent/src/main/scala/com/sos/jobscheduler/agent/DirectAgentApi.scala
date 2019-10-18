package com.sos.jobscheduler.agent

import com.sos.jobscheduler.agent.command.CommandHandler
import com.sos.jobscheduler.agent.data.AgentApi
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.views.AgentOverview
import com.sos.jobscheduler.agent.scheduler.AgentHandle
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.event.EventWatch
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.data.command.{CommandHandlerDetailed, CommandHandlerOverview}
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.{Order, OrderId}
import monix.eval.Task
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class DirectAgentApi(commandHandler: CommandHandler, agentHandle: AgentHandle, meta: CommandMeta = CommandMeta.Anonymous)
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

  def eventWatchForMaster(masterId: MasterId): Task[Checked[EventWatch]] =
    agentHandle.eventWatch(masterId)
}
