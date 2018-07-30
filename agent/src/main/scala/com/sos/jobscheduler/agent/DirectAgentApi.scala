package com.sos.jobscheduler.agent

import com.sos.jobscheduler.agent.command.{CommandHandler, CommandMeta}
import com.sos.jobscheduler.agent.data.AgentApi
import com.sos.jobscheduler.agent.data.command.{CommandHandlerDetailed, CommandHandlerOverview}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.views.AgentOverview
import com.sos.jobscheduler.agent.scheduler.AgentHandle
import com.sos.jobscheduler.common.event.EventWatch
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.{Order, OrderId}
import monix.eval.Task
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class DirectAgentApi(commandHandler: CommandHandler, agentHandle: AgentHandle, meta: CommandMeta = CommandMeta())
extends AgentApi
{
  def commandExecute(command: AgentCommand): Task[command.Response] =
    Task.deferFuture(commandHandler.typedExecute(command, meta))

  def commandOverview: Task[CommandHandlerOverview] =
    Task.deferFuture(commandHandler.overview)

  def commandDetailed: Task[CommandHandlerDetailed] =
    Task.deferFuture(commandHandler.detailed)

  def overview: Task[AgentOverview] =
    Task.deferFuture(agentHandle.overview)

  def order(orderId: OrderId): Task[Order[Order.State]] =
    Task.deferFuture(commandHandler.typedExecute(AgentCommand.GetOrder(orderId), meta))
      .map(_.order)

  def orderIds: Task[Seq[OrderId]] =
    Task.deferFuture(commandHandler.typedExecute(AgentCommand.GetOrderIds, meta))
      .map(_.orderIds)

  def orders: Task[Seq[Order[Order.State]]] =
    Task.deferFuture(commandHandler.typedExecute(AgentCommand.GetOrders, meta))
      .map(_.orders)

  def eventWatchForMaster(masterId: MasterId): Task[EventWatch[Event]] =
    agentHandle.eventWatch(masterId)
}
