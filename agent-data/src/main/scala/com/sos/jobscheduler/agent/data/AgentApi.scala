package com.sos.jobscheduler.agent.data

import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.views.AgentOverview
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.data.order.{Order, OrderId}
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait AgentApi
{
  def commandExecute(command: AgentCommand): Task[Checked[command.Response]]

  //def commandOverview: Task[CommandHandlerOverview]
  //
  //def commandDetailed: Task[CommandHandlerDetailed]

  def overview: Task[AgentOverview]

  def order(orderId: OrderId): Task[Checked[Order[Order.State]]]

  def orderIds: Task[Checked[Seq[OrderId]]]

  def orders: Task[Checked[Seq[Order[Order.State]]]]
}
