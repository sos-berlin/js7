package com.sos.jobscheduler.agent.data

import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.views.AgentOverview
import com.sos.jobscheduler.data.order.{Order, OrderId}
import monix.eval.Task
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
trait AgentApi
{
  def commandExecute(command: AgentCommand): Task[command.Response]

  //def commandOverview: Task[CommandHandlerOverview]
  //
  //def commandDetailed: Task[CommandHandlerDetailed]

  def overview: Task[AgentOverview]

  def order(orderId: OrderId): Task[Order[Order.State]]

  def orderIds: Task[Seq[OrderId]]

  def orders: Task[Seq[Order[Order.State]]]
}
