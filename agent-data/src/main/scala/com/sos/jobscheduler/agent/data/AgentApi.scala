package js7.agent.data

import js7.agent.data.commands.AgentCommand
import js7.agent.data.views.AgentOverview
import js7.base.problem.Checked
import js7.data.order.{Order, OrderId}
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
