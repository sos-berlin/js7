package js7.agent.subagent

import cats.syntax.foldable._
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.stream.Numbered
import js7.base.utils.ScalaUtils.syntax._
import js7.data.item.InventoryItem
import js7.data.order.Order
import js7.data.subagent.SubagentId
import js7.data.value.expression.Expression
import js7.subagent.data.SubagentCommand
import js7.subagent.data.SubagentCommand.{AttachItem, StartOrderProcess}
import monix.eval.Task

final class SubagentDispatcher(
  subagentId: SubagentId,
  protected val postCommand: Numbered[SubagentCommand] => Task[Checked[SubagentCommand.Response]])
  extends CommandDispatcher
{
  protected type Command = SubagentCommand
  protected type Response = SubagentCommand.Response

  protected def name = subagentId.toString

  def startProcess(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression],
    items: Iterable[InventoryItem])
  : Task[Checked[Unit]] =
    attachItems(items).flatMapT(_ =>
      executeCommand(StartOrderProcess(order, defaultArguments))
        .rightAs(()))

  private def attachItems(items: Iterable[InventoryItem]): Task[Checked[Unit]] =
    executeCommands(items.view.map(AttachItem(_)))
      .map(_.rightAs(()).combineAll)
}
