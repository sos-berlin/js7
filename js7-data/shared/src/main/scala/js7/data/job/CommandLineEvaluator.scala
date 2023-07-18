package js7.data.job

import cats.instances.list.*
import cats.syntax.traverse.*
import js7.base.problem.Checked
import js7.data.value.expression.Scope

final class CommandLineEvaluator()(implicit scope: Scope)
{
  def eval(commandLineExpression: CommandLineExpression): Checked[CommandLine] =
    commandLineExpression.expressions
      .traverse(_.eval.map(_.convertToString))
      .flatMap(CommandLine.checked)
}
