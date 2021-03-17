package js7.data.job

import cats.instances.list._
import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.data.value.expression.Evaluator

final class CommandLineEvaluator(evaluator: Evaluator)
{
  def eval(commandLineExpression: CommandLineExpression): Checked[CommandLine] =
    commandLineExpression.expressions
      .traverse(expr => evaluator.eval(expr).map(_.convertToString))
      .flatMap(CommandLine.checked)
}
