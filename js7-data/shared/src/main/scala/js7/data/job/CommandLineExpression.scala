package js7.data.job

import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.{ListExpression, MkString}
import js7.data.value.expression.ExpressionOptimizer.optimizeExpression

final case class CommandLineExpression(string: String, expressions: List[Expression])
{
  override def toString = string
}

object CommandLineExpression
{
  def optimizeCommandLine(commandLine: CommandLineExpression): CommandLineExpression =
    CommandLineExpression(
      commandLine.string,
      commandLine.expressions
        .view
        .map(optimizeExpression)
        .map {
          case o @ MkString(ListExpression(_ :: _)) => o
          case MkString(expr) => expr
          case o => o
        }
        .toList)
}
