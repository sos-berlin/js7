package js7.data.job

import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.{ListExpr, MkString}
import js7.data.value.expression.ExpressionOptimizer.optimize

final case class CommandLineExpression(string: String, expressions: List[Expression]):
  override def toString = string


object CommandLineExpression:
  def optimizeCommandLine(commandLine: CommandLineExpression): CommandLineExpression =
    CommandLineExpression(
      commandLine.string,
      commandLine.expressions
        .view
        .map(optimize)
        .map:
          case o @ MkString(ListExpr(seq)) if seq.sizeIs > 1 => o
          case MkString(expr) => expr
          case o => o
        .toList)
