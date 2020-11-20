package js7.data.value.expression

import js7.data.value.expression.Expression.{ListExpression, MkString, StringConstant, StringExpression}
import scala.util.chaining.scalaUtilChainingOps

object ExpressionOptimizer
{
  def optimizeExpression(expression: Expression): Expression =
    expression match {
      case o: MkString => optimizeMkString(o)
      case o => o
    }

  def optimizeMkString(mkString: MkString): StringExpression = {
    optimizeExpression(mkString.expression) match {
      case ListExpression(list) =>
        list
          .map(optimizeExpression)
          .filter(_ != StringConstant.empty)
          .flatMap {
            case MkString(ListExpression(expressions)) => expressions
            case MkString(expr) => expr :: Nil
            case expr => expr :: Nil
          }
          .pipe(mergeStringConstants) match {
            case Nil => StringConstant.empty
            case (string: StringExpression) :: Nil => string
            case o :: Nil => MkString(o)
            case list => MkString(ListExpression(list))
          }

      case o: StringExpression => o
      case o => MkString(o)
    }
  }

  private def mergeStringConstants(expressions: List[Expression]): List[Expression] =
    expressions
      .scanLeft(List.empty[Expression])((reverseList, expr) =>
        (reverseList, expr) match {
          case ((a: StringConstant) :: tail, b: StringConstant) =>
            StringConstant(a.string ++ b.string) :: tail
          case _ =>
            expr :: reverseList
        })
      .lastOption.toList.flatten.reverse
}
