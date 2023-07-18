package js7.data.value.expression

import js7.data.value.expression.Expression.{InterpolatedString, ListExpr, MkString, StringConstant, StringExpr}
import scala.util.chaining.scalaUtilChainingOps

object ExpressionOptimizer
{
  def optimizeExpression(expression: Expression): Expression =
    expression match {
      case o: MkString => optimizeMkString(o)
      case o: InterpolatedString => optimizeInterpolated(o)
      case o => o
    }

  private def optimizeMkString(mkString: MkString): StringExpr =
    optimizeExpression(mkString.expression) match {
      case ListExpr(list) =>
        optimizeConcatList(list).pipe(mergeStringConstants) match {
          case Nil => StringConstant.empty
          case (string: StringExpr) :: Nil => string
          case o :: Nil => MkString(o)
          case list => MkString(ListExpr(list))
        }

      case o: StringExpr => o
      case o => MkString(o)
    }

  private def optimizeInterpolated(interpolated: InterpolatedString): StringExpr =
    optimizeConcatList(interpolated.subexpressions).pipe(mergeStringConstants) match {
      case Nil => StringConstant.empty
      case (string: StringExpr) :: Nil => string
      case list => InterpolatedString(list)
    }

  private def optimizeConcatList(exprs: List[Expression]): List[Expression] =
    exprs
      .map(optimizeExpression)
      .filter(_ != StringConstant.empty)
      .flatMap {
        case MkString(ListExpr(expressions)) => expressions
        case MkString(expr) => expr :: Nil
        case expr => expr :: Nil
      }
      .pipe(mergeStringConstants)

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
