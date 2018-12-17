package com.sos.jobscheduler.core.expression

import cats.data.Validated.Invalid
import cats.instances.list._
import cats.syntax.apply._
import cats.syntax.traverse._
import cats.syntax.validated._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.core.expression.Evaluator._
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression._

/**
  * @author Joacim Zschimmer
  */
final class Evaluator(scope: Scope)
{
  def eval(expr: Expression): Checked[Value] =
    expr match {
      case expr: BooleanExpression ⇒ evalBoolean(expr)
      case expr: NumericExpression ⇒ evalNumeric(expr)
      case expr: StringExpression ⇒ evalString(expr)
      case    _: ListExpression ⇒ throw new NotImplementedError("eval(ListExpression)")
    }

  def evalBoolean(expr: BooleanExpression): Checked[BooleanValue] =
    expr match {
      case BooleanConstant(o)    ⇒ BooleanValue(o).valid
      case Equal          (a, b) ⇒ anyAnyToBool(a, b)(_ == _)
      case NotEqual       (a, b) ⇒ anyAnyToBool(a, b)(_ != _)
      case LessThan       (a, b) ⇒ numNumToBool(a, b)(_.number <  _.number)
      case GreaterThan    (a, b) ⇒ numNumToBool(a, b)(_.number >  _.number)
      case LessOrEqual    (a, b) ⇒ numNumToBool(a, b)(_.number <= _.number)
      case GreaterOrEqual (a, b) ⇒ numNumToBool(a, b)(_.number >= _.number)
      case In(a, set: ListExpression) ⇒ eval(a).map2(evalListExpression(set))((a1, set1) ⇒ BooleanValue(set1 contains a1))
      case Matches(a: StringExpression, b: StringExpression) ⇒ evalString(a).map2(evalString(b))(_.string matches _.string).map(BooleanValue.apply)
      case Not            (a)    ⇒ evalBoolean(a) map (o ⇒ !o.bool) map BooleanValue.apply
      case And            (a, b) ⇒ evalBoolean(a) flatMap (o ⇒ if (!o.bool) o.valid else evalBoolean(b))
      case Or             (a, b) ⇒ evalBoolean(a) flatMap (o ⇒ if (o.bool) o.valid else evalBoolean(b))
      case _ ⇒ Invalid(Problem(s"Expression is not evaluable: $expr"))  // Should not happen
    }

  private def evalListExpression(expr: ListExpression): Checked[List[Value]] =
    expr.expressions traverse eval

  def evalNumeric(expr: NumericExpression): Checked[NumericValue] =
    expr match {
      case NumericConstant(o) ⇒ NumericValue(o).valid
      case OrderReturnCode ⇒ NumericValue(scope.returnCode.number).valid
      case ToNumber(e) ⇒ eval(e) flatMap toNumeric
    }

  def evalString(expr: StringExpression): Checked[StringValue] =
    expr match {
      case StringConstant(o) ⇒
        StringValue(o).valid

      case Variable(stringExpr, default) ⇒
        evalString(stringExpr) flatMap (name ⇒
          scope.variableNameToString.lift(name.string) map StringConstant.apply orElse default
            toChecked Problem(s"No such variable: ${name.string}")
            flatMap evalString)
    }

  private def anyAnyToBool[A <: Expression, B <: Expression](a: A, b: B)(op: (Value, Value) ⇒ Boolean): Checked[BooleanValue] =
    for {
      a1 ← eval(a)
      b1 ← eval(b)
    } yield BooleanValue(op(a1, b1))

  private def numNumToBool[A <: NumericExpression, B <: NumericExpression](a: A, b: B)(op: (NumericValue, NumericValue) ⇒ Boolean): Checked[BooleanValue] =
    for {
      a1 ← evalNumeric(a)
      b1 ← evalNumeric(b)
    } yield BooleanValue(op(a1, b1))

  private def ifBoolean(v: Value): Checked[BooleanValue] =
    v match {
      case v: BooleanValue ⇒ v.valid
      case _ ⇒ Problem("Boolean value expected")
    }

  private def ifNumeric(v: Value): Checked[NumericValue] =
    v match {
      case v: NumericValue ⇒ v.valid
      case _ ⇒ Problem("Numeric value expected")
    }

  private def toNumeric(v: Value): Checked[NumericValue] =
    v match {
      case v: NumericValue ⇒ v.valid
      case BooleanValue(false) ⇒ NumericValue(0).valid
      case BooleanValue(true) ⇒ NumericValue(1).valid
      case StringValue(o) ⇒
        try NumericValue(BigDecimal(o)).valid
        catch { case _: NumberFormatException ⇒
          // getMessage returns null
          Problem(s"Not a valid number: ${o.truncateWithEllipsis(10)}")
        }
    }

  //private def castValue[A: ClassTag](v: Value): Checked[A] =
  //  if (implicitClass[A] isAssignableFrom v.getClass)
  //
  // v.asInstanceOf[A].valid
  //  else
  //    Problem(s"Not a ${implicitClass[A].simpleScalaName}")
}

object Evaluator
{
  def evalBoolean(scope: Scope, expression: BooleanExpression): Checked[Boolean] =
    new Evaluator(scope).evalBoolean(expression) map (_.bool)

  sealed trait Value

  final case class BooleanValue(bool: Boolean) extends Value
  object BooleanValue {
    val True = BooleanValue(true)
    val False = BooleanValue(false)
  }

  final case class NumericValue(number: BigDecimal) extends Value
  object NumericValue {
    def fromString(number: String): Checked[NumericValue] =
      try
        NumericValue(BigDecimal(number)).valid
      catch { case e: NumberFormatException ⇒ Problem(e.getMessage)}
  }

  final case class StringValue(string: String) extends Value
}
