package com.sos.jobscheduler.data.workflow.parser

import com.sos.jobscheduler.data.workflow.instructions.expr.Expression
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression._
import com.sos.jobscheduler.data.workflow.parser.BasicParsers._
import com.sos.jobscheduler.data.workflow.parser.BasicParsers.ops._
import fastparse.all._

/**
  * @author Joacim Zschimmer
  */
object ExpressionParser
{
  lazy val expression: P[Expression] =
    or

  lazy val booleanExpression: P[BooleanExpression] =
    expression flatMap {
      case o: BooleanExpression ⇒ valid(o)
      case o ⇒ invalid(s"Expression is not of type Boolean: $o")
    }

  lazy val numericExpression: P[NumericExpression] =
    expression flatMap {
      case o: NumericExpression ⇒ valid(o)
      case o ⇒ invalid(s"Expression is not of type Number: $o")
    }

  lazy val stringExpression: P[StringExpression] =
    expression flatMap {
      case o: StringExpression ⇒ valid(o)
      case o ⇒ invalid(s"Expression is not of type String: $o")
    }

  private val trueConstant = P[BooleanConstant](
    keyword("true") map (_ ⇒ BooleanConstant(true)))

  private val falseConstant = P[BooleanConstant](
    keyword("false") map (_ ⇒ BooleanConstant(false)))

  private val returnCode = P[OrderReturnCode.type](
    keyword("returnCode") map (_ ⇒ OrderReturnCode))

  private val booleanConstant = trueConstant | falseConstant

  private val numericConstant = P[NumericConstant](int map (o ⇒ NumericConstant(o)))

  private val stringConstant = P[StringConstant](quotedString map StringConstant.apply)

  private[parser] val variable = P[StringExpression] {
    val variableFunction = P(keyword("variable") ~~/ (inParentheses(stringExpression) map Variable.apply))
    val simpleName = P[String]((CharPred(Variable.isSimpleNameStart) ~ CharsWhile(Variable.isSimpleNamePart, min = 0)).!)
    val dollarVariable = P((P("$") ~ simpleName) map (o ⇒ Variable(StringConstant(o))))
    variableFunction | dollarVariable
  }

  private val factor = P[Expression](
    inParentheses(expression) | booleanConstant | numericConstant | stringConstant | returnCode | variable)

  // TODO Reject comparison of incomparable types ("1" != 1)
  private val comparison = P[Expression]{
    val compareOperator = P(StringIn("==", "!=", "<=", ">=", "<", ">")).!
    val inSet = P(sequence(expression) map (o ⇒ ListExpression(o.toList)))
    val expr = P[(Expression, Option[(String, Expression)])](
      factor ~~/ ((compareOperator ~~/ factor) | (keyword("in").! ~~/ inSet)).?)
    expr flatMap {
      case (a, None) ⇒ valid(a)
      case (a: Expression, Some(("==", b: Expression))) ⇒ valid(Equal(a, b))
      case (a: Expression, Some(("!=", b: Expression))) ⇒ valid(NotEqual(a, b))
      case (a: NumericExpression, Some(("<=", b: NumericExpression))) ⇒ valid(LessOrEqual(a, b))
      case (a: NumericExpression, Some((">=", b: NumericExpression))) ⇒ valid(GreaterOrEqual(a, b))
      case (a: NumericExpression, Some(("<" , b: NumericExpression))) ⇒ valid(LessThan(a, b))
      case (a: NumericExpression, Some((">" , b: NumericExpression))) ⇒ valid(GreaterThan(a, b))
      case (a: Expression, Some(("in", list: ListExpression))) ⇒ valid(In(a, list))
      case (a, Some((op: String, b: Expression))) ⇒ invalid(s"Types are not comparable: " + Precedence.toString(a, op, Precedence.Comparison, b))
    }}

  private val and: P[Expression] = P(
    (comparison ~~/ (P("&&") ~~/ and).?) flatMap {
      case (a, None) ⇒ valid(a)
      case (a: BooleanExpression, Some(b: BooleanExpression)) ⇒ valid(And(a, b))
      case (a, Some(b)) ⇒ invalid(s"Operator && requires Boolean operands: " + Precedence.toString(a, "&&", Precedence.And, b))
    }
  )

  private val or: P[Expression] = P(
    (and ~~/ (P("||") ~~/ or).?) flatMap {
      case (a, None) ⇒ valid(a)
      case (a: BooleanExpression, Some(b: BooleanExpression)) ⇒ valid(Or(a, b))
      case (a, Some(b)) ⇒ invalid(s"Operator || requires Boolean operands: " + Precedence.toString(a, "||", Precedence.Or, b))
    }
  )

}
