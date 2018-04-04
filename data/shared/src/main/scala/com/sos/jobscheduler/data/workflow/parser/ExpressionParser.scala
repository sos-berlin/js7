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
    wordOperation

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

  private val parenthesizedExpression = P[Expression](
    parenthesizedCommaSeq(expression) map {
      case Seq(o) ⇒ o
      case seq ⇒ ListExpression(seq.toList)
    }
  )

  private val trueConstant = P[BooleanConstant](
    keyword("true") map (_ ⇒ BooleanConstant(true)))

  private val falseConstant = P[BooleanConstant](
    keyword("false") map (_ ⇒ BooleanConstant(false)))

  private val returnCode = P[OrderReturnCode.type](
    keyword("returnCode") map (_ ⇒ OrderReturnCode))

  private val booleanConstant = trueConstant | falseConstant

  private val numericConstant = P[NumericConstant](int
    .map(o ⇒ NumericConstant(o)))

  private val stringConstant = P[StringConstant](quotedString
    .map(StringConstant.apply))

  private[parser] val dollarVariable = P[StringExpression] {
    val simpleName = P[String]((CharPred(Variable.isSimpleNameStart) ~ CharsWhile(Variable.isSimpleNamePart, min = 0)).!)
    (P("$") ~ simpleName)
      .map(o ⇒ Variable(StringConstant(o)))
  }

  private[parser] val functionCall = P(
    P(keyword("variable") ~~/ inParentheses(stringExpression ~ (comma ~ stringExpression).?)
      .map { case (name, default) ⇒ Variable(name, default) }))

  private val factorOnly = P(
    parenthesizedExpression | booleanConstant | numericConstant | stringConstant | returnCode | dollarVariable | functionCall)

  private val factor = P(
    factorOnly ~ (w ~ "." ~~/ keyword).? map {
      case (o, None) ⇒ o
      case (o, Some("toNumber")) ⇒ ToNumber(o)
    })

  private lazy val not: P[Expression] = P(
    (P("!") ~~/ bFactor) flatMap {
      case o: BooleanExpression ⇒ valid(Not(o))
      case _ ⇒ invalid("Operator '!' expects Boolean expression")
    })

  private val bFactor = not | factor

  // TODO Reject comparison of incomparable types ("1" != 1)
  private val comparison: P[Expression] =
    leftRecurse(bFactor, P(StringIn("==", "!=", "<=", ">=", "<", ">")).!) {
      case (a, "==", b) ⇒ valid(Equal(a, b))
      case (a, "!=", b) ⇒ valid(NotEqual(a, b))
      case (a: NumericExpression, "<=", b: NumericExpression) ⇒ valid(LessOrEqual(a, b))
      case (a: NumericExpression, ">=", b: NumericExpression) ⇒ valid(GreaterOrEqual(a, b))
      case (a: NumericExpression, "<" , b: NumericExpression) ⇒ valid(LessThan(a, b))
      case (a: NumericExpression, ">" , b: NumericExpression) ⇒ valid(GreaterThan(a, b))
      case (a, op: String, b) ⇒ invalid(s"Types are not comparable: " + Precedence.toString(a, op, Precedence.Comparison, b))
    }

  private val and: P[Expression] =
    leftRecurse(comparison, P("&&")) {
      case (a: BooleanExpression, (), b: BooleanExpression) ⇒ valid(And(a, b))
      case (a, (), b) ⇒ invalid(s"Operator && requires Boolean operands: " + Precedence.toString(a, "&&", Precedence.And, b))
    }

  private val or: P[Expression] =
    leftRecurse(and, P("||")) {
      case (a: BooleanExpression, (), b: BooleanExpression) ⇒ valid(Or(a, b))
      case (a, (), b) ⇒ invalid(s"Operator || requires Boolean operands: " + Precedence.toString(a, "||", Precedence.Or, b))
    }

  private val wordOperation: P[Expression] =
    leftRecurse(or, keyword) {
      case (a, "in", list: ListExpression) ⇒ valid(In(a, list))
      case (_, "in", _) ⇒ invalid("List expected after operator 'in'")
      case (a, "matches", b: StringExpression) ⇒ valid(Matches(a, b))
      case (_, "matches", _) ⇒ invalid("String expected after operator 'matches'")
      case (a, op, b) ⇒ invalid(s"Operator '$op' with unexpected operand type: " + Precedence.toString(a, op, Precedence.Or, b))
    }
}
