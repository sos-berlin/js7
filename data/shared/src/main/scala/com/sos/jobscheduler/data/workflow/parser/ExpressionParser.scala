package com.sos.jobscheduler.data.workflow.parser

import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression._
import com.sos.jobscheduler.data.workflow.parser.BasicParsers._
import fastparse.NoWhitespace._
import fastparse._

/**
  * @author Joacim Zschimmer
  */
object ExpressionParser
{
  def expression[_: P]: P[Expression] =
    P(wordOperation)

  def booleanExpression[_: P] = P[BooleanExpression](
    expression flatMap {
      case o: BooleanExpression => valid(o)
      case o => invalid(s"Expression is not of type Boolean: $o")
    })

  def numericExpression[_: P] = P[NumericExpression](
    expression flatMap {
      case o: NumericExpression => valid(o)
      case o => invalid(s"Expression is not of type Number: $o")
    })

  def stringExpression[_: P] = P[StringExpression](
    expression flatMap {
      case o: StringExpression => valid(o)
      case o => invalid(s"Expression is not of type String: $o")
    })

  private def parenthesizedExpression[_: P] = P[Expression](
    ("(" ~ w ~/ expression ~ w ~ ")") |
      bracketCommaSequence(expression).map(o => ListExpression(o.toList)))

  private def trueConstant[_: P] = P[BooleanConstant](
    keyword("true") map (_ => BooleanConstant(true)))

  private def falseConstant[_: P] = P[BooleanConstant](
    keyword("false") map (_ => BooleanConstant(false)))

  private def returnCode[_: P] = P[OrderReturnCode.type](
    keyword("returnCode") map (_ => OrderReturnCode))

  private def catchCount[_: P] = P[OrderCatchCount.type](
    keyword("catchCount") map (_ => OrderCatchCount))

  private def booleanConstant[_: P] = P(trueConstant | falseConstant)

  private def numericConstant[_: P] = P[NumericConstant](int
    .map(o => NumericConstant(o)))

  private def stringConstant[_: P] = P[StringConstant](quotedString
    .map(StringConstant.apply))

  private[parser] def dollarVariable[_: P] = P[StringExpression] {
    def simpleName = P[String]((CharPred(Variable.isSimpleNameStart) ~ CharsWhile(Variable.isSimpleNamePart, 0)).!)
    ("$" ~ simpleName)
      .map(o => Variable(StringConstant(o)))
  }

  private[parser] def variableFunctionCall[_: P] = P(
    P(keyword("variable") ~ w ~/ inParentheses(stringExpression ~ (comma ~ stringExpression).?)
      .map { case (name, default) => Variable(name, default) }))

  private def factorOnly[_: P] = P(
    parenthesizedExpression | booleanConstant | numericConstant | stringConstant | returnCode | catchCount |
      dollarVariable | variableFunctionCall)

  private def factor[_: P] = P(
    factorOnly ~ (w ~ "." ~ w ~/ keyword).? flatMap {
      case (o, None) => valid(o)
      case (o, Some("toNumber")) => valid(ToNumber(o))
      case (o: StringExpression, Some("toBoolean")) => valid(ToBoolean(o))
      case (o: StringExpression, Some("stripMargin")) => valid(StripMargin(o))
      case (o, Some(f)) => invalid(s"known function: .$f")   //  for ${o.getClass.simpleScalaName}")
    })

  private def not[_: P]: P[Expression] = P(
    ("!" ~ w ~/ bFactor) flatMap {
      case o: BooleanExpression => valid(Not(o))
      case _ => invalid("Operator '!' expects Boolean expression")
    })

  private def bFactor[_: P] = P(not | factor)

  // TODO Reject comparison of incomparable types ("1" != 1)
  private def comparison[_: P] = P[Expression](
    leftRecurse(bFactor, P(StringIn("==", "!=", "<=", ">=", "<", ">")).!) {
      case (a, "==", b) => valid(Equal(a, b))
      case (a, "!=", b) => valid(NotEqual(a, b))
      case (a: NumericExpression, "<=", b: NumericExpression) => valid(LessOrEqual(a, b))
      case (a: NumericExpression, ">=", b: NumericExpression) => valid(GreaterOrEqual(a, b))
      case (a: NumericExpression, "<" , b: NumericExpression) => valid(LessThan(a, b))
      case (a: NumericExpression, ">" , b: NumericExpression) => valid(GreaterThan(a, b))
      case (a, op: String, b) => invalid(s"comparable types: " + Precedence.toString(a, op, Precedence.Comparison, b))
    })

  private def and[_: P] = P[Expression](
    leftRecurse(comparison, "&&") {
      case (a: BooleanExpression, (), b: BooleanExpression) => valid(And(a, b))
      case (a, (), b) => invalid(s"Operator && requires Boolean operands: " + Precedence.toString(a, "&&", Precedence.And, b))
    })

  private def or[_: P] = P[Expression](
    leftRecurse(and, "||") {
      case (a: BooleanExpression, (), b: BooleanExpression) => valid(Or(a, b))
      case (a, (), b) => invalid(s"boolean operarands for operator ||: " + Precedence.toString(a, "||", Precedence.Or, b))
    })

  private def wordOperation[_: P] = P[Expression](
    leftRecurse(or, keyword) {
      case (a, "in", list: ListExpression) => valid(In(a, list))
      case (_, "in", _) => invalid("List expected after operator 'in'")
      case (a, "matches", b: StringExpression) => valid(Matches(a, b))
      case (_, "matches", _) => invalid("String expected after operator 'matches'")
      case (a, op, b) => invalid(s"Operator '$op' with unexpected operand type: " + Precedence.toString(a, op, Precedence.Or, b))
    })
}
