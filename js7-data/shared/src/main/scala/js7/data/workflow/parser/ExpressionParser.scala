package js7.data.workflow.parser

import fastparse.NoWhitespace._
import fastparse._
import js7.data.expression.Expression
import js7.data.expression.Expression._
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.parser.BasicParsers.{keyValue, _}

/**
  * @author Joacim Zschimmer
  */
object ExpressionParser
{
  def constantExpression[_: P]: P[Expression] =
    expression

  def expression[_: P]: P[Expression] =
    P(wordOperation)

  private def parenthesizedExpression[_: P] = P[Expression](
    ("(" ~ w ~/ expression ~ w ~ ")") |
      bracketCommaSequence(expression).map(o => ListExpression(o.toList)))

  private def trueConstant[_: P] = P[BooleanConstant](
    keyword("true").map(_ => BooleanConstant(true)))

  private def falseConstant[_: P] = P[BooleanConstant](
    keyword("false").map(_ => BooleanConstant(false)))

  private def catchCount[_: P] = P[OrderCatchCount.type](
    keyword("catchCount").map(_ => OrderCatchCount))

  def booleanConstant[_: P] = P(trueConstant | falseConstant)

  private def numericConstant[_: P] = P[NumericConstant](
    int.map(o => NumericConstant(o)))

  private def stringConstant[_: P] = P[StringConstant](quotedString
    .map(StringConstant.apply))

  def quotedString[_: P] = P[String](BasicParsers.quotedString)  // Public

  private[parser] def dollarNamedValue[_: P] = P[NamedValue] {
    def simpleName = P[String](
      (CharPred(NamedValue.isSimpleNameStart) ~ CharsWhile(NamedValue.isSimpleNamePart, 0)).!)
    def nameOnly(p: P[String]) = P[NamedValue](
      p.map(o => NamedValue.last(o)))
    //def arg = P[NamedValue](("arg::" ~~/ identifier)
    //  .map(key => NamedValue(NamedValue.Argument, NamedValue.KeyValue(StringConstant(key)))))
    //def byLabel = P[NamedValue](("label::" ~~/ identifier ~~ "." ~~/ identifier)
    //  .map { case (jobName, key) => NamedValue(NamedValue.ByLabel(Label(jobName)), NamedValue.KeyValue(StringConstant(key))) })
    //def byJob = P[NamedValue](("job::" ~~/ identifier ~~ "." ~~/ identifier)
    //  .map { case (jobName, key) => NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name(jobName)), NamedValue.KeyValue(StringConstant(key))) })
    //def byPrefix = P[NamedValue]((identifier ~~ "." ~~/ identifier)
    //  .map { case (prefix, key) => NamedValue(NamedValue.LastOccurredByPrefix(prefix), NamedValue.KeyValue(StringConstant(key))) })
    def curlyName = P[NamedValue]("{" ~~/ (/*arg | byLabel | byJob | byPrefix |*/ nameOnly(identifier)) ~~ "}"./)

    "$" ~~ (nameOnly(simpleName) | curlyName)
  }

  //private def namedValue[_: P] = P[NamedValue](
  //  (where ~ P(keyword("returnCode")))
  //    .map(where => NamedValue(where, NamedValue.ReturnCode)) |
  //  (where ~ P(keyword("variable") ~ w ~/ inParentheses(stringExpression ~ (comma ~ stringExpression).?)))
  //    .map { case (where, (name, default)) => NamedValue(where, NamedValue.KeyValue(name), default) })
  //
  //private def where[_: P] = P[NamedValue.Where](
  //  (identifier ~ w ~ "." ~ w).?
  //    .map(_.fold[NamedValue.Where](NamedValue.LastOccurred)(NamedValue.LastOccurredByPrefix.apply)))

  private def argumentFunctionCall[_: P] = P[NamedValue](
    keyword("argument") ~ w ~
      inParentheses(
        for {
          kv <- keyValues(keyValue("default", expression) | keyValue("key", expression) | keyValue("", expression))
          key <- kv.oneOf[Expression]("key", "").map(_._2)
          default <- kv.get[Expression]("default")
        } yield NamedValue(NamedValue.Argument, NamedValue.KeyValue(key), default)))

  private def variableFunctionCall[_: P] = P[NamedValue](
    keyword("variable") ~ w ~ inParentheses(
      for {
        kv <- keyValues(namedValueKeyValue | keyValue("key", expression) | keyValue("", expression))
        where <- kv.oneOfOr[NamedValue.Where](Set("label", "job"), NamedValue.LastOccurred)
        key <- kv.oneOf[Expression]("key", "").map(_._2)
        default <- kv.get[Expression]("default")
      } yield NamedValue(where, NamedValue.KeyValue(key), default)))

  private def returnCode[_: P] = P[NamedValue] {
    def withArguments = P[NamedValue](
      inParentheses(
        for {
          kv <- keyValues(namedValueKeyValue)
          where <- kv.oneOfOr[NamedValue.Where](Set("label", "job"), NamedValue.LastOccurred)
          default <- kv.get[Expression]("default")
        } yield NamedValue(where, NamedValue.ReturnCode, default)))

    keyword("returnCode") ~ w ~ withArguments.?.map(_ getOrElse LastReturnCode)
  }

  private def namedValueKeyValue[_: P] = P[(String, Any)](
    keyValueConvert("label", identifier)(o => Right(NamedValue.ByLabel(o))) |
    keyValueConvert("job", identifier)(o => Right(NamedValue.LastExecutedJob(WorkflowJob.Name(o)))) |
    keyValue("default", expression))

  private def factorOnly[_: P] = P(
    parenthesizedExpression | booleanConstant | numericConstant | stringConstant | catchCount |
      argumentFunctionCall | variableFunctionCall | returnCode | dollarNamedValue)

  private def factor[_: P] = P(
    factorOnly ~ (w ~ "." ~ w ~/ keyword).? flatMap {
      case (o, None) => valid(o)
      case (o, Some("toNumber")) => valid(ToNumber(o))
      case (o, Some("toBoolean")) => valid(ToBoolean(o))
      case (o, Some("stripMargin")) => valid(StripMargin(o))
      case (o, Some("mkString")) => valid(MkString(o))
      case (_, Some(f)) => invalid(s"known function: .$f")   //  for ${o.getClass.simpleScalaName}")
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
      case (a, "<=", b) => valid(LessOrEqual(a, b))
      case (a, ">=", b) => valid(GreaterOrEqual(a, b))
      case (a, "<" , b) => valid(LessThan(a, b))
      case (a, ">" , b) => valid(GreaterThan(a, b))
      case (_, o, _) => invalid(s"Unexpected operator: $o") // Does not happen
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
      case (a, "matches", b) => valid(Matches(a, b))
      case (a, op, b) => invalid(s"Operator '$op' with unexpected operand type: " + Precedence.toString(a, op, Precedence.Or, b))
    })
}
