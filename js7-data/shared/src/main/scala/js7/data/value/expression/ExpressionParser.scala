package js7.data.value.expression

import fastparse.NoWhitespace._
import fastparse._
import js7.base.problem.Checked
import js7.data.job.JobResourcePath
import js7.data.parser.BasicParsers
import js7.data.parser.BasicParsers._
import js7.data.parser.Parsers.checkedParse
import js7.data.value.expression.Expression._
import js7.data.value.expression.ExpressionOptimizer.optimizeExpression
import js7.data.workflow.instructions.executable.WorkflowJob

/**
  * @author Joacim Zschimmer
  */
object ExpressionParser
{
  def parse(string: String): Checked[Expression] =
    checkedParse(string, expressionOnly(_))
      .left.map(_.withPrefix("Error in expression:"))

  def constantExpression[_: P]: P[Expression] =
    expression

  def expressionOnly[_: P]: P[Expression] =
    P(wordOperation ~ End)

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

  private def singleQuotedStringConstant[_: P] = P[StringConstant](singleQuoted
    .map(StringConstant.apply))

  def quotedString[_: P] = P[String](BasicParsers.quotedString)  // Public

  def interpolatedString[_: P] = P[StringExpression](
    "\"" ~~/
    interpolatedStringContent ~~/
    "\"")

  private def interpolatedStringContent[_: P] = P[StringExpression] {
    def simpleConstant = P(CharsWhile(ch => ch != '"' && ch != '\\' && ch != '$').!)
    def constant = P(((simpleConstant | escapedCharInString).rep.map(_.mkString)).map(StringConstant(_)))

    def curlyName = P("{" ~~/ identifier ~~/ "}")
    def namedValue = (identifier | digits/*regex group*/ | curlyName).map(NamedValue(_))
    def expr = P("$" ~~/ (namedValue | ("(" ~~/ expression ~~/ ")")))

    (constant ~~/ (expr ~~/ constant).rep)
      .map {
        case (o: StringConstant, Nil) => o
        case (first, pairs) =>
          optimizeExpression(InterpolatedString(
            (Some(first) ++ pairs.flatMap { case (expr, constant) => expr :: constant :: Nil})
              .toList)
          ).asInstanceOf[StringExpression]
      }
  }

  def dollarNamedValue[_: P] = P[Expression] {
    //def arg = P[NamedValue](("arg::" ~~/ identifier)
    //  .map(key => NamedValue(NamedValue.Argument, NamedValue.KeyValue(StringConstant(key)))))
    //def byLabel = P[NamedValue](("label::" ~~/ identifier ~~ "." ~~/ identifier)
    //  .map { case (jobName, key) => NamedValue(NamedValue.ByLabel(Label(jobName)), NamedValue.KeyValue(StringConstant(key))) })
    //def byJob = P[NamedValue](("job::" ~~/ identifier ~~ "." ~~/ identifier)
    //  .map { case (jobName, key) => NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name(jobName)), NamedValue.KeyValue(StringConstant(key))) })
    //def byPrefix = P[NamedValue]((identifier ~~ "." ~~/ identifier)
    //  .map { case (prefix, key) => NamedValue(NamedValue.LastOccurredByPrefix(prefix), NamedValue.KeyValue(StringConstant(key))) })
    //def curlyName = P[NamedValue]("{" ~~/ (/*arg | byLabel | byJob | byPrefix |*/ nameOnly(identifier)) ~~ "}"./)
    def curlyName = P[Expression](
      ("{" ~~/ identifier ~~ "}"./)
        .map(NamedValue.last(_)))

    "$" ~~ ((identifier | digits/*regex group*/).map(NamedValue.last(_)) | curlyName)
  }

  private def jobResourceSetting[_: P] = P[JobResourceSetting](
    ("JobResource" ~ ":" ~/ jobResourcePath ~ ":" ~/ identifier)
      .map((JobResourceSetting.apply(_, _)).tupled))

  private def jobResourcePath[_: P] = P[JobResourcePath](
    (CharPred(JobResourcePath.isNameStart) ~ CharsWhile(JobResourcePath.isNamePartMaybe)).!
      .flatMap(o => checkedToP(JobResourcePath.checked(o))))

  private def argumentFunctionCall[_: P] = P[NamedValue](
    keyword("argument") ~ w ~/
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

  private def functionCall[_: P] = P[FunctionCall](
    (identifier ~/ w ~/ inParentheses(commaSequence((identifier ~ w ~ "=").? ~ w ~ expression)))
      .map { case (name, arguments) =>
        FunctionCall(
          name,
          arguments.map { case (maybeName, expr) => Argument(expr, maybeName) })
      })

  private def namedValueKeyValue[_: P] = P[(String, Any)](
    keyValueConvert("label", identifier)(o => Right(NamedValue.ByLabel(o))) |
    keyValueConvert("job", identifier)(o => Right(NamedValue.LastExecutedJob(WorkflowJob.Name(o)))) |
    keyValue("default", expression))

  private def missing[_: P] = P[MissingConstant](
    P("missing").map(_ => MissingConstant()))

  private def nullConstant[_: P] = P[NullConstant](
    P("null").map(_ => NullConstant))

  private def factorOnly[_: P] = P(
    parenthesizedExpression | booleanConstant | numericConstant |
      singleQuotedStringConstant | interpolatedString | dollarNamedValue |
      catchCount | argumentFunctionCall | variableFunctionCall |
      missing | nullConstant |
      jobResourceSetting |
      functionCall)

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

  private def addition[_: P] = P[Expression](
    bFactor.flatMap(initial =>
      leftRecurse(initial, P(StringIn("++", "+", "-")).!, bFactor) {
        case (a, "++", b) => valid(Concat(a, b))
        case (a, "+", b) => valid(Add(a, b))
        case (a, "-", b) => valid(Substract(a, b))
        case (_, o, _) => invalid(s"Unexpected operator: $o") // Does not happen
      }))

  private def comparison[_: P] = P[Expression](
    addition.flatMap(initial =>
      leftRecurse(initial, P(StringIn("==", "!=", "<=", ">=", "<", ">")).!, addition) {
        case (a, "==", b) => valid(Equal(a, b))
        case (a, "!=", b) => valid(NotEqual(a, b))
        case (a, "<=", b) => valid(LessOrEqual(a, b))
        case (a, ">=", b) => valid(GreaterOrEqual(a, b))
        case (a, "<" , b) => valid(LessThan(a, b))
        case (a, ">" , b) => valid(GreaterThan(a, b))
        case (_, o, _) => invalid(s"Unexpected operator: $o") // Does not happen
      }))

  private def and[_: P] = P[Expression](
    comparison.flatMap(initial =>
      leftRecurse(initial, "&&", comparison) {
        case (a: BooleanExpression, (), b: BooleanExpression) =>
          valid(And(a, b))
        case (a, (), b) =>
          invalid(s"Operator && requires Boolean operands: " +
            Precedence.toString(a, "&&", Precedence.And, b))
      }))

  private def or[_: P] = P[Expression](
    and.flatMap(initial =>
      leftRecurse(initial, "||", and) {
        case (a: BooleanExpression, (), b: BooleanExpression) => valid(Or(a, b))
        case (a, (), b) => invalid(s"boolean operarands for operator ||: " +
          Precedence.toString(a, "||", Precedence.Or, b))
      }))

  private def word1Operation[_: P] = P[Expression](
    (or ~~/ w).flatMap(expr =>
      (P("?").!.?.map {
        case None => expr
        case Some(_) => OrNull(expr)
      })))

  private def wordOperation[_: P] = P[Expression](
    (word1Operation ~~ w).flatMap(initial =>
      leftRecurse(initial, keyword, word1Operation) {
        case (a, "in", list: ListExpression) => valid(In(a, list))
        case (_, "in", _) => invalid("List expected after operator 'in'")
        case (a, "matches", b) => valid(Matches(a, b))
        case (a, "orElse", b) => valid(OrElse(a, b))
        case (a, op, b) => invalid(s"Operator '$op' with unexpected operand type: " +
          Precedence.toString(a, op, Precedence.Or, b))
      }))
}
