package js7.data.value.expression

import cats.parse.Numbers.digits
import cats.parse.Parser.{char, charWhere, charsWhile, charsWhile0, end, failWith, pure, string, stringIn}
import cats.parse.{Parser, Parser0}
import js7.base.problem.Checked
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.job.JobResourcePath
import js7.data.parser.CatsBasicParsers.*
import js7.data.parser.CatsBasicParsers.syntax.*
import js7.data.parser.CatsParsers.checkedParse
import js7.data.value.expression.Expression.*
import js7.data.value.expression.ExpressionOptimizer.optimizeExpression
import js7.data.workflow.instructions.executable.WorkflowJob
import org.jetbrains.annotations.TestOnly

object CatsExpressionParser
{
  @TestOnly
  def expr(expressionString: String): Expression =
    parseExpression(expressionString).orThrow

  def parseExpression(string: String): Checked[Expression] =
    checkedParse(string, expression.surroundedBy(w) <* end)
      .left.map(_.withPrefix("Error in expression:"))

  @TestOnly
  def exprFunction(string: String): ExprFunction =
    parseFunction(string).orThrow

  def parseFunction(string: String): Checked[ExprFunction] =
    checkedParse(string, functionDefinition.surroundedBy(w) <* end)
      .left.map(_.withPrefix("Error in functionDefinition:"))

  def parseExpressionOrFunction(string: String): Checked[Expression] =
    checkedParse(string, expressionOrFunction.surroundedBy(w) <* end)

  private val parameterList: Parser[List[String]] =
    inParentheses(commaSequence(identifier))

  private val functionDefinition: Parser[ExprFunction] =
    (parameterList ~ ((w ~ string("=>") ~ w) *> expression))
      .map { case (names, expression) =>
        ExprFunction(names.map(VariableDeclaration(_)), expression)
      }

  private val functionExpr: Parser[FunctionExpr] =
    functionDefinition.map(FunctionExpr(_))

  private val expressionOrFunction: Parser[Expression] =
    functionExpr | expression

  private val listExpr: Parser[ListExpression] =
    bracketCommaSequence(expression)
      .map(ListExpression(_))

  private val parenthesizedExpression: Parser[Expression] =
    inParentheses(expression)

  private val trueConstant: Parser[BooleanConstant] =
    keyword("true").as(BooleanConstant(true))

  private val falseConstant: Parser[BooleanConstant] =
    keyword("false").as(BooleanConstant(false))

  private val catchCount: Parser[OrderCatchCount.type] =
    keyword("catchCount").as(OrderCatchCount)

  val booleanConstant =
    trueConstant | falseConstant

  private val numericConstant: Parser[NumericConstant] =
    bigDecimal.map(o => NumericConstant(o))

  private val singleQuotedStringConstant: Parser[StringConstant] =
    singleQuoted.map(StringConstant.apply)

  private val curlyName: Parser[Expression] =
    (identifier ~ (char('.') *> identifier).rep0)
      .between(char('{'), char('}'))
      .map { case (name, fields) =>
        fields
          .scanLeft[Expression](NamedValue(name))(DotExpression(_, _))
          .last
      }

  private val interpolatedStringContent: Parser0[StringExpression] = {
    val namedValue = (identifier | digits/*regex group*/).map(NamedValue(_))
    val expr = char('$') *> (namedValue | curlyName | inParentheses(expression))
    val simpleConstant = charsWhile(ch => ch != '"' && ch != '\\' && ch != '$').string
    val constant = (simpleConstant | escapedCharInString).rep
      .map(_.toList.mkString)
      .map(StringConstant(_))

    (constant.? ~ (expr ~ constant.?).rep0)
      .map {
        case (None, Nil) => StringConstant.empty
        case (Some(o: StringConstant), Nil) => o
        case (maybeFirst, pairs) =>
          optimizeExpression(InterpolatedString(
            maybeFirst.toList :::
              pairs.flatMap { case (expr, constant) => expr :: constant.toList })
          ).asInstanceOf[StringExpression]
      }
  }

  private val interpolatedString: Parser[StringExpression] =
    interpolatedStringContent.with1.surroundedBy(char('"'))

  private val objectExpr: Parser[ObjectExpression] =
    curly(commaSequence(identifier ~ ((w ~ char(':') ~ w) *> expression)))
      .flatMap(pairs =>
        checkedToParser(pairs.checkUniqueness(_._1))
          .as(ObjectExpression(pairs.toMap)))

  def dollarNamedValue: Parser[Expression] =  {
    //def arg: Parser[NamedValue] = (("arg::" ~ identifier)
    //  .map(key => NamedValue(NamedValue.Argument, StringConstant(key))))
    //def byLabel: Parser[NamedValue] = (("label::" ~ identifier ~ "." ~ identifier)
    //  .map { case (jobName, key) => NamedValue(NamedValue.ByLabel(Label(jobName)), StringConstant(key)) })
    //def byJob: Parser[NamedValue] = (("job::" ~ identifier ~ "." ~ identifier)
    //  .map { case (jobName, key) => NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name(jobName)), StringConstant(key)) })
    //def byPrefix: Parser[NamedValue] = ((identifier ~ "." ~ identifier)
    //  .map { case (prefix, key) => NamedValue(NamedValue.LastOccurredByPrefix(prefix), StringConstant(key)) })
    //def curlyName: Parser[NamedValue] = ("{" ~ (/*arg | byLabel | byJob | byPrefix |*/ nameOnly(identifier)) ~ "}"./)
    string("$") *>
      ((identifier | digits/*regex group*/).map(NamedValue(_)) | curlyName)
  }

  private val jobResourcePath: Parser[JobResourcePath] = (charWhere(JobResourcePath.isNameStart) ~ charsWhile0(JobResourcePath.isNamePartMaybe)).string
    .flatMap(o => checkedToParser(JobResourcePath.checked(o)))

  private val jobResourceVariable: Parser[JobResourceVariable] =
    (string("JobResource:") *> jobResourcePath ~ (char(':') *> identifier).?)
      .map((JobResourceVariable.apply(_, _)).tupled)

  private val argumentFunctionCall: Parser[NamedValue] =
    (keyword("argument") ~ w) *>
      inParentheses(
        for {
          kv <- keyValues(
            keyValue("default", expression) |
              keyValue("key", expression) |
              keyValue("", expression))
          key <- kv.oneOf[Expression]("key", "").map(_._2)
          default <- kv.get[Expression]("default")
        } yield NamedValue(NamedValue.Argument, key, default))

  private val namedValueKeyValue: Parser[(String, Any)] =
    keyValueConvert("label", identifier)(o => Right(NamedValue.ByLabel(o))) |
    keyValueConvert("job", identifier)(o => Right(NamedValue.LastExecutedJob(WorkflowJob.Name(o)))) |
    keyValue("default", expression)


  private val variableFunctionCall: Parser[NamedValue] =
    (keyword("variable") ~ w) *>
      inParentheses(
        for {
          kv <- keyValues(namedValueKeyValue
            | keyValue("key", expression)
            | keyValue("", expression))
          where <- kv.oneOfOr[NamedValue.Where](Set("label", "job"), NamedValue.LastOccurred)
          key <- kv.oneOf[Expression]("key", "").map(_._2)
          default <- kv.get[Expression]("default")
        } yield NamedValue(where, key, default))

  private val functionCall: Parser[Expression] =
    (identifier ~~ inParentheses(commaSequence(
      (identifier <* (w ~ char('=') ~ w)).backtrack.? ~~ expression/*OrFunction*/))
    ).flatMap {
      case ("toBoolean", arguments) =>
        arguments match {
          case Seq((None, arg)) => pure(ToBoolean(arg))
          case _ => failWith("toBoolean function expects exacly one argument")
        }
      case ("toNumber", arguments) =>
        arguments match {
          case Seq((None, arg)) => pure(ToNumber(arg))
          case _ => failWith("toNumber function expects exacly one argument")
        }
      case ("stripMargin", arguments) =>
        arguments match {
          case Seq((None, arg)) => pure(StripMargin(arg))
          case _ => failWith("stripMargin function expects exacly one argument")
        }
      case ("mkString", arguments) =>
        arguments match {
          case Seq((None, arg)) => pure(MkString(arg))
          case _ => failWith("mkString function expects exacly one argument")
        }
      case ("replaceAll", arguments) =>
        arguments match {
          case Seq((None, string), (None, pattern), (None, replacement)) =>
            pure(ReplaceAll(string, pattern, replacement))
          case _ => failWith("replaceAll function expects exacly three arguments")
        }
      case (name, arguments) =>
        pure(FunctionCall(
          name,
          arguments.map { case (maybeName, expr) => Argument(expr, maybeName) }))
    }

  private val missing: Parser[MissingConstant] =
    string("missing").as(MissingConstant())

  private val nullConstant: Parser[NullConstant] =
    string("null").as(NullConstant)

  private val factor =
    parenthesizedExpression | booleanConstant | numericConstant |
      singleQuotedStringConstant | interpolatedString | listExpr | objectExpr | dollarNamedValue |
      catchCount | argumentFunctionCall | variableFunctionCall |
      missing | nullConstant |
      jobResourceVariable |
      functionCall

  private val dotExpression =
    (factor ~ (char('.').surroundedBy(w).backtrack.with1 *> identifier).rep0).flatMap {
      case (o, Seq()) => pure(o)
      // TODO Don't use these legacy names:
      case (o, Seq("toNumber")) => pure(ToNumber(o))
      case (o, Seq("toBoolean")) => pure(ToBoolean(o))
      case (o, Seq("stripMargin")) => pure(StripMargin(o))
      case (o, Seq("mkString")) => pure(MkString(o))
      case (o, fields) => pure(fields.scanLeft[Expression](o)(DotExpression(_, _)).last)
    }

  private val argumentExpression =
    (dotExpression ~~ inParentheses(expression).?)
      .map {
        case (o, None) => o
        case (o, Some(arg)) => ArgumentExpression(o, arg)
      }

  private val bFactor =
    not | argumentExpression

  private lazy val not: Parser[Expression] =
    Parser.defer(
      ((char('!') ~ w) *> bFactor).flatMap {
        case o: BooleanExpression => pure(Not(o))
        case _ => failWith("Operator '!' requires a Boolean expression")
      })

  private val multiplication: Parser[Expression] =
    leftRecurse(bFactor, stringIn(List("*", "/")).string, bFactor) {
      case (a, ("*", b)) => Multiply(a, b)
      case (a, ("/", b)) => Divide(a, b)
      case (_, (x, _)) => throw new MatchError(x)
    }

  private val addition: Parser[Expression] =
    leftRecurse(multiplication, stringIn(List("++", "+", "-")).string, multiplication) {
      case (a, ("++", b)) => Concat(a, b)
      case (a, ("+", b)) => Add(a, b)
      case (a, ("-", b)) => Substract(a, b)
      case (_, (x, _)) => throw new MatchError(x)
    }

  private val comparison: Parser[Expression] =
    leftRecurse(addition, stringIn(List("==", "!=", "<=", ">=", "<", ">")).string, addition) {
      case (a, ("==", b)) => Equal(a, b)
      case (a, ("!=", b)) => NotEqual(a, b)
      case (a, ("<=", b)) => LessOrEqual(a, b)
      case (a, (">=", b)) => GreaterOrEqual(a, b)
      case (a, ("<" , b)) => LessThan(a, b)
      case (a, (">" , b)) => GreaterThan(a, b)
      case (_, (x, _)) => throw new MatchError(x)
    }

  private val and: Parser[Expression] =
    leftRecurseParsers(comparison, string("&&"), comparison) {
      case (a: BooleanExpression, ((), b: BooleanExpression)) =>
        pure(And(a, b))
      case (a, ((), b)) =>
        failWith(s"Operator && requires Boolean operands: " +
          Precedence.toString(a, "&&", Precedence.And, b))
    }

  private val or: Parser[Expression] =
    leftRecurseParsers(and, string("||"), and) {
      case (a: BooleanExpression, ((), b: BooleanExpression)) => pure(Or(a, b))
      case (a, ((), b)) => failWith(s"Expected boolean operans for operator ||: " +
        Precedence.toString(a, "||", Precedence.Or, b))
    }

  private val word1Operation: Parser[Expression] =
    (or <* w).flatMap(expr =>
      char('?').?.map {
        case None => expr
        case Some(()) => OrNull(expr)
      })

  private val wordOperation: Parser[Expression] =
    leftRecurseParsers(word1Operation, keyword, word1Operation) {
      case (a, ("in", list: ListExpression)) => pure(In(a, list))
      case (_, ("in", _)) => failWith("Expected a List after operator 'in'")
      case (a, ("matches", b)) => pure(Matches(a, b))
      case (a, ("orElse", b)) => pure(OrElse(a, b))
      case (a, (op, b)) => failWith(s"Operator '$op' with unexpected operand type: " +
        Precedence.toString(a, op, Precedence.Or, b))
    }

  lazy val expression: Parser[Expression] =
    Parser.defer(wordOperation)

  val constantExpression: Parser[Expression] =
    expression
}
