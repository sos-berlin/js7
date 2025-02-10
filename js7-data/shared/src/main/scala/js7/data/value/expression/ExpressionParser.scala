package js7.data.value.expression

import cats.parse.Numbers.digits
import cats.parse.Parser.{char, charWhere, charsWhile, charsWhile0, defer, end, failWith, not, pure, string}
import cats.parse.{Parser, Parser0}
import js7.base.parser.BasicParsers.*
import js7.base.parser.Parsers.checkedParse
import js7.base.parser.Parsers.syntax.*
import js7.base.problem.Checked
import js7.base.utils.CatsUtils.syntax.mkString
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.job.JobResourcePath
import js7.data.value.expression.Expression.*
import js7.data.value.expression.ExpressionOptimizer.optimize
import js7.data.workflow.instructions.executable.WorkflowJob
import org.jetbrains.annotations.TestOnly

object ExpressionParser:

  private val isKeyword = Set(
    "argument", "false", "else", "error", "in", "if",
    "matches", "missing", "then", "true", "variable")

  val knownSymbols = Set("controllerId", "job", "maxTries", "label", "orderId",
    "timedOut", "tryCount", "workflowPath", "workflowPosition")

  //private val knownFunctions = Set(
  //  "env", "impure", "jobResourceVariable", "jobResourceVariables",
  //  "min", "max", "mkString", "now", "replaceAll",
  //  "scheduledOrEmpty", "stripMargin", "subagentIds",
  //  "toBoolean", "toNumber", "toFile")

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

  @TestOnly
  def parseQuotedString(string: String): Checked[String] =
    checkedParse(string, quotedString.surroundedBy(w) <* end)

  private val parameterList: Parser[List[String]] =
    inParentheses(commaSequence(identifier))

  private val functionDefinition: Parser[ExprFunction] =
    (parameterList.backtrack ~ ((w ~ symbol("=>") ~ w) *> expression))
      .map: (names, expression) =>
        ExprFunction(names*)(expression)

  private val functionExpr: Parser[FunctionExpr] =
    functionDefinition.map(FunctionExpr(_))

  private val expressionOrFunction: Parser[Expression] =
    functionExpr | expression

  private val listExpr: Parser[ListExpr] =
    bracketCommaSequence(expression)
      .map(ListExpr(_))

  private val parenthesizedExpression: Parser[Expression] =
    inParentheses(expression)

  private val trueConstant: Parser[BooleanConstant] =
    keyword("true").as(BooleanConstant(true))

  private val falseConstant: Parser[BooleanConstant] =
    keyword("false").as(BooleanConstant(false))

  val booleanConstant: Parser[BooleanConstant] =
    trueConstant | falseConstant

  private val numericConstant: Parser[NumericConstant] =
    bigDecimal.map(o => NumericConstant(o))

  private val singleQuotedStringConstant: Parser[StringConstant] =
    singleQuoted.map(StringConstant.apply)

  private val curlyName: Parser[Expression] =
    (identifier ~ (char('.') *> identifier).rep0)
      .between(char('{'), char('}'))
      .map: (name, fields) =>
        fields
          .scanLeft[Expression](NamedValue(name))(DotExpr(_, _))
          .last

  private val interpolatedStringContent: Parser0[StringExpr] =
    val namedValue = (identifier | digits/*regex group*/).map(NamedValue(_))
    val expr = char('$') *> (namedValue | curlyName | inParentheses(expression))
    val simpleConstant = charsWhile(ch => ch != '"' && ch != '\\' && ch != '$').string
    val constant = (simpleConstant | escapedCharInString.map(_.toString)).rep
      .map(_.mkString)
      .map(StringConstant(_))

    (constant.? ~ (expr ~ constant.?).rep0)
      .map:
        case (None, Nil) => StringConstant.empty
        case (Some(o: StringConstant), Nil) => o
        case (maybeFirst, pairs) =>
          optimize(InterpolatedString:
            maybeFirst.toList :::
              pairs.flatMap((expr, constant) => expr :: constant.toList)
          ).asInstanceOf[StringExpr]

  private val interpolatedString: Parser[StringExpr] =
    interpolatedStringContent.with1.surroundedBy(char('"'))

  private val objectExpr: Parser[ObjectExpr] =
    curly(commaSequence(identifier ~ ((w ~ symbol(":") ~ w) *> expression)))
      .flatMap: pairs =>
        checkedToParser(pairs.checkUniquenessBy(_._1))
          .as(ObjectExpr(pairs.toMap))

  def dollarNamedValue: Parser[Expression] =
    //def arg: Parser[NamedValue] = (("arg::" ~ identifier)
    //  .map(key => NamedValue(NamedValue.Argument, StringConstant(key))))
    //def byLabel: Parser[NamedValue] = (("label::" ~ identifier ~ "." ~ identifier)
    //  .map { case (jobName, key) => NamedValue(NamedValue.ByLabel(Label(jobName)), StringConstant(key)) })
    //def byJob: Parser[NamedValue] = (("job::" ~ identifier ~ "." ~ identifier)
    //  .map { case (jobName, key) => NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name(jobName)), StringConstant(key)) })
    //def byPrefix: Parser[NamedValue] = ((identifier ~ "." ~ identifier)
    //  .map { case (prefix, key) => NamedValue(NamedValue.LastOccurredByPrefix(prefix), StringConstant(key)) })
    //def curlyName: Parser[NamedValue] = ("{" ~ (/*arg | byLabel | byJob | byPrefix |*/ nameOnly(identifier)) ~ "}"./)
    char('$') *>
      ((identifier | digits/*regex group*/).map(NamedValue(_)) | curlyName)

  private val jobResourcePath: Parser[JobResourcePath] = (charWhere(JobResourcePath.isNameStart) ~ charsWhile0(JobResourcePath.isNamePartMaybe)).string
    .flatMap(o => checkedToParser(JobResourcePath.checked(o)))

  private val jobResourceVariable: Parser[JobResourceVariable] =
    (string("JobResource:") *> jobResourcePath ~ (symbol(":") *> identifier).?)
      .map((JobResourceVariable.apply(_, _)).tupled)

  private val argumentFunctionCall: Parser[NamedValue] =
    (keyword("argument") ~ w) *>
      inParentheses(
        for
          kv <- keyValues(
            keyValue("default", expression) |
              keyValue("key", expression) |
              keyValue("", expression))
          key <- kv.oneOf[Expression]("key", "").map(_._2)
          default <- kv.get[Expression]("default")
        yield NamedValue(NamedValue.Argument, key, default))

  private val namedValueKeyValue: Parser[(String, Any)] =
    keyValueConvert("label", identifier)(o => Right(NamedValue.ByLabel(o))) |
    keyValueConvert("job", identifier)(o => Right(NamedValue.LastExecutedJob(WorkflowJob.Name(o)))) |
    keyValue("default", expression)

  private val variableFunctionCall: Parser[NamedValue] =
    (keyword("variable") ~ w) *>
      inParentheses(
        for
          kv <- keyValues(namedValueKeyValue
            | keyValue("key", expression)
            | keyValue("", expression))
          where <- kv.oneOfOr[NamedValue.Where](Set("label", "job"), NamedValue.LastOccurred)
          key <- kv.oneOf[Expression]("key", "").map(_._2)
          default <- kv.get[Expression]("default")
        yield NamedValue(where, key, default))

  private val functionCall: Parser[Expression] =
    (identifier ~~
      inParentheses:
        commaSequence:
          (identifier <* (w ~ symbol("=") ~ w)).backtrack.? ~~ expression
      .?
    ).flatMap:
      case ("toBoolean", Some(arguments)) =>
        arguments match
          case Seq((None, arg)) => pure(ToBoolean(arg))
          case _ => failWith("toBoolean function expects exactly one argument")
      case ("toNumber", Some(arguments)) =>
        arguments match
          case Seq((None, arg)) => pure(ToNumber(arg))
          case _ => failWith("toNumber function expects exactly one argument")
      case ("stripMargin", Some(arguments)) =>
        arguments match
          case Seq((None, arg)) => pure(StripMargin(arg))
          case _ => failWith("stripMargin function expects exactly one argument")
      case ("mkString", Some(arguments)) =>
        arguments match
          case Seq((None, arg)) => pure(MkString(arg))
          case _ => failWith("mkString function expects exactly one argument")
      case ("impure", Some(arguments)) =>
        arguments match
          case Seq((None, arg)) => pure(Impure(arg))
          case _ => failWith("'impure' function expects exactly one argument")
      case ("replaceAll", Some(arguments)) =>
        arguments match
          case Seq((None, string), (None, pattern), (None, replacement)) =>
            pure(ReplaceAll(string, pattern, replacement))
          case _ => failWith("replaceAll function expects exactly three arguments")
      case ("substring", Some(arguments)) =>
        arguments match
          case Seq((None, string), (None, start)) =>
            pure(Substring(string, start))
          case Seq((None, string), (None, start), (None, end)) =>
            pure(Substring(string, start, Some(end)))
          case _ => failWith("substring function expects exactly three arguments")
      case ("match", Some(arguments)) =>
        arguments match
          case Seq((None, string), (None, pattern), (None, replacement)) =>
            pure(Match(string, pattern, replacement))
          case _ => failWith("match function expects exactly three arguments")
      case ("min", Some(arguments)) =>
        arguments match
          case Seq((None, a), (None, b)) => pure(Min(a, b))
          case _ => failWith("min function expects exactly two arguments")
      case ("max", Some(arguments)) =>
        arguments match
          case Seq((None, a), (None, b)) => pure(Max(a, b))
          case _ => failWith("max function expects exactly two arguments")
      case (name, Some(arguments)) =>
        pure(FunctionCall(
          name,
          Some(arguments.map((maybeName, expr) => Argument(expr, maybeName)))))
      case (name, None) =>  // "()" is optional
        if knownSymbols(name) then
          pure(FunctionCall(name, arguments = None))
        else if isKeyword(name) then
          failWith(s"Unexpected '$name' keyword, maybe parentheses around it are missing?")
        else
          failWith(s"Unknown symbol: $name")

  /** Then special function `error` .*/
  private val errorFunctionCall: Parser[ErrorExpr] =
    keyword("error") *> inParentheses(expression)
      .map(expr => ErrorExpr(expr))

  private val missingConstant: Parser[MissingConstant] =
    keyword("missing").as(MissingConstant)

  private val factor =
    parenthesizedExpression |
      booleanConstant | numericConstant |
      singleQuotedStringConstant | interpolatedString |
      listExpr | objectExpr |
      dollarNamedValue |
      missingConstant |
      jobResourceVariable |
      errorFunctionCall | argumentFunctionCall | variableFunctionCall |
      functionCall

  private val dotOrArgumentExpression =
    enum Suffix:
      case DotName(name: String)
      case Arg(expr: Expression)
    import Suffix.{Arg, DotName}

    val dotName = (char('.').surroundedBy(w).backtrack.with1 *> identifier).map(DotName(_))
    val arg = inParentheses(expression).map(Arg(_))

    ((factor <* w) ~ (dotName | arg).rep0).flatMap:
      case (o, Seq()) => pure(o)
      // TODO Don't use these legacy names:
      case (o, Seq(DotName("toNumber"))) => pure(ToNumber(o))
      case (o, Seq(DotName("toBoolean"))) => pure(ToBoolean(o))
      case (o, Seq(DotName("stripMargin"))) => pure(StripMargin(o))
      case (o, Seq(DotName("mkString"))) => pure(MkString(o))

      case (o, seq) => pure:
        seq.scanLeft[Expression](o):
          case (expr, DotName(name)) => DotExpr(expr, name)
          case (expr, Arg(arg)) => ArgumentExpr(expr, arg)
        .last

  private val questionMarkExpr: Parser[Expression] =
    ((dotOrArgumentExpression <* w) ~ (symbol("?") *> w *> dotOrArgumentExpression.?).rep0)
      .map: (a, more) =>
        more.foldLeft(a):
          case (a, None) => OrMissing(a)
          case (a, Some(b)) => Catch(a, b)

  private val negate: Parser[Expression] =
    ((symbol("-") *> w *> not(numericConstant)).backtrack *> defer(questionMarkExpr))
      .map(Negate(_))

  private val notOperator: Parser[Expression] =
    ((symbol("!") ~ w) *> defer(questionMarkExpr))
      .map(Not(_))

  private val prefixOperatorExpr: Parser[Expression] =
    notOperator | negate | questionMarkExpr

  private val multiplication: Parser[Expression] =
    leftRecurse(prefixOperatorExpr, symbol("*").as('*') | symbol("/").as('/'), prefixOperatorExpr):
      case (a, ('*', b)) => Multiply(a, b)
      case (a, ('/', b)) => Divide(a, b)
      case (_, (x, _)) => throw MatchError(x)

  private val addition: Parser[Expression] =
    leftRecurse(multiplication, symbol("++" :: "+" :: "-" :: Nil), multiplication):
      case (a, ("++", b)) => Concat(a, b)
      case (a, ("+", b)) => Add(a, b)
      case (a, ("-", b)) => Substract(a, b)
      case (_, (x, _)) => throw MatchError(x)

  private val comparison: Parser[Expression] =
    leftRecurse(addition, symbol("<=" :: ">=" :: "<" :: ">" :: Nil), addition):
      case (a, ("<=", b)) => LessOrEqual(a, b)
      case (a, (">=", b)) => GreaterOrEqual(a, b)
      case (a, ("<" , b)) => LessThan(a, b)
      case (a, (">" , b)) => GreaterThan(a, b)
      case (_, (x, _)) => throw MatchError(x)

  private val equal: Parser[Expression] =
    leftRecurse(comparison, symbol("==" :: "!=" :: Nil), comparison):
      case (a, ("==", b)) => Equal(a, b)
      case (a, ("!=", b)) => NotEqual(a, b)
      case (_, (x, _)) => throw MatchError(x)

  private val and: Parser[Expression] =
    leftRecurseParsers(equal, symbol("&&"), equal):
      case (a, ((), b)) => pure(And(a, b))

  private val or: Parser[Expression] =
    leftRecurseParsers(and, symbol("||"), and):
      case (a, ((), b)) => pure(Or(a, b))

  private val wordOperation: Parser[Expression] =
    leftRecurseParsers(or, keyword("in" :: "matches" :: Nil), or):
      case (a, ("in", list: ListExpr)) => pure(In(a, list))
      case (_, ("in", _)) => failWith("Expected a List after operator 'in'")
      case (a, ("matches", b)) => pure(Matches(a, b))
      case (a, (op, b)) => failWith(s"Unknown operator '$op': " +
        Precedence.toString(a, op, Precedence.Or, b))

  private val ifThenElseOperation: Parser[Expression] =
    locally:
      val if_ = keyword("if") *> w *> wordOperation
      val then_ = keyword("then") *> w *> defer(ifThenElseOperation)
      val else_ = keyword("else") *> w *> defer(ifThenElseOperation)
      (if_ ~ (w *> then_ <* w) ~ else_).map:
        case ((condition, then_), else_) => IfThenElse(condition, then_, else_)
    | wordOperation

  val constantExpression: Parser[Expression] =
    expression

  // Not a val due to recursive usage
  def expression: Parser[Expression] =
    defer:
      ifThenElseOperation
