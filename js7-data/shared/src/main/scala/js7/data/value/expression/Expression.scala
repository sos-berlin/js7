package js7.data.value.expression

import cats.syntax.all.*
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json, JsonObject}
import java.lang.Character.{isUnicodeIdentifierPart, isUnicodeIdentifierStart}
import java.util.regex.{Pattern, PatternSyntaxException}
import js7.base.circeutils.CirceUtils.toDecoderResult
import js7.base.log.Logger
import js7.base.parser.BasicPrinter.{appendIdentifier, appendIdentifierWithBackticks, identifierToString, isIdentifierPart}
import js7.base.problem.Checked.{CheckedOption, catchExpected, catchNonFatalDontLog}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichOption}
import js7.base.utils.ScalaUtils.withStringBuilder
import js7.base.utils.typeclasses.IsEmpty
import js7.data.job.JobResourcePath
import js7.data.value.ValuePrinter.appendQuotedContent
import js7.data.value.ValueType.{ErrorInExpressionProblem, UnexpectedValueTypeProblem, UnknownNameInExpressionProblem}
import js7.data.value.expression.ExpressionParser.parseExpression
import js7.data.value.{BooleanValue, FunctionValue, GoodValue, ListValue, MissingValue, NumberValue, ObjectValue, StringValue, Value, ValuePrinter, ValueType}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Label
import org.jetbrains.annotations.TestOnly
import scala.collection.{View, mutable}
import scala.language.implicitConversions
import scala.math.Ordered.orderingToOrdered
import scala.util.chaining.scalaUtilChainingOps

/**
  * @author Joacim Zschimmer
  */
sealed trait Expression extends HasPrecedence:
  def subexpressions: Iterable[Expression]

  /** Expression is pure (yields always the same result) and
   * can be evaluated with the EmptyScope.
   * <p>
   *   Only expressions requiring a Scope may be impure (depending on the scope).
   */
  def isPure: Boolean

  def referencedJobResourcePaths: Iterable[JobResourcePath] =
    subexpressions.view
      .flatMap(_.referencedJobResourcePaths)
      .toSet

  final def evalAs[V <: Value](implicit V: Value.Companion[V], scope: Scope): Checked[V] =
    eval.flatMap(_.as[V])

  final def eval(implicit scope: Scope): Checked[Value] =
    evalRaw match
      case Left(UnexpectedValueTypeProblem(_, MissingValue)) => Right(MissingValue)
      case Left(problem) => Left(problem)
      case Right(o: Value) => Right(o)

  protected def evalRaw(using scope: Scope): Checked[Value]

  final def evalAsBoolean(implicit scope: Scope): Checked[Boolean] =
    eval.flatMap(_.as[BooleanValue]).map(_.booleanValue)

  final def evalAsNumber(implicit scope: Scope): Checked[BigDecimal] =
    eval.flatMap(_.as[NumberValue]).map(_.number)

  final def evalAsInt(implicit scope: Scope): Checked[Int] =
    evalAsNumber.flatMap: o =>
      catchExpected[ArithmeticException](o.toIntExact)

  final def evalToString(implicit scope: Scope): Checked[String] =
    eval.flatMap(_.toStringValueString)

  final def evalAsString(implicit scope: Scope): Checked[String] =
    eval.flatMap(_.as[StringValue]).map(_.string)

  final def evalAsVector(implicit scope: Scope): Checked[Vector[Value]] =
    eval.flatMap(_.as[ListValue]).map(_.elements)


object Expression:
  private val logger = Logger[this.type]

  @TestOnly
  val LastReturnCode: NamedValue = NamedValue("returnCode")

  given jsonEncoder: Encoder[Expression] = expr => Json.fromString(expr.toString)
  given jsonDecoder: Decoder[Expression] = c =>
    c.as[String].flatMap:
      parseExpression(_).toDecoderResult(c.history)


  sealed transparent trait IsPureIfSubexpressionsArePure extends Expression:
    def isPure = subexpressions.forall(_.isPure)


  sealed transparent trait IsPure extends Expression:
    final def isPure = true

  sealed transparent trait IsImpure extends Expression:
    final def isPure = false


  sealed trait Constant extends IsPure:
    final def precedence: Int = Precedence.Factor
    final def subexpressions: Iterable[Expression] = Nil
    def toValue: Value

    final def evalRaw(using scope: Scope): Checked[Value] =
      Right(toValue)

  sealed trait SimpleValueExpr extends Expression

  sealed trait BooleanExpr extends SimpleValueExpr

  sealed trait NumericExpr extends SimpleValueExpr

  sealed trait StringExpr extends SimpleValueExpr


  final case class IfThenElse(condition: Expression, thenExpr: Expression, elseExpr: Expression)
  extends BooleanExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.IfThenElse
    def subexpressions: Iterable[Expression] =
      condition :: thenExpr :: elseExpr :: Nil

    protected def evalRaw(using scope: Scope) =
      for
        bool <- condition.evalAsBoolean
        result <- (if bool then thenExpr else elseExpr).eval
      yield
        result

    override def toString: String =
      "if " + inParentheses(condition) +
        " then " + inParentheses(thenExpr) +
        " else " + inParentheses(elseExpr)


  final case class Negate(a: Expression)
  extends NumericExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Factor

    def subexpressions: Seq[Expression] =
      a :: Nil

    protected def evalRaw(using scope: Scope) =
      a.eval.flatMap:
        case NumberValue(a) => Right(NumberValue(-a))
        case a => unexpectedType(NumberValue, a)

    override def toString: String =
      "-" + inParentheses(a)


  final case class Not(a: Expression)
  extends BooleanExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Factor
    def subexpressions: Iterable[Expression] = a.subexpressions

    protected def evalRaw(using scope: Scope) =
      for a <- a.evalAsBoolean yield
        BooleanValue(!a.booleanValue)

    override def toString: String = "!" + inParentheses(a)


  final case class And(a: Expression, b: Expression)
  extends BooleanExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.And
    def subexpressions: Iterable[Expression] = View(a, b)

    protected def evalRaw(using scope: Scope) =
      a.evalAsBoolean.flatMap:
        case false => Right(BooleanValue.False)
        case true => b.eval.flatMap(_.as[BooleanValue])

    override def toString: String = makeString(a, "&&", b)


  final case class Or(a: Expression, b: Expression)
  extends BooleanExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Or
    def subexpressions: Iterable[Expression] = View(a, b)

    protected def evalRaw(using scope: Scope) =
      a.evalAsBoolean.flatMap:
        case false => b.eval.flatMap(_.as[BooleanValue])
        case true => Right(BooleanValue.True)

    override def toString: String = makeString(a, "||", b)


  final case class Equal(a: Expression, b: Expression)
  extends BooleanExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Equal
    def subexpressions: Iterable[Expression] = View(a, b)

    protected def evalRaw(using scope: Scope) =
      for a <- a.eval; b <- b.eval yield BooleanValue(a == b)

    override def toString: String = makeString(a, "==", b)


  final case class NotEqual(a: Expression, b: Expression)
  extends BooleanExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Equal
    def subexpressions: Iterable[Expression] = View(a, b)

    protected def evalRaw(using scope: Scope) =
      for a <- a.eval; b <- b.eval yield BooleanValue(a != b)

    override def toString: String = makeString(a, "!=", b)


  sealed trait IsComparable(
    compareNumbers: (NumberValue, NumberValue) => Boolean,
    compareStrings: (StringValue, StringValue) => Boolean,
    repr: String)
  extends BooleanExpr, IsPureIfSubexpressionsArePure:
    val a, b: Expression

    def precedence: Int = Precedence.Comparison
    def subexpressions: Iterable[Expression] = View(a, b)

    protected def evalRaw(using scope: Scope) =
      for
        a <- a.eval
        b <- b.eval
        result <- (a, b) match
          case (a: NumberValue, b: NumberValue) =>
            Right(BooleanValue(compareNumbers(a, b)))
          case (a: StringValue, b: StringValue) =>
            Right(BooleanValue(compareStrings(a, b)))
          case (a, b) =>
            if a == MissingValue || b == MissingValue then
              Right(MissingValue)
            else
              Left(Problem(s"${a.valueType} and ${b.valueType} are not comparable"))
      yield
        result

    override def toString: String = makeString(a, repr, b)


  final case class LessOrEqual(a: Expression, b: Expression)
    extends IsComparable(_ <= _, _ <= _, "<=")


  final case class GreaterOrEqual(a: Expression, b: Expression)
    extends IsComparable(_ >= _, _ >= _, ">=")


  final case class LessThan(a: Expression, b: Expression)
    extends IsComparable(_ < _, _ < _, "<")


  final case class GreaterThan(a: Expression, b: Expression)
    extends IsComparable(_ > _, _ > _, ">")


  final case class Concat(a: Expression, b: Expression)
  extends BooleanExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Addition
    def subexpressions: Iterable[Expression] = View(a, b)

    protected def evalRaw(using scope: Scope) =
      for
        a <- a.eval
        b <- b.eval
        result <- (a, b) match
          case (a: ListValue, b: ListValue) =>
            Right(ListValue(a.elements ++ b.elements))
          case (a, b) =>
            for a <- a.toStringValue; b <- b.toStringValue yield
              StringValue(a.string + b.string)
      yield result

    override def toString: String = makeString(a, "++", b)


  sealed trait NumericBinaryExpr
  extends NumericExpr, IsPureIfSubexpressionsArePure:
    final def subexpressions: Iterable[Expression] = View(a, b)

    protected final def evalRaw(using scope: Scope) =
      (a.eval, b.eval) match
        case (Left(problem), _) => Left(problem)
        case (_, Left(problem)) => Left(problem)
        case (Right(NumberValue(a)), Right(NumberValue(b))) =>
          catchNonFatalDontLog:
            NumberValue(op(a, b))
        case (Right(_: NumberValue), Right(b)) => unexpectedType(NumberValue, b)
        case (Right(a), Right(_)) => unexpectedType(NumberValue, a)

    protected val a: Expression
    protected val b: Expression
    protected def op(a: BigDecimal, b: BigDecimal): BigDecimal


  final case class Multiply(a: Expression, b: Expression)
  extends NumericBinaryExpr:
    def precedence: Int = Precedence.Multiplication

    protected def op(a: BigDecimal, b: BigDecimal) = a * b

    override def toString: String =
      makeString(a, "*", b)


  final case class Divide(a: Expression, b: Expression)
  extends NumericBinaryExpr:
    def precedence: Int = Precedence.Multiplication

    protected def op(a: BigDecimal, b: BigDecimal) = a / b

    override def toString: String = makeString(a, "/", b)


  final case class Add(a: Expression, b: Expression)
  extends NumericBinaryExpr:
    def precedence: Int = Precedence.Addition

    protected def op(a: BigDecimal, b: BigDecimal) = a + b

    override def toString: String = makeString(a, "+", b)


  final case class Substract(a: Expression, b: Expression)
  extends NumericBinaryExpr:
    def precedence: Int = Precedence.Addition

    protected def op(a: BigDecimal, b: BigDecimal) = a - b

    override def toString: String = makeString(a, "-", b)


  final case class In(a: Expression, b: ListExpr)
  extends BooleanExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.WordOperator
    def subexpressions: Iterable[Expression] = View(a, b)

    protected def evalRaw(using scope: Scope) =
      for a <- a.eval; b <- b.evalAsVector yield BooleanValue(b contains a)

    override def toString: String = makeString(a, "in", b)


  final case class Matches(a: Expression, b: Expression)
  extends BooleanExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.WordOperator
    def subexpressions: Iterable[Expression] = View(a, b)

    protected def evalRaw(using scope: Scope) =
      for
        a <- a.eval.map(_.missingToEmptyString).flatMap(_.toStringValueString)
        b <- b.evalToString
        result <- catchExpected[PatternSyntaxException](
          BooleanValue(a.matches(b)))
      yield result

    override def toString: String = makeString(a, "matches", b)


  final case class Catch(a: Expression, default: Expression)
  extends BooleanExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.OrElse
    def subexpressions: Iterable[Expression] = a :: default :: Nil

    protected def evalRaw(using scope: Scope) =
      evalOrDefault(a, default)

    override def toString: String = makeString(a, "?", default)


  final case class OrMissing(a: Expression)
  extends BooleanExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.OrElse
    def subexpressions: Iterable[Expression] = a :: Nil

    protected def evalRaw(using scope: Scope) =
      evalOrDefault(a, MissingConstant)

    override def toString: String =
      inParentheses(a) + "?"


  private def evalOrDefault(expr: Expression, default: Expression)(implicit scope: Scope)
  : Checked[Value] =
    expr.eval match
      case Left(problem) =>
        logger.trace(s"?-operator catched $problem")
        default.eval
      case Right(MissingValue) => default.eval
      case Right(o: GoodValue) => Right(o)


  final case class ArgumentExpr(obj: Expression, arg: Expression)
  extends IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Factor
    def subexpressions: Iterable[Expression] = obj :: arg :: Nil

    protected def evalRaw(using scope: Scope) =
      for
        list <- obj.evalAsVector
        index <- arg.evalAsInt
        _ <- (index >= 0 && index < list.length) !!
          Problem(s"Index $index out of range 0...${list.length - 1}")
      yield list(index)

    override def toString: String =
      inParentheses(obj) + "(" + arg + ")"


  final case class DotExpr(a: Expression, name: String) extends IsPureIfSubexpressionsArePure:
    def subexpressions: Iterable[Expression] = a :: Nil
    def precedence: Int = Precedence.Dot

    protected def evalRaw(using scope: Scope) =
      for
        a <- a.evalAs[ObjectValue]
        a <- a.nameToValue.get(name) !! UnknownNameInExpressionProblem(s"$toString.$name")
      yield a

    override def toString: String =
      inParentheses(a) + "." + identifierToString(name)


  final case class ListExpr(subexpressions: List[Expression]) extends IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Factor

    protected def evalRaw(using scope: Scope) =
      subexpressions.traverse(_.eval).map(_.toVector).map(ListValue(_))

    override def toString: String = subexpressions.mkString("[", ", ", "]")


  final case class ObjectExpr(nameToExpr: Map[String, Expression])
  extends IsPureIfSubexpressionsArePure:
    def subexpressions: Iterable[Expression] = nameToExpr.values
    def isEmpty: Boolean = nameToExpr.isEmpty
    def nonEmpty: Boolean = nameToExpr.nonEmpty
    def precedence: Int = Precedence.Factor

    protected def evalRaw(using scope: Scope) =
      nameToExpr.toVector
        .traverse { case (k, v) => v.eval.map(k -> _) }
        .map(pairs => ObjectValue(pairs.toMap))

    override def toString: String = nameToExpr
      .map { case (k, v) => identifierToString(k) + ":" + v }
      .mkString("{", ", ", "}")

  object ObjectExpr:
    val empty: ObjectExpr = ObjectExpr(Map.empty)

    implicit val objectExpressionIsEmpty: IsEmpty[ObjectExpr] =
      IsEmpty[ObjectExpr](_.isEmpty)

    implicit val jsonEncoder: Encoder.AsObject[ObjectExpr] =
      o => JsonObject.fromIterable(o.nameToExpr.view.mapValues(_.asJson).toSeq)

    implicit val jsonDecoder: Decoder[ObjectExpr] =
      _.as[Map[String, Expression]] map ObjectExpr.apply


  final case class FunctionExpr(function: ExprFunction)
  extends Expression.IsPureIfSubexpressionsArePure:
    def subexpressions: Iterable[Expression] = function.expression :: Nil

    protected def evalRaw(using scope: Scope) =
      Right(FunctionValue(function))

    protected def precedence = Precedence.Function

    override def toString: String = function.toString


  final case class ToNumber(expression: Expression)
  extends NumericExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Factor
    def subexpressions: Iterable[Expression] = expression :: Nil

    protected def evalRaw(using scope: Scope) =
      expression.eval.flatMap(_.toNumberValue)

    override def toString = s"toNumber($expression)"


  final case class ToBoolean(expression: Expression)
  extends BooleanExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Factor
    def subexpressions: Iterable[Expression] = expression :: Nil

    protected def evalRaw(using scope: Scope) =
      expression.eval.flatMap(_.toBooleanValue)

    override def toString = s"toBoolean($expression)"


  final case class BooleanConstant(booleanValue: Boolean)
  extends BooleanExpr, Constant:
    val toValue: Value = BooleanValue(booleanValue)

    override def toString: String = booleanValue.toString

  object BooleanConstant:
    val False: BooleanConstant = new BooleanConstant(false)
    val True: BooleanConstant = new BooleanConstant(true)

    def apply(booleanValue: Boolean): BooleanConstant =
      if booleanValue then True else False


  final case class NumericConstant(number: BigDecimal)
  extends NumericExpr, Constant:
    val toValue: Value = NumberValue(number)

    override def toString: String = number.toString


  final case class StringConstant(string: String)
  extends StringExpr, Constant:
    val toValue: Value = StringValue(string)

    override def toString: String = StringConstant.quote(string)

  object StringConstant:
    val empty = new StringConstant("")

    def quote(string: String): String =
      ValuePrinter.quoteString(string)


  final case class NamedValue(
    where: NamedValue.Where, nameExpr: Expression, default: Option[Expression] = None)
  extends IsImpure:
    import NamedValue.*
    def precedence: Int = Precedence.Factor
    def subexpressions: Iterable[Expression] = Nil

    protected def evalRaw(using scope: Scope) =
      val w = where match
        case NamedValue.Argument => ValueSearch.Argument
        case NamedValue.LastOccurred => ValueSearch.LastOccurred
        case NamedValue.ByLabel(label) => ValueSearch.LastExecuted(PositionSearch.ByLabel(label))
        case NamedValue.LastExecutedJob(jobName) => ValueSearch.LastExecuted(PositionSearch.ByWorkflowJob(jobName))
      for
        name <- nameExpr.evalAsString
        maybeValue <- scope.findValue(ValueSearch(w, ValueSearch.Name(name))).sequence
        value <- maybeValue
          .map(Right(_))
          .getOrElse(
            default.map(_.eval.flatMap(_.toStringValue))
              .toChecked(
                where match {
                  case NamedValue.Argument =>
                    Problem(s"No such order argument: $name")
                  case NamedValue.LastOccurred =>
                    Problem(s"No such named value: $name")
                  case NamedValue.ByLabel(Label(label)) =>
                    Problem(s"Workflow instruction at label $label did not return a named value '$name'")
                  case NamedValue.LastExecutedJob(WorkflowJob.Name(jobName)) =>
                    Problem(s"Last execution of job '$jobName' did not return a named value '$name'")
                })
              .flatten)
      yield
        value

    override def toString: String = (where, nameExpr, default) match
      case (LastOccurred, StringConstant(key), None) if !key.contains('`') =>
        if isSimpleName(key) || key.forall(c => c >= '0' && c <= '9') then
          s"$$$key"
        else
          s"$$`$key`"

      case (LastOccurred, nameExpr, None) => s"variable($nameExpr)"

      //case (Argument, NamedValue(StringConstant(key)), None) if isIdentifier(key) => s"$${arg::$key}"
      //case (LastOccurredByPrefix(prefix), NamedValue(StringConstant(key)), None) if isIdentifier(key) => s"$${$prefix.$key}"
      //case (ByLabel(Label(label)), NamedValue(StringConstant(key)), None) if isIdentifier(key) => s"$${label::$label.$key}"
      //case (LastExecutedJob(WorkflowJob.Name(jobName)), NamedValue(StringConstant(key)), None) if isIdentifier(key) => s"$${job::$jobName.$key}"
      case _ =>
        val args = mutable.Buffer.empty[String]
        lazy val sb = new StringBuilder
        args += s"key=$nameExpr"
        where match
          case NamedValue.Argument =>
          case NamedValue.LastOccurred =>

          case NamedValue.LastExecutedJob(jobName) =>
            sb.clear()
            sb.append("job=")
            appendIdentifier(sb, jobName.string)
            args += sb.toString

          case NamedValue.ByLabel(label) =>
            sb.clear()
            sb.append("label=")
            appendIdentifier(sb, label.string)
            args += sb.toString
        for d <- default do args += "default=" + d.toString
        (where, nameExpr) match
          case (NamedValue.Argument, _) =>
            s"argument(${args mkString ", "})"

          case _ =>
            s"variable(${args mkString ", "})"

  object NamedValue:
    def apply(name: String): NamedValue =
      NamedValue(NamedValue.LastOccurred, StringConstant(name))

    def apply(name: String, default: Expression): NamedValue =
      NamedValue(NamedValue.LastOccurred, StringConstant(name), Some(default))

    def argument(name: String): NamedValue =
      NamedValue(NamedValue.Argument, StringConstant(name))

    private[Expression] def isSimpleName(name: String) =
      name.nonEmpty && isSimpleNameStart(name.head) && name.tail.forall(isSimpleNamePart)

    private[expression] def isSimpleNameStart(c: Char) =
      isUnicodeIdentifierStart(c)

    private[expression] def isSimpleNamePart(c: Char) =
      isUnicodeIdentifierPart(c)

    sealed trait Where
    case object LastOccurred extends Where
    final case class LastExecutedJob(jobName: WorkflowJob.Name) extends Where
    final case class ByLabel(label: Label) extends Where
    case object Argument extends Where


  //// A name different from predefined scala.Symbol
  ///** Like a function call without parameter list. */
  //final case class Symbol_(name: String) extends IsImpure:
  //  def precedence: Int = Precedence.Factor
  //
  //  def subexpressions: Iterable[Expression] = Nil
  //
  //  def evalRaw(using scope: Scope): Checked[Value] =
  //    scope.symbolToValue.applyOrElse(name, _ => Left(Problem(s"Unknown symbol: $name")))
  //
  //  override def toString = name


  final case class FunctionCall(name: String, arguments: Option[Seq[Argument]])
  extends IsImpure:
    protected def precedence = Precedence.Factor
    def subexpressions: Iterable[Expression] = Nil

    def evalRaw(using scope: Scope): Checked[Value] =
      scope.evalFunctionCall(this)
        .getOrElse(Left(Problem(s"Unknown function: $name")))

    override def toString = s"$name${arguments.fold("")(a => s"(${a.mkString(", ")})")}"


  final case class Argument(expression: Expression, maybeName: Option[String] = None):
    override def toString = maybeName.fold("")(_ + "=") + expression


  final case class JobResourceVariable(jobResourcePath: JobResourcePath, name: Option[String])
  extends IsImpure:
    protected def precedence = Precedence.Factor

    def subexpressions: Iterable[Expression] = Nil

    override def referencedJobResourcePaths: Iterable[JobResourcePath] = jobResourcePath :: Nil

    def evalRaw(using scope: Scope): Checked[Value] =
      scope.evalJobResourceVariable(this)
        .getOrElse(Left(Problem(s"JobResources are not accessible here: $toString")))

    override def toString: String = withStringBuilder(64) { sb =>
      sb.append("JobResource")
      sb.append(':')
      sb.append(jobResourcePath.string)
      for nam <- name do
        sb.append(':')
        appendIdentifier(sb, nam)
    }


  final case class StripMargin(a: Expression)
  extends StringExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Factor
    def subexpressions: Iterable[Expression] = a :: Nil

    def evalRaw(using scope: Scope): Checked[Value] =
      for a <- a.evalAsString yield
        StringValue(a.stripMargin)

    override def toString = s"stripMargin($a)"


  final case class MkString(expression: Expression)
  extends StringExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Factor
    def subexpressions: Iterable[Expression] = expression :: Nil

    def evalRaw(using scope: Scope): Checked[Value] =
      expression.eval.map :
        case ListValue(list) => StringValue(list.map(_.convertToString).mkString)
        case value => StringValue(value.convertToString)

    override def toString = s"mkString($expression)"


  /** Make expression appeare impure — for testing. */
  final case class Impure(expression: Expression) extends IsImpure:
    def precedence: Int = Precedence.Factor
    def subexpressions: Iterable[Expression] = expression :: Nil

    def evalRaw(using scope: Scope): Checked[Value] =
      expression.evalRaw

    override def toString = s"impure($expression)"


  /** Like MkString, but with a different toString representation. */
  final case class InterpolatedString(subexpressions: List[Expression])
  extends StringExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Factor

    def evalRaw(using scope: Scope): Checked[Value] =
      subexpressions
        .traverse(_.eval.map(_.convertToString))
        .map(seq => StringValue(seq.mkString))

    override def toString: String =
      withStringBuilder { sb =>
        sb.append('"')
        subexpressions.tails foreach:
          case StringConstant(string) :: _ =>
            appendQuotedContent(sb, string)

          case NamedValue(NamedValue.LastOccurred, StringConstant(name), None) :: next =>
            sb.append('$')
            next match
              case StringConstant(next) :: Nil if next.headOption.forall(isIdentifierPart) =>
                appendIdentifierWithBackticks(sb, name)
              case _ =>
                appendIdentifier(sb, name)

          case expr :: _ =>
            sb.append("$(")
            sb.append(expr)
            sb.append(')')

          case Nil =>
        sb.append('"')
      }


  final case class ReplaceAll(string: Expression, pattern: Expression, replacement: Expression)
  extends StringExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Factor
    def subexpressions: Iterable[Expression] = View(string, pattern, replacement)

    private val precompiledPattern = precompilePattern(pattern)

    def evalRaw(using scope: Scope): Checked[Value] =
      for
        string <- string.eval.flatMap(_.asString)
        pattern <- precompiledPattern.getOrElse(compilePattern(pattern))
        replacement <- replacement.eval.flatMap(_.asString)
        result <- catchExpected[RuntimeException]:
          StringValue(pattern.matcher(string).replaceAll(replacement))
      yield
        result

    override def toString = s"replaceAll($string, $pattern, $replacement)"


  final case class Substring(string: Expression, start: Expression, end: Option[Expression] = None)
  extends StringExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Factor
    def subexpressions: Iterable[Expression] =
      View(string, start) ++ end

    def evalRaw(using scope: Scope): Checked[Value] =
      for
        string <- string.eval.flatMap(_.asString)
        start <- start.eval.flatMap(_.asInt)
        end <- end.traverse(_.eval.flatMap(_.asInt))
        result <- catchExpected[RuntimeException]:
          StringValue(string.substring(start, end getOrElse string.length))
      yield
        result

    override def toString = s"substring($string, $start${end.fold("")(", " + _)})"


  /** Match returns the replacement of the whole (anchored) string, or fails. */
  final case class Match(string: Expression, pattern: Expression, replacement: Expression)
  extends StringExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Factor
    def subexpressions: Iterable[Expression] = View(string, pattern, replacement)

    private val precompiledPattern = precompilePattern(pattern)

    def evalRaw(using scope: Scope): Checked[Value] =
      for
        string <- string.eval.flatMap(_.asString)
        pattern <- precompiledPattern.getOrElse(compilePattern(pattern))
        replacement <- replacement.eval.flatMap(_.asString)
        result <- catchExpected[RuntimeException]:
          val matcher = pattern.matcher(string)
          if !matcher.matches() then
            Left(Problem("Does not match"))
          else
            Right(StringValue(matcher.replaceFirst(replacement)))
        result <- result
      yield
        result

    override def toString = s"match($string, $pattern, $replacement)"


  private def precompilePattern(pattern: Expression): Option[Checked[Pattern]] =
    pattern.isPure ? locally:
      compilePattern(pattern)(using Scope.empty)
        .tap:
          _.swap.foreach: problem =>
            logger.warn(s"Expression will fail with: $problem")

  private def compilePattern(pattern: Expression)(using Scope): Checked[Pattern] =
    pattern.eval.flatMap(_.asString).flatMap: pattern =>
      try
        Right(Pattern.compile(pattern))
      catch case t: PatternSyntaxException =>
        val i = t.getIndex
        Left(Problem(s"${t.getDescription} in regular expression pattern: “${
          pattern.take(i) + "❓" + pattern.drop(i)}”"))


  final case class Min(a: Expression, b: Expression)
  extends StringExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Factor
    def subexpressions: Iterable[Expression] = View(a, b)

    def evalRaw(using scope: Scope): Checked[Value] =
      for
        a <- a.eval.flatMap(_.asNumber)
        b <- b.eval.flatMap(_.asNumber)
      yield
        NumberValue(a min b)

    override def toString = s"min($a, $b)"


  final case class Max(a: Expression, b: Expression)
  extends StringExpr, IsPureIfSubexpressionsArePure:
    def precedence: Int = Precedence.Factor
    def subexpressions: Iterable[Expression] = View(a, b)

    def evalRaw(using scope: Scope): Checked[Value] =
      for
        a <- a.eval.flatMap(_.asNumber)
        b <- b.eval.flatMap(_.asNumber)
      yield
        NumberValue(a max b)

    override def toString = s"max($a, $b)"


  final case class ErrorExpr(errorMessage: Expression)
  extends IsPureIfSubexpressionsArePure/*not Constant, because it is an error?*/ :
    def precedence: Int = Precedence.Factor
    def subexpressions: Iterable[Expression] = Nil

    def evalRaw(using scope: Scope): Checked[Value] =
      errorMessage.eval
        .flatMap(expr => Left(ErrorInExpressionProblem(expr.convertToString)))

    override def toString = s"error($errorMessage)"


  type MissingConstant = MissingConstant.type
  case object MissingConstant extends Constant:
    val toValue: Value = MissingValue

    override val toString = "missing"


  final class ImpureTest(eval: () => Checked[Value]) extends IsImpure:
    def precedence: Int = Precedence.Highest
    def subexpressions: Iterable[Expression] = Nil
    protected def evalRaw(using scope: Scope) = eval()

  private def unexpectedType(t: ValueType, v: Value): Checked[Value] =
    v match
      case MissingValue => Right(MissingValue)
      case v => Left(UnexpectedValueTypeProblem(t, v))

  object convenience:
    implicit def convenientBooleanConstant(b: Boolean): BooleanConstant =
      BooleanConstant(b)

    implicit def convenientNumericConstant(n: Int): NumericConstant =
      NumericConstant(n)

    implicit def convenientNumericConstant(n: Long): NumericConstant =
      NumericConstant(n)

    implicit def convenientNumericConstant(n: BigDecimal): NumericConstant =
      NumericConstant(n)

    implicit def convenientStringConstant(string: String): StringConstant =
      StringConstant(string)

    implicit def convenientListConstant(list: List[Expression]): ListExpr =
      ListExpr(list)
