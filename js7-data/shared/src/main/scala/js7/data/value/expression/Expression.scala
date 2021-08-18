package js7.data.value.expression

import cats.syntax.traverse._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json, JsonObject}
import java.lang.Character.{isUnicodeIdentifierPart, isUnicodeIdentifierStart}
import java.util.regex.Pattern
import js7.base.circeutils.CirceUtils.CirceUtilsChecked
import js7.base.problem.Checked.CheckedOption
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichOption
import js7.base.utils.ScalaUtils.withStringBuilder
import js7.base.utils.typeclasses.IsEmpty
import js7.data.job.JobResourcePath
import js7.data.parser.BasicPrinter.{appendIdentifier, appendIdentifierWithBackticks, identifierToString, isIdentifierPart}
import js7.data.value.ValuePrinter.appendQuotedContent
import js7.data.value.ValueType.MissingValueProblem
import js7.data.value.{BooleanValue, IsErrorValue, ListValue, MissingValue, NullValue, NumberValue, ObjectValue, StringValue, Value, ValuePrinter}
import js7.data.workflow.Label
import js7.data.workflow.instructions.executable.WorkflowJob
import scala.collection.{View, mutable}

/**
  * @author Joacim Zschimmer
  */
sealed trait Expression extends Expression.Precedence
{
  def subexpressions: Iterable[Expression]

  // Experimental, not used
  protected def isPure: Boolean

  final def isConstant = this.isInstanceOf[Expression.Constant]

  def referencedJobResourcePaths: Iterable[JobResourcePath] =
    subexpressions.view
      .flatMap(_.referencedJobResourcePaths)
      .toSet

  final def eval(implicit scope: Scope): Checked[Value] =
    evalAllowError.flatMap {
      case v: IsErrorValue => v.problem
      case o => Right(o)
    }

  protected def evalAllowError(implicit scope: Scope): Checked[Value]

  final def evalAsBoolean(implicit scope: Scope): Checked[Boolean] =
    eval.flatMap(_.asBooleanValue).map(_.booleanValue)

  final def evalAsNumber(implicit scope: Scope): Checked[BigDecimal] =
    eval.flatMap(_.asNumberValue).map(_.number)

  final def evalToString(implicit scope: Scope): Checked[String] =
    eval.flatMap(_.toStringValue).map(_.string)

  final def evalAsString(implicit scope: Scope): Checked[String] =
    eval.flatMap(_.asStringValue).map(_.string)

  final def evalAsVector(implicit scope: Scope): Checked[Vector[Value]] =
    eval.flatMap(_.asListValue).map(_.elements)
}

object Expression
{
  implicit val jsonEncoder: Encoder[Expression] = expr => Json.fromString(expr.toString)
  implicit val jsonDecoder: Decoder[Expression] =
    c => c.as[String]
      .flatMap(ExpressionParser.parse(_).toDecoderResult(c.history))

  // Experimental
  sealed trait PurityDependsOnSubexpressions extends Expression {
    def isPure = subexpressions.forall(_.isPure)
  }

  // Experimental
  sealed trait Pure extends Expression {
    def isPure = true
  }

  sealed trait Constant extends Pure {
    final def precedence = Precedence.Factor
    final def subexpressions = Nil
    def toValue: Value

    final def evalAllowError(implicit scope: Scope) =
      Right(toValue)
  }

  sealed trait SimpleValueExpression extends Expression

  sealed trait BooleanExpression extends SimpleValueExpression

  sealed trait NumericExpression extends SimpleValueExpression

  sealed trait StringExpression extends SimpleValueExpression

  final case class Not(a: BooleanExpression)
  extends BooleanExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.Factor
    def subexpressions = a.subexpressions

    protected def evalAllowError(implicit scope: Scope) =
      for (a <- a.evalAsBoolean) yield
        BooleanValue(!a.booleanValue)

    override def toString = "!" + Precedence.inParentheses(a, precedence)
  }

  final case class And(a: BooleanExpression, b: BooleanExpression)
  extends BooleanExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.And
    def subexpressions = View(a, b)

    protected def evalAllowError(implicit scope: Scope) =
      a.evalAsBoolean.flatMap {
        case false => Right(BooleanValue.False)
        case true => b.evalAsBoolean.map(b => BooleanValue(b.booleanValue))
      }

    override def toString = toString(a, "&&", b)
  }

  final case class Or(a: BooleanExpression, b: BooleanExpression)
  extends BooleanExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.Or
    def subexpressions = View(a, b)

    protected def evalAllowError(implicit scope: Scope) =
      a.evalAsBoolean.flatMap {
        case false => b.evalAsBoolean.map(b => BooleanValue(b.booleanValue))
        case true => Right(BooleanValue.True)
      }

    override def toString = toString(a, "||", b)
  }

  final case class Equal(a: Expression, b: Expression)
  extends BooleanExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.Comparison
    def subexpressions = View(a, b)

    protected def evalAllowError(implicit scope: Scope) =
      for (a <- a.eval; b <- b.eval) yield BooleanValue(a == b)

    override def toString = toString(a, "==", b)
  }

  final case class NotEqual(a: Expression, b: Expression)
  extends BooleanExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.Comparison
    def subexpressions = View(a, b)

    protected def evalAllowError(implicit scope: Scope) =
      for (a <- a.eval; b <- b.eval) yield BooleanValue(a != b)

    override def toString = toString(a, "!=", b)
  }

  final case class LessOrEqual(a: Expression, b: Expression)
  extends BooleanExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.Comparison
    def subexpressions = View(a, b)

    protected def evalAllowError(implicit scope: Scope) =
      for (a <- a.evalAsNumber; b <- b.evalAsNumber) yield
        BooleanValue(a <= b)

    override def toString = toString(a, "<=", b)
  }

  final case class GreaterOrEqual(a: Expression, b: Expression)
  extends BooleanExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.Comparison
    def subexpressions = View(a, b)

    protected def evalAllowError(implicit scope: Scope) =
      for (a <- a.evalAsNumber; b <- b.evalAsNumber) yield BooleanValue(a >= b)

    override def toString = toString(a, ">=", b)
  }

  final case class LessThan(a: Expression, b: Expression)
  extends BooleanExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.Comparison
    def subexpressions = View(a, b)

    protected def evalAllowError(implicit scope: Scope) =
      for (a <- a.evalAsNumber; b <- b.evalAsNumber) yield BooleanValue(a < b)

    override def toString = toString(a, "<", b)
  }

  final case class GreaterThan(a: Expression, b: Expression)
  extends BooleanExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.Comparison
    def subexpressions = View(a, b)

    protected def evalAllowError(implicit scope: Scope) =
      for (a <- a.evalAsNumber; b <- b.evalAsNumber) yield BooleanValue(a > b)

    override def toString = toString(a, ">", b)
  }

  final case class Concat(a: Expression, b: Expression)
  extends BooleanExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.Addition
    def subexpressions = View(a, b)

    protected def evalAllowError(implicit scope: Scope) =
      for  {
        a <- a.eval
        b <- b.eval
        result <- (a, b) match {
          case (a: ListValue, b: ListValue) =>
            Right(ListValue(a.elements ++ b.elements))
          case (a, b) =>
            for (a <- a.toStringValue; b <- b.toStringValue) yield StringValue(a.string + b.string)
        }
      } yield result

    override def toString = toString(a, "++", b)
  }

  sealed trait NumericBinaryOperation
  extends NumericExpression with PurityDependsOnSubexpressions {
    final def subexpressions = View(a, b)

    protected final def evalAllowError(implicit scope: Scope) =
      for {
        a <- a.evalAsNumber
        b <- b.evalAsNumber
      } yield Value.catchNonFatal(NumberValue(op(a, b)))

    protected val a: Expression
    protected val b: Expression
    protected def op(a: BigDecimal, b: BigDecimal): BigDecimal
  }

  final case class Multiply(a: Expression, b: Expression)
  extends NumericBinaryOperation {
    def precedence = Precedence.Multiplication

    protected def op(a: BigDecimal, b: BigDecimal) = a * b

    override def toString = toString(a, "*", b)
  }

  final case class Divide(a: Expression, b: Expression)
  extends NumericBinaryOperation {
    def precedence = Precedence.Multiplication

    protected def op(a: BigDecimal, b: BigDecimal) = a / b

    override def toString = toString(a, "/", b)
  }

  final case class Add(a: Expression, b: Expression)
  extends NumericBinaryOperation {
    def precedence = Precedence.Addition

    protected def op(a: BigDecimal, b: BigDecimal) = a + b

    override def toString = toString(a, "+", b)
  }

  final case class Substract(a: Expression, b: Expression)
  extends NumericBinaryOperation {
    def precedence = Precedence.Addition

    protected def op(a: BigDecimal, b: BigDecimal) = a - b

    override def toString = toString(a, "-", b)
  }

  final case class In(a: Expression, b: ListExpression)
  extends BooleanExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.WordOperator
    def subexpressions = View(a, b)

    protected def evalAllowError(implicit scope: Scope) =
      for (a <- a.eval; b <- b.evalAsVector) yield BooleanValue(b contains a)

    override def toString = toString(a, "in", b)
  }

  final case class Matches(a: Expression, b: Expression)
  extends BooleanExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.WordOperator
    def subexpressions = View(a, b)

    protected def evalAllowError(implicit scope: Scope) =
      for {
        a <- a.evalToString
        b <- b.evalToString
        result <- Checked.catchNonFatal(BooleanValue(a matches b))
      } yield result

    override def toString = toString(a, "matches", b)
  }

  final case class OrElse(a: Expression, default: Expression)
  extends BooleanExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.WordOperator
    def subexpressions = a :: default :: Nil

    protected def evalAllowError(implicit scope: Scope) =
      a.evalAllowError.flatMap {
        case _: IsErrorValue | NullValue => default.eval
        case o => Right(o)
      }

    override def toString = toString(a, "orElse", default)
  }

  final case class OrNull(a: Expression)
  extends BooleanExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.Factor
    def subexpressions = a :: Nil

    protected def evalAllowError(implicit scope: Scope) =
      a.evalAllowError.map {
        case _: IsErrorValue => NullValue
        case o => o
      }

    override def toString = Precedence.inParentheses(a, precedence) + "?"
  }

  final case class DotExpression(a: Expression, name: String) extends PurityDependsOnSubexpressions {
    def subexpressions = a :: Nil
    def precedence = Precedence.DotOperator

    protected def evalAllowError(implicit scope: Scope) =
      a.eval flatMap {
        case a: ObjectValue =>
          a.nameToValue.get(name) !! Problem(s"Unknown object '$name' field name in: $toString")

        case _ => Left(Problem(s"Object expected: $toString"))
      }

    override def toString =
      Precedence.inParentheses(a, precedence) + "." + identifierToString(name)
  }

  final case class ListExpression(subexpressions: List[Expression]) extends PurityDependsOnSubexpressions {
    def precedence = Precedence.Factor

    protected def evalAllowError(implicit scope: Scope) =
      subexpressions.traverse(_.eval).map(_.toVector).map(ListValue(_))

    override def toString = subexpressions.mkString("[", ", ", "]")
  }

  final case class ObjectExpression(nameToExpr: Map[String, Expression])
  extends PurityDependsOnSubexpressions {
    def subexpressions = nameToExpr.values
    def isEmpty = nameToExpr.isEmpty
    def nonEmpty = nameToExpr.nonEmpty
    def precedence = Precedence.Factor

    protected def evalAllowError(implicit scope: Scope) =
      nameToExpr.toVector
        .traverse { case (k, v) => v.eval.map(k -> _) }
        .map(pairs => ObjectValue(pairs.toMap))

    override def toString = nameToExpr
      .map { case (k, v) => identifierToString(k) + ":" + v }
      .mkString("{", ", ", "}")
  }
  object ObjectExpression {
    val empty = ObjectExpression(Map.empty)
    implicit val objectExpressionIsEmpty = IsEmpty[ObjectExpression](_.isEmpty)
    implicit val jsonEncoder: Encoder.AsObject[ObjectExpression] =
      o => JsonObject.fromIterable(o.nameToExpr.view.mapValues(_.asJson).toSeq)

    implicit val jsonDecoder: Decoder[ObjectExpression] =
      _.as[Map[String, Expression]] map ObjectExpression.apply
  }

  final case class ToNumber(expression: Expression)
  extends NumericExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.Factor
    def subexpressions = expression :: Nil

    protected def evalAllowError(implicit scope: Scope) =
      expression.eval.flatMap(_.toNumberValue)

    override def toString = s"toNumber($expression)"
  }

  final case class ToBoolean(expression: Expression)
  extends BooleanExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.Factor
    def subexpressions = expression :: Nil

    protected def evalAllowError(implicit scope: Scope) =
      expression.eval.flatMap(_.toBooleanValue)

    override def toString = s"toBoolean($expression)"
  }

  final case class BooleanConstant(booleanValue: Boolean)
  extends BooleanExpression with Constant {
    val toValue = BooleanValue(booleanValue)

    override def toString = booleanValue.toString
  }

  final case class NumericConstant(number: BigDecimal)
  extends NumericExpression with Constant {
    val toValue = NumberValue(number)

    override def toString = number.toString
  }

  final case class StringConstant(string: String)
  extends StringExpression with Constant {
    val toValue = StringValue(string)

    override def toString = StringConstant.quote(string)
  }
  object StringConstant {
    val empty = new StringConstant("")

    def quote(string: String): String =
      ValuePrinter.quoteString(string)
  }

  final case class NamedValue(where: NamedValue.Where, what: NamedValue.What, default: Option[Expression] = None)
  extends Expression {
    import NamedValue._
    def precedence = Precedence.Factor
    def subexpressions = Nil
    def isPure = false

    protected def evalAllowError(implicit scope: Scope) = {
      val w = where match {
        case NamedValue.Argument => ValueSearch.Argument
        case NamedValue.LastOccurred => ValueSearch.LastOccurred
        case NamedValue.ByLabel(label) => ValueSearch.LastExecuted(PositionSearch.ByLabel(label))
        case NamedValue.LastExecutedJob(jobName) => ValueSearch.LastExecuted(PositionSearch.ByWorkflowJob(jobName))
      }
      what match {
        case NamedValue.KeyValue(stringExpr) =>
          for {
            name <- stringExpr.evalToString
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
          } yield value
      }
    }

    override def toString = (where, what, default) match {
      case (LastOccurred, KeyValue(StringConstant(key)), None) if !key.contains('`') =>
        if (isSimpleName(key) || key.forall(c => c >= '0' && c <= '9'))
          s"$$$key"
        else
          s"$$`$key`"

      case (LastOccurred, KeyValue(expression), None) => s"variable($expression)"

      //case (Argument, NamedValue(StringConstant(key)), None) if isIdentifier(key) => s"$${arg::$key}"
      //case (LastOccurredByPrefix(prefix), NamedValue(StringConstant(key)), None) if isIdentifier(key) => s"$${$prefix.$key}"
      //case (ByLabel(Label(label)), NamedValue(StringConstant(key)), None) if isIdentifier(key) => s"$${label::$label.$key}"
      //case (LastExecutedJob(WorkflowJob.Name(jobName)), NamedValue(StringConstant(key)), None) if isIdentifier(key) => s"$${job::$jobName.$key}"
      case _ =>
        val args = mutable.Buffer.empty[String]
        lazy val sb = new StringBuilder
        what match {
          case NamedValue.KeyValue(expr) => args += s"key=$expr"
          case _ =>
        }
        where match {
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
        }
        for (d <- default) args += "default=" + d.toString
        (where, what) match {
          case (NamedValue.Argument, NamedValue.KeyValue(_)) =>
            s"argument(${args mkString ", "})"

          case (_, NamedValue.KeyValue(_)) =>
            s"variable(${args mkString ", "})"
        }
    }
  }
  object NamedValue {
    def apply(name: String): NamedValue =
      last(name)

    def last(name: String) = NamedValue(NamedValue.LastOccurred, NamedValue.KeyValue(name))
    def last(name: String, default: Expression) = NamedValue(NamedValue.LastOccurred, NamedValue.KeyValue(name), Some(default))
    def argument(name: String) = NamedValue(NamedValue.Argument, NamedValue.KeyValue(name))

    private[Expression] def isSimpleName(name: String) = name.nonEmpty && isSimpleNameStart(name.head) && name.tail.forall(isSimpleNamePart)
    private[expression] def isSimpleNameStart(c: Char) = isUnicodeIdentifierStart(c)
    private[expression] def isSimpleNamePart(c: Char) = isUnicodeIdentifierPart(c)

    sealed trait Where
    case object LastOccurred extends Where
    final case class LastExecutedJob(jobName: WorkflowJob.Name) extends Where
    final case class ByLabel(label: Label) extends Where
    case object Argument extends Where

    sealed trait What
    final case class KeyValue(key: Expression) extends What
    object KeyValue {
      def apply(key: String) = new KeyValue(StringConstant(key))
    }
  }

  final case class FunctionCall(name: String, arguments: Seq[Argument] = Nil)
  extends Expression {
    protected def precedence = Precedence.Factor
    def subexpressions = Nil
    def isPure = false

    def evalAllowError(implicit scope: Scope) =
      scope.evalFunctionCall(this)
        .getOrElse(Left(Problem(s"Unknown function: $name")))

    override def toString = s"$name(${arguments.mkString(", ")})"
  }

  final case class Argument(expression: Expression, maybeName: Option[String] = None) {
    override def toString = maybeName.fold("")(_ + "=") + expression
  }

  final case class JobResourceVariable(jobResourcePath: JobResourcePath, name: Option[String])
  extends Expression {
    protected def precedence = Precedence.Factor

    def subexpressions = Nil

    override def isPure = false

    override def referencedJobResourcePaths = jobResourcePath :: Nil

    def evalAllowError(implicit scope: Scope) =
      scope.evalJobResourceVariable(this)
        .getOrElse(Left(Problem(s"JobResources are not accessible here: $toString")))

    override def toString = withStringBuilder(64) { sb =>
      sb.append("JobResource")
      sb.append(':')
      sb.append(jobResourcePath.string)
      for (nam <- name) {
        sb.append(':')
        appendIdentifier(sb, nam)
      }
    }
  }

  val LastReturnCode: NamedValue = NamedValue.last("returnCode")

  final case object OrderCatchCount extends NumericExpression {
    def precedence = Precedence.Factor
    def subexpressions = Nil
    override def isPure = false

    def evalAllowError(implicit scope: Scope) =
      scope.symbolToValue("catchCount")
        .getOrElse(Left(Problem(s"Unknown symbol: $OrderCatchCount")))
        .flatMap(_.toNumberValue)

    override def toString = "catchCount"
  }

  final case class StripMargin(a: Expression)
  extends StringExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.Factor
    def subexpressions = a :: Nil

    def evalAllowError(implicit scope: Scope) =
      a.evalAsString.map(string => StringValue(string.stripMargin))

    override def toString = s"stripMargin($a)"
  }

  final case class MkString(expression: Expression)
  extends StringExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.Factor
    def subexpressions = expression :: Nil

    def evalAllowError(implicit scope: Scope) =
      expression.eval flatMap  {
        case ListValue(list) =>
          list.traverse(_.toStringValue).map(o => StringValue(o.map(_.string).mkString))
        case value => value.toStringValue
      }

    override def toString = s"mkString($expression)"
  }

  /** Like MkString, but with a different toString representation. */
  final case class InterpolatedString(subexpressions: List[Expression])
  extends StringExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.Factor

    def evalAllowError(implicit scope: Scope) =
      subexpressions
        .traverse(_.eval.map(_.convertToString))
        .map(seq => StringValue(seq.mkString))

    override def toString =
      withStringBuilder { sb =>
        sb.append('"')
        subexpressions.tails foreach {
          case StringConstant(string) :: _ =>
            appendQuotedContent(sb, string)

          case NamedValue(NamedValue.LastOccurred, NamedValue.KeyValue(StringConstant(name)), None) :: next =>
            sb.append('$')
            next match {
              case StringConstant(next) :: Nil if next.headOption.forall(isIdentifierPart) =>
                appendIdentifierWithBackticks(sb, name)
              case _ =>
                appendIdentifier(sb, name)
            }

          case expr :: _ =>
            sb.append("$(")
            sb.append(expr)
            sb.append(')')

          case Nil =>
        }
        sb.append('"')
      }
  }

  final case class ReplaceAll(string: Expression, pattern: Expression, replacement: Expression)
  extends StringExpression with PurityDependsOnSubexpressions {
    def precedence = Precedence.Factor
    def subexpressions = View(string, pattern, replacement)

    def evalAllowError(implicit scope: Scope) =
      for {
        string <- string.eval.flatMap(_.asString)
        pattern <- pattern.eval.flatMap(_.asString)
        pattern <- Checked.catchNonFatal(Pattern.compile(pattern))
        replacement <- replacement.eval.flatMap(_.asString)
        result <- Checked.catchNonFatal(StringValue(pattern.matcher(string).replaceAll(replacement)))
      } yield result

    override def toString = s"replaceAll($string, $pattern, $replacement)"
  }

  /** `problem` must be defined only if internally generated. */
  final case class MissingConstant(problem: Problem = MissingValueProblem)
  extends Pure/*not Constant, because MissingValue is an error*/ {
    def precedence = Precedence.Factor
    def subexpressions = Nil

    def evalAllowError(implicit scope: Scope) =
      Right(MissingValue(problem))

    override def toString = "missing"  // We don't have a parsable syntax for problem !!!
  }

  type NullConstant = NullConstant.type
  case object NullConstant extends Constant {
    def toValue = NullValue

    override def toString = "null"
  }

  final class ImpureTest(eval: () => Checked[Value]) extends Expression {
    def isPure = false
    def precedence = Precedence.Highest
    def subexpressions = Nil
    protected def evalAllowError(implicit scope: Scope) = eval()
  }

  sealed trait Precedence {
    protected def precedence: Int

    protected def inParentheses(o: Precedence): String = Precedence.inParentheses(o, precedence)

    protected def toString(a: Expression, op: String, b: Expression) =
      Precedence.toString(a, op, precedence, b)
  }
  object Precedence {
    // Higher number means higher precedence
    private val next = Iterator.from(1).next _
    val DotOperator = next()
    val WordOperator = next()
    val Word1Operator = next()
    val Or = next()
    val And = next()
    val Comparison = next()
    val Addition = next()
    val Multiplication = next()
    val Factor = next()
    val Highest = next()

    def toString(a: Expression, op: String, opPrecedence: Int, b: Expression): String =
      inParentheses(a, opPrecedence) + " " + op + " " + inParentheses(b, opPrecedence + 1)

    def inParentheses(o: Precedence, opPrecedence: Int): String =
      if (o.precedence >= opPrecedence)
        o.toString
      else
        s"($o)"
  }
}
