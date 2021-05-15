package js7.data.value.expression

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json, JsonObject}
import java.lang.Character.{isUnicodeIdentifierPart, isUnicodeIdentifierStart}
import js7.base.circeutils.CirceUtils.CirceUtilsChecked
import js7.base.utils.ScalaUtils.withStringBuilder
import js7.base.utils.typeclasses.IsEmpty
import js7.data.parser.BasicPrinter.{appendIdentifier, appendIdentifierWithBackticks, isIdentifierPart}
import js7.data.value.ValuePrinter.{appendQuotedContent, quoteString}
import js7.data.workflow.Label
import js7.data.workflow.instructions.executable.WorkflowJob
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
sealed trait Expression extends Expression.Precedence

object Expression
{
  implicit val jsonEncoder: Encoder[Expression] = expr => Json.fromString(expr.toString)
  implicit val jsonDecoder: Decoder[Expression] =
    c => c.as[String]
      .flatMap(ExpressionParser.parse(_).toDecoderResult(c.history))

  sealed trait SimpleValueExpression extends Expression

  sealed trait BooleanExpression extends SimpleValueExpression

  sealed trait NumericExpression extends SimpleValueExpression

  sealed trait StringExpression extends SimpleValueExpression

  final case class Not(a: BooleanExpression) extends BooleanExpression {
    def precedence = Precedence.Factor
    override def toString = "!" + Precedence.inParentheses(a, precedence)
  }

  final case class And(a: BooleanExpression, b: BooleanExpression) extends BooleanExpression {
    def precedence = Precedence.And
    override def toString = toString(a, "&&", b)
  }

  final case class Or(a: BooleanExpression, b: BooleanExpression) extends BooleanExpression {
    def precedence = Precedence.Or
    override def toString = toString(a, "||", b)
  }

  final case class Equal(a: Expression, b: Expression) extends BooleanExpression {
    def precedence = Precedence.Comparison
    override def toString = toString(a, "==", b)
  }

  final case class NotEqual(a: Expression, b: Expression) extends BooleanExpression {
    def precedence = Precedence.Comparison
    override def toString = toString(a, "!=", b)
  }

  final case class LessOrEqual(a: Expression, b: Expression) extends BooleanExpression {
    def precedence = Precedence.Comparison
    override def toString = toString(a, "<=", b)
  }

  final case class GreaterOrEqual(a: Expression, b: Expression) extends BooleanExpression {
    def precedence = Precedence.Comparison
    override def toString = toString(a, ">=", b)
  }

  final case class LessThan(a: Expression, b: Expression) extends BooleanExpression {
    def precedence = Precedence.Comparison
    override def toString = toString(a, "<", b)
  }

  final case class GreaterThan(a: Expression, b: Expression) extends BooleanExpression {
    def precedence = Precedence.Comparison
    override def toString = toString(a, ">", b)
  }

  final case class Concat(a: Expression, b: Expression) extends BooleanExpression {
    def precedence = Precedence.Addition
    override def toString = toString(a, "++", b)
  }

  final case class Add(a: Expression, b: Expression) extends BooleanExpression {
    def precedence = Precedence.Addition
    override def toString = toString(a, "+", b)
  }

  final case class Substract(a: Expression, b: Expression) extends BooleanExpression {
    def precedence = Precedence.Addition
    override def toString = toString(a, "-", b)
  }

  final case class In(a: Expression, b: ListExpression) extends BooleanExpression {
    def precedence = Precedence.WordOperator
    override def toString = toString(a, "in", b)
  }

  final case class Matches(a: Expression, b: Expression) extends BooleanExpression {
    def precedence = Precedence.WordOperator
    override def toString = toString(a, "matches", b)
  }

  final case class ListExpression(expressions: List[Expression]) extends Expression {
    def precedence = Precedence.Factor
    override def toString = expressions.mkString("[", ", ", "]")
  }

  // TODO Rename this. Das ist normaler Ausdruck, sondern eine Sammlung von Ausdrücken
  // Brauchen wir dafür eine Klasse?
  final case class ObjectExpression(nameToExpr: Map[String, Expression]) extends Expression {
    def isEmpty = nameToExpr.isEmpty
    def nonEmpty = nameToExpr.nonEmpty
    def precedence = Precedence.Factor
    override def toString = nameToExpr
      .map { case (k, v) => quoteString(k) + ":" + v }
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

  final case class ToNumber(expression: Expression) extends NumericExpression {
    def precedence = Precedence.Factor
    override def toString = Precedence.inParentheses(expression, precedence) + ".toNumber"
  }

  final case class ToBoolean(expression: Expression) extends BooleanExpression {
    def precedence = Precedence.Factor
    override def toString = Precedence.inParentheses(expression, precedence) + ".toBoolean"
  }

  final case class BooleanConstant(booleanValue: Boolean) extends BooleanExpression {
    def precedence = Precedence.Factor
    override def toString = booleanValue.toString
  }

  final case class NumericConstant(number: BigDecimal) extends NumericExpression {
    def precedence = Precedence.Factor
    override def toString = number.toString
  }

  final case class StringConstant(string: String) extends StringExpression {
    def precedence = Precedence.Factor
    override def toString = StringConstant.quote(string)
  }
  object StringConstant {
    val empty = new StringConstant("")

    private val inhibitsSingleQuoteString: Set[Char] = (0 until 0x20).filter(_ != '\n')
      .appendedAll((0x7f until 0xa0)
      .appended('\''.toInt))
      .map(_.toChar).toSet

    def quote(string: String): String =
      if (string.isEmpty)
        "\"\""
      else if (!string.exists(StringConstant.inhibitsSingleQuoteString))
        s"'$string'"
      else {
        val sb = new StringBuilder(64)
        sb.append('"')
        string foreach {
          case '\\' => sb.append("\\\\")
          case '\"' => sb.append("\\\"")
          case '\t' => sb.append("\\t")
          case '\r' => sb.append("\\r")
          case '\n' => sb.append("\\n")
          case '$' => sb.append("\\$")
          case c => sb.append(c)
        }
        sb.append('"')
        sb.toString
      }
  }

  final case class NamedValue(where: NamedValue.Where, what: NamedValue.What, default: Option[Expression] = None)
  extends Expression {
    import NamedValue._
    def precedence = Precedence.Factor
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
          case NamedValue.LastOccurredByPrefix(prefix) =>
            args += "prefix=" + prefix   // Not used

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
    final case class LastOccurredByPrefix(string: String) extends Where
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
    override def toString = s"$name(${arguments.mkString(", ")})"
  }

  final case class Argument(expression: Expression, maybeName: Option[String] = None) {
    override def toString = maybeName.fold("")(_ + "=") + expression
  }

  val LastReturnCode: NamedValue = NamedValue.last("returnCode")

  final case object OrderCatchCount extends NumericExpression {
    def precedence = Precedence.Factor
    override def toString = "catchCount"
  }

  final case class StripMargin(expression: Expression) extends StringExpression {
    def precedence = Precedence.Factor
    override def toString = Precedence.inParentheses(expression, precedence) + ".stripMargin"
  }

  final case class MkString(expression: Expression) extends StringExpression {
    def precedence = Precedence.Factor
    override def toString = Precedence.inParentheses(expression, precedence) + ".mkString"
  }

  /** Like MkString, but with a different toString representation. */
  final case class InterpolatedString(expressions: List[Expression]) extends StringExpression {
    def precedence = Precedence.Factor

    override def toString =
      withStringBuilder { sb =>
        sb.append('"')
        expressions.tails foreach {
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

  sealed trait Precedence {
    protected def precedence: Int

    protected def inParentheses(o: Precedence): String = Precedence.inParentheses(o, precedence)

    protected def toString(a: Expression, op: String, b: Expression) =
      Precedence.toString(a, op, precedence, b)
  }
  object Precedence {
    val WordOperator = 1
    val Or = 2
    val And = 3
    val Comparison = 4
    val Addition = 6
    val Factor = 99

    def toString(a: Expression, op: String, opPrecedence: Int, b: Expression): String =
      inParentheses(a, opPrecedence) + " " + op + " " + inParentheses(b, opPrecedence + 1)

    def inParentheses(o: Precedence, opPrecedence: Int): String =
      if (o.precedence >= opPrecedence)
        o.toString
      else
        s"($o)"
  }
}
