package js7.data.value.expression

import fastparse.NoWhitespace._
import io.circe.{Decoder, Encoder, Json}
import java.lang.Character.{isUnicodeIdentifierPart, isUnicodeIdentifierStart}
import js7.base.circeutils.CirceUtils.CirceUtilsChecked
import js7.base.utils.Identifier.isIdentifier
import js7.data.parser.Parsers.checkedParse
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
    c => c.as[String].flatMap(string => checkedParse(string, ExpressionParser.expression(_)).toDecoderResult(c.history))

  sealed trait BooleanExpression extends Expression

  sealed trait NumericExpression extends Expression

  sealed trait StringExpression extends Expression

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
    override def toString =
      if (string.isEmpty) "\"\""
      else if (string contains '\'')
        s""""$string""""
      else
        s"'$string'"
  }

  final case class NamedValue(where: NamedValue.Where, what: NamedValue.What, default: Option[Expression] = None)
  extends Expression {
    import NamedValue._
    def precedence = Precedence.Factor
    override def toString = (where, what, default) match {
      case (LastOccurred, KeyValue(StringConstant(key)), None) if isSimpleName(key) => "$" + key
      case (LastOccurred, KeyValue(StringConstant(key)), None) if isIdentifier(key) => s"$${$key}"
      case (LastOccurred, KeyValue(expression), None) => s"variable($expression)"
      //case (Argument, NamedValue(StringConstant(key)), None) if isIdentifier(key) => s"$${arg::$key}"
      //case (LastOccurredByPrefix(prefix), NamedValue(StringConstant(key)), None) if isIdentifier(key) => s"$${$prefix.$key}"
      //case (ByLabel(Label(label)), NamedValue(StringConstant(key)), None) if isIdentifier(key) => s"$${label::$label.$key}"
      //case (LastExecutedJob(WorkflowJob.Name(jobName)), NamedValue(StringConstant(key)), None) if isIdentifier(key) => s"$${job::$jobName.$key}"
      case _ =>
        val args = mutable.Buffer[String]()
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
            args += "job=" + jobName.string
          case NamedValue.ByLabel(label) =>
            args += "label=" + label.string
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
    def last(key: String) = NamedValue(NamedValue.LastOccurred, NamedValue.KeyValue(key))
    def last(key: String, default: Expression) = NamedValue(NamedValue.LastOccurred, NamedValue.KeyValue(key), Some(default))
    def argument(key: String) = NamedValue(NamedValue.Argument, NamedValue.KeyValue(key))

    def isSimpleName(name: String) = isSimpleNameStart(name.head) && name.tail.forall(isSimpleNamePart)
    def isSimpleNameStart(c: Char) = isUnicodeIdentifierStart(c)
    def isSimpleNamePart(c: Char) = isUnicodeIdentifierPart(c)

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
