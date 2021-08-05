package js7.data.workflow

import js7.data.job.JobResourcePath
import js7.data.value.expression.Expression
import js7.data.value.{Value, ValueType}

sealed trait OrderParameter {
  val name: String
  def referencedJobResourcePaths: Iterable[JobResourcePath]
}

object OrderParameter
{
  def apply(name: String, valueType: ValueType): OrderParameter =
    Required(name, valueType)

  def apply(name: String, valueType: ValueType, expression: Expression): OrderParameter =
    Optional(name, valueType, expression)

  def apply(name: String, expression: Expression.Constant): OrderParameter =
    Optional(name, expression.toValue.valueType, expression)

  sealed trait HasType extends OrderParameter {
    def valueType: ValueType
  }

  /** A required order argument. */
  final case class Required(name: String, valueType: ValueType)
  extends HasType {
    def referencedJobResourcePaths = Nil
  }

  object HasValue {
    def unapply(p: OrderParameter): Option[Value] =
      PartialFunction.condOpt(p) {
        case OrderParameter.Optional(_, _, expr: Expression.Constant) => expr.toValue
        case OrderParameter.Final(_, expr: Expression.Constant) => expr.toValue
      }
  }

  sealed trait HasExpression extends OrderParameter {
    def expression: Expression
  }
  object HasExpression {
    def unapply(p: OrderParameter): Option[Expression] =
      PartialFunction.condOpt(p) {
        case OrderParameter.Optional(_, _, expr) => expr
        case OrderParameter.Final(_, expr) => expr
      }
  }

  /** An optional value may be overridden by an order argument. */
  final case class Optional(name: String, valueType: ValueType, expression: Expression)
  extends HasType with HasExpression {
    def referencedJobResourcePaths = expression.referencedJobResourcePaths
  }

  /** A final value may not be overriden. */
  final case class Final(name: String, expression: Expression)
  extends HasExpression {
    def referencedJobResourcePaths = expression.referencedJobResourcePaths
  }
}
