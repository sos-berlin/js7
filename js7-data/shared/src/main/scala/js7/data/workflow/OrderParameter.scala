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
  sealed trait HasType extends OrderParameter {
    def valueType: ValueType
  }

  final case class Required(name: String, valueType: ValueType)
  extends HasType {
    def referencedJobResourcePaths = Nil
  }

  object HasValue {
    def unapply(p: OrderParameter) =
      PartialFunction.condOpt(p) {
        case OrderParameter.Optional(_, value) => value
        case OrderParameter.WorkflowDefined(_, expr: Expression.Constant) => expr.toValue
      }
  }

  final case class Optional(name: String, value: Value)
  extends HasType {
    def valueType = value.valueType
    def referencedJobResourcePaths = Nil
  }

  final case class WorkflowDefined(name: String, expression: Expression)
  extends OrderParameter {
    def referencedJobResourcePaths = expression.referencedJobResourcePaths
  }

  def apply(name: String, valueType: ValueType): OrderParameter =
    Required(name, valueType)

  def apply(name: String, default: Value): OrderParameter =
    Optional(name, default)
}
