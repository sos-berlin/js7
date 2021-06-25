package js7.data.workflow

import js7.data.job.JobResourcePath
import js7.data.value.expression.Expression
import js7.data.value.{Value, ValueType}

sealed trait WorkflowParameter {
  val name: String
  def referencedJobResourcePaths: Iterable[JobResourcePath]
}

object WorkflowParameter
{
  sealed trait HasType extends WorkflowParameter {
    def valueType: ValueType
  }

  final case class Required(name: String, valueType: ValueType)
  extends HasType {
    def referencedJobResourcePaths = Nil
  }

  object HasValue {
    def unapply(p: WorkflowParameter) =
      PartialFunction.condOpt(p) {
        case WorkflowParameter.Optional(_, value) => value
        case WorkflowParameter.WorkflowDefined(_, expr: Expression.Constant) => expr.toValue
      }
  }

  final case class Optional(name: String, value: Value)
  extends HasType {
    def valueType = value.valueType
    def referencedJobResourcePaths = Nil
  }

  final case class WorkflowDefined(name: String, expression: Expression)
  extends WorkflowParameter {
    def referencedJobResourcePaths = expression.referencedJobResourcePaths
  }

  def apply(name: String, valueType: ValueType): WorkflowParameter =
    Required(name, valueType)

  def apply(name: String, default: Value): WorkflowParameter =
    Optional(name, default)
}
