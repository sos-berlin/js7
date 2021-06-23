package js7.data.workflow

import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.data.value.{Value, ValueType}

final case class WorkflowParameter private(
  name: String,
  valueType: ValueType,
  default: Option[Value] = None)

object WorkflowParameter
{
  def apply(name: String, valueType: ValueType, default: Option[Value] = None): WorkflowParameter =
    checked(name, valueType, default).orThrow

  def apply(name: String, default: Value): WorkflowParameter =
    checked(name, default.valueType, Some(default)).orThrow

  def checked(name: String, valueType: ValueType, default: Option[Value] = None)
  : Checked[WorkflowParameter] =
    if (!default.forall(_.valueType == valueType))
      Left(Problem(
        s"Parameter '$name': type of default value does not match parameter type '$valueType'"))
    else
      Right(new WorkflowParameter(name, valueType, default))
}
