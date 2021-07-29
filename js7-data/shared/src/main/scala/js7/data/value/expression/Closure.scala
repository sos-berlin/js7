package js7.data.value.expression

import js7.base.problem.Checked
import js7.data.value.{ListValue, Value}

// NOT USED
private final case class Closure(function: ExprFunction, scope: Scope)
{
  def eval(arguments: ListValue): Checked[Value] =
    function.eval(arguments)(scope)
}
