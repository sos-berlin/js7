package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.=>?
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.value.Value
import js7.data.value.expression.Scope

object ArgumentlessFunctionScope:

  def apply(nameToValue: String =>? Checked[Value]): Scope =
    val nameToValue_ = nameToValue
    new Scope:
      override def symbolValue(name: String) = nameToValue_.get(name)
      override def toString = "ArgumentlessFunctionScope"

  def simpleJava(nameToValue: String =>? Value.SimpleJava): Scope =
    apply:
      Function.unlift: (k: String) =>
        nameToValue.lift(k).map: v =>
          Value.ofSimpleJava(v)
