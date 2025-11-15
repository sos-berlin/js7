package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.data.value.Value
import js7.data.value.expression.Scope

object ArgumentlessFunctionScope:

  def apply(nameToValue: PartialFunction[String, Checked[Value]]): Scope =
    val nameToValue_ = nameToValue
    new Scope:
      override def symbolToValue = nameToValue_
      override def toString = "ArgumentlessFunctionScope"

  def simpleJava(nameToValue: PartialFunction[String, Value.SimpleJava]): Scope =
    apply:
      Function.unlift: (k: String) =>
        nameToValue.lift(k).map: v =>
          Value.ofSimpleJava(v)
