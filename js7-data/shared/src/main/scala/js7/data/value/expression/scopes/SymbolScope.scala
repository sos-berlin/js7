package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.data.value.Value
import js7.data.value.expression.Scope

final class SymbolScope(
  override val symbolToValue: PartialFunction[String, Checked[Value]])
extends Scope:

  override def toString = "SymbolScope"


object SymbolScope:
  def apply(nameToValue: PartialFunction[String, Checked[Value]]): Scope =
    new SymbolScope(nameToValue)
