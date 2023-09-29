package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.data.value.Value
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.NamedValueScope.namesToString
import scala.collection.MapView

final class NameToCheckedValueScope(
  override val nameToCheckedValue: MapView[String, Checked[Value]])
extends Scope:
  override def toString = s"NameToCheckedValueScope(${namesToString(nameToCheckedValue)})"

object NameToCheckedValueScope:
  def apply(nameToValue: MapView[String, Checked[Value]]): Scope =
    new NameToCheckedValueScope(nameToValue)
