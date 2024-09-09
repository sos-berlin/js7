package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.data.value.Value
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.NamedValueScope.namesToString
import scala.collection.MapView

final class NamedValueScope(
  override val nameToCheckedValue: MapView[String, Checked[Value]])
extends Scope:

  override def toString = s"NamedValueScope(${namesToString(nameToCheckedValue)})"


object NamedValueScope:

  def apply(namedValue: (String, Value)*): Scope =
    apply(namedValue.toMap)

  def apply(nameToValue: Map[String, Value]): Scope =
    apply(nameToValue.view)

  def apply(nameToValue: MapView[String, Value]): Scope =
    new NamedValueScope(nameToValue.view.mapValues(Right(_)))

  def checkedValues(namedValue: (String, Checked[Value])*): Scope =
    new NamedValueScope(namedValue.toMap.view)

  private[scopes] def namesToString[V](nameToV: MapView[String, V]) =
    nameToV.keys.toVector.sorted.mkString(", ")
