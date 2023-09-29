package js7.data.value.expression.scopes

import js7.data.value.Value
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.NamedValueScope.namesToString
import scala.collection.MapView

final class NamedValueScope(nameToValue: MapView[String, Value])
extends Scope:
  override def nameToCheckedValue =
    nameToValue.mapValues(Right(_))

  override def toString = s"NamedValueScope(${namesToString(nameToCheckedValue)})"

object NamedValueScope:
  def apply(namedValue: (String, Value)*): Scope =
    apply(namedValue.toMap)

  def apply(nameToValue: Map[String, Value]): Scope =
    apply(nameToValue.view)

  def apply(nameToValue: MapView[String, Value]): Scope =
    new NamedValueScope(nameToValue)

  private[scopes] def namesToString[V](nameToV: MapView[String, V]) =
    nameToV.keys.toVector.sorted.mkString(", ")
