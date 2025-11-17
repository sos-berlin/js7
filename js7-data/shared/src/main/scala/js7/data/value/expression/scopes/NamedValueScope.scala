package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.value.Value
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.NamedValueScope.namesToString
import scala.collection.MapView

final class NamedValueScope(
  nameToCheckedValue: PartialFunction[String, Checked[Value]])
extends Scope:

  override def namedValue(name: String): Option[Checked[Value]] =
    nameToCheckedValue.get(name)

  override def toString = s"NamedValueScope(${namesToString(nameToCheckedValue)})"


object NamedValueScope:

  def apply(namedValue: (String, Value)*): Scope =
    apply(namedValue.map((k, v) => k -> Right(v)).toMap)

  /**
   * @param nameToValue Value is expected to be constant.
   */
  def simple(nameToValue: PartialFunction[String, Value]): Scope =
    apply:
      Function.unlift: (k: String) =>
        nameToValue.lift(k).map(Right(_))

  /**
   * @param nameToValue Value is expected to be constant.
   */
  def simpleJava(nameToValue: PartialFunction[String, Value.SimpleJava]): Scope =
    apply:
      Function.unlift: (k: String) =>
        nameToValue.lift(k).map:
          Value.ofSimpleJava

  /**
   * @param nameToValue Value is expected to be constant.
   */
  def apply(nameToValue: Map[String, Value]): Scope =
    apply(nameToValue.view)

  /**
   * @param nameToValue Value is expected to be constant.
   */
  def apply(nameToValue: MapView[String, Value]): Scope =
    apply:
      nameToValue.mapValues(Right(_))

  /**
   * @param nameToChecked: Checked[Value] is expected to be constant.
   */
  def apply(nameToChecked: PartialFunction[String, Checked[Value]]): Scope =
    new NamedValueScope(nameToChecked)

  private[scopes] def namesToString[V](nameToV: PartialFunction[String, V]) =
    nameToV match
      case o: MapView[String, ?] @unchecked => o.keys.toVector.sorted.mkString(", ")
      case o: Map[String, ?] @unchecked => o.keys.toVector.sorted.mkString(", ")
      case _ => nameToV.toString
