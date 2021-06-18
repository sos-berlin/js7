package js7.data.value.expression.scopes

import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.data.value.Value
import js7.data.value.expression.ValueSearch.{LastOccurred, Name}
import js7.data.value.expression.{Scope, ValueSearch}
import scala.collection.MapView

final class LazyNamedValueScope(nameToValue: MapView[String, Checked[Value]])
extends Scope
{
  override def findValue(search: ValueSearch)(implicit scope: Scope) =
    search match {
      case ValueSearch(LastOccurred, Name(name)) =>
        nameToValue.get(name).sequence

      case _ =>
        super.findValue(search)
    }

  override def toString = s"LazyNamedValueScope(${nameToValue.keys.mkString(", ")})"
}

object LazyNamedValueScope
{
  def apply(nameToValue: MapView[String, Checked[Value]]): Scope =
    new LazyNamedValueScope(nameToValue)
}
