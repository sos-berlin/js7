package js7.data.value.expression.scopes

import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.base.utils.Lazy
import js7.data.value.Value
import js7.data.value.expression.ValueSearch.{LastOccurred, Name}
import js7.data.value.expression.{Scope, ValueSearch}

final class NamedValueScope(nameToValue: PartialFunction[String, Checked[Value]])
extends Scope
{
  override def findValue(search: ValueSearch)(implicit scope: Scope) =
    search match {
      case ValueSearch(LastOccurred, Name(name)) =>
        nameToValue.lift(name).sequence

      case _ =>
        super.findValue(search)
    }

  override def toString = {
    val x = nameToValue match {
      case nameToValue: Map[String, Checked[Value]] =>
        nameToValue.keys.toVector.sorted.mkString(", ")

      case _ => "?"
    }
    s"NamedValueScope($x)"
  }
}

object NamedValueScope
{
  def apply(nameToValue: PartialFunction[String, Value]): Scope =
    new NamedValueScope(nameToValue.andThen(Right(_)))

  def fromChecked(nameToValue: PartialFunction[String, Checked[Value]]): Scope =
    new NamedValueScope(nameToValue)

  def fromLazy(nameToValue: Map[String, Lazy[Checked[Value]]]): Scope =
    new NamedValueScope(nameToValue.andThen(_.apply()))
}
