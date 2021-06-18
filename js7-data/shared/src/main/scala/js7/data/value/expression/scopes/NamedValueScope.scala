package js7.data.value.expression.scopes

import js7.data.value.Value
import js7.data.value.expression.ValueSearch.{LastOccurred, Name}
import js7.data.value.expression.{Scope, ValueSearch}

final class NamedValueScope(nameToValue: PartialFunction[String, Value])
extends Scope
{
  private lazy val nameToMaybeValue = nameToValue.lift

  override def findValue(search: ValueSearch)(implicit scope: Scope) =
    search match {
      case ValueSearch(LastOccurred, Name(name)) =>
        Right(nameToMaybeValue(name))

      case _ =>
        super.findValue(search)
    }

  override def toString = "NamedValueScope"
}

object NamedValueScope
{
  def apply(nameToValue: PartialFunction[String, Value]): Scope =
    new NamedValueScope(nameToValue)
}
