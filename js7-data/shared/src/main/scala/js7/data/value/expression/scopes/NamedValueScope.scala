package js7.data.value.expression.scopes

import js7.data.value.expression.ValueSearch.{LastOccurred, Name}
import js7.data.value.expression.{Scope, ValueSearch}
import js7.data.value.{NamedValues, Value}

final class NamedValueScope(nameToValue: Map[String, Value]) extends Scope
{
  override def findValue(search: ValueSearch) =
    Right(search match {
      case ValueSearch(LastOccurred, Name(name)) =>
        nameToValue.get(name)

      case _ => None
    })
}

object NamedValueScope
{
  def apply(namedValues: NamedValues): Scope =
    new NamedValueScope(namedValues)
}
