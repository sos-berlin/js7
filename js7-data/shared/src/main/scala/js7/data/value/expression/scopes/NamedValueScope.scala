package js7.data.value.expression.scopes

import js7.data.value.Value
import js7.data.value.expression.ValueSearch.{LastOccurred, Name}
import js7.data.value.expression.{Scope, ValueSearch}

final class NamedValueScope(nameToValue: Map[String, Value]) extends Scope
{
  override def findValue(search: ValueSearch) =
    Right(search match {
      case ValueSearch(LastOccurred, Name(name)) =>
        nameToValue.get(name)

      case _ => None
    })
}
