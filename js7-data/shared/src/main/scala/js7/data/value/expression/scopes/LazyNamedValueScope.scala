package js7.data.value.expression.scopes

import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.base.utils.Lazy
import js7.data.value.Value
import js7.data.value.expression.ValueSearch.{LastOccurred, Name}
import js7.data.value.expression.{Scope, ValueSearch}

final class LazyNamedValueScope(nameToValue: Map[String, Lazy[Checked[Value]]]) extends Scope
{
  override def findValue(search: ValueSearch) =
    search match {
      case ValueSearch(LastOccurred, Name(name)) =>
        nameToValue.get(name).traverse(_())

      case _ => Right(None)
    }
}
