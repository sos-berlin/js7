package js7.data.value.expression.scopes

import js7.data.value.Value
import js7.data.value.expression.Scope

final class SymbolScope(nameToSymbolValue: PartialFunction[String, Value])
extends Scope
{
  override def symbolToValue(symbol: String) =
    nameToSymbolValue.lift(symbol)
      .map(Right(_))
      .orElse(super.symbolToValue(symbol))

  override def toString = "SymbolScope"
}

object SymbolScope
{
  def apply(nameToValue: PartialFunction[String, Value]): Scope =
    new SymbolScope(nameToValue)
}
