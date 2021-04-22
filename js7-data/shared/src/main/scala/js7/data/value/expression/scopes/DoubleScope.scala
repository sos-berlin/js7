package js7.data.value.expression.scopes

import js7.data.value.Value
import js7.data.value.expression.Expression.FunctionCall
import js7.data.value.expression.{Scope, ValueSearch}

private[expression] class DoubleScope(a: Scope, b: Scope) extends Scope
{
  override def symbolToValue(symbol: String) =
    a.symbolToValue(symbol) orElse b.symbolToValue(symbol)

  val findValue: ValueSearch => Option[Value] =
    search => a.findValue(search) orElse b.findValue(search)

  override def evalFunctionCall(functionCall: FunctionCall) =
    a.evalFunctionCall(functionCall) orElse b.evalFunctionCall(functionCall)
}
