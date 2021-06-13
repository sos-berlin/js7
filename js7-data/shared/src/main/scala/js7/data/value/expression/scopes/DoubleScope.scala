package js7.data.value.expression.scopes

import js7.data.value.expression.Expression.{FunctionCall, JobResourceSetting}
import js7.data.value.expression.{Scope, ValueSearch}

private[expression] class DoubleScope(a: Scope, b: Scope) extends Scope
{
  override def symbolToValue(symbol: String) =
    a.symbolToValue(symbol) orElse b.symbolToValue(symbol)

  override def findValue(search: ValueSearch) =
    a.findValue(search)
      .flatMap(_.fold(b.findValue(search))(o => Right(Some(o))))

  override def evalFunctionCall(functionCall: FunctionCall) =
    a.evalFunctionCall(functionCall) orElse b.evalFunctionCall(functionCall)

  override def evalJobResourceSetting(setting: JobResourceSetting) =
    a.evalJobResourceSetting(setting) orElse b.evalJobResourceSetting(setting)
}
