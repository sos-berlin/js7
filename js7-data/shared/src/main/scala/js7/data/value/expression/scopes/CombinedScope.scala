package js7.data.value.expression.scopes

import js7.data.value.expression.Expression.{FunctionCall, JobResourceVariable}
import js7.data.value.expression.{Scope, ValueSearch}

private[expression] class CombinedScope(first: Scope, second: Scope)
extends Scope:

  override def symbolToValue =
    first.symbolToValue orElse second.symbolToValue

  override lazy val nameToCheckedValue =
    first.nameToCheckedValue orElse second.nameToCheckedValue

  override def findValue(search: ValueSearch) =
    first.findValue(search) orElse second.findValue(search)

  override def evalFunctionCall(functionCall: FunctionCall)(implicit scope: Scope) =
    first.evalFunctionCall(functionCall) orElse second.evalFunctionCall(functionCall)

  override def evalJobResourceVariable(v: JobResourceVariable)(implicit scope: Scope) =
    first.evalJobResourceVariable(v) orElse second.evalJobResourceVariable(v)

  override def toString = s"$first |+| $second"   // combine is associative
