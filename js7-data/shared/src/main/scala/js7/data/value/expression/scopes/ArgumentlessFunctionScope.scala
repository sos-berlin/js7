package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.data.value.Value
import js7.data.value.expression.Expression.FunctionCall
import js7.data.value.expression.{Expression, Scope}

final class ArgumentlessFunctionScope(
  symbolToValue: PartialFunction[String, Checked[Value]])
extends Scope:

  private def lifted = symbolToValue.lift

  override def evalFunctionCall(functionCall: FunctionCall)(using Scope) =
    functionCall match
      case FunctionCall(name, Nil) => lifted(name)
      case _ => None

  override def toString = "ArgumentlessFunctionScope"


object ArgumentlessFunctionScope:
  def apply(nameToValue: PartialFunction[String, Checked[Value]]): Scope =
    new ArgumentlessFunctionScope(nameToValue)
