package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.data.value.Value
import js7.data.value.expression.Expression.FunctionCall
import js7.data.value.expression.Scope
import scala.annotation.unused

final class ArgumentlessFunctionScope(
  symbolToValue: PartialFunction[String, Checked[Value]])
extends Scope:

  private def lifted = symbolToValue.lift

  override def evalFunctionCall(functionCall: FunctionCall)(using @unused scope: Scope) =
    functionCall match
      case FunctionCall(name, None | Some(Nil)) => lifted(name)
      case _ => None

  override def toString = "ArgumentlessFunctionScope"


object ArgumentlessFunctionScope:

  def apply(nameToValue: PartialFunction[String, Checked[Value]]): Scope =
    new ArgumentlessFunctionScope(nameToValue)

  def simpleJava(nameToValue: PartialFunction[String, Value.SimpleJava]): Scope =
    apply:
      Function.unlift: (k: String) =>
        nameToValue.lift(k).map: v =>
          Value.ofSimpleJava(v)
