package js7.data.state

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.subagent.{SubagentItem, SubagentSelection, SubagentSelectionId}
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{ListValue, StringValue}
import scala.collection.View

final class AgentsSubagentIdsScope(state: StateView) extends Scope
{
  override def evalFunctionCall(functionCall: Expression.FunctionCall)(implicit scope: Scope) =
    functionCall match {
      case FunctionCall("subagentIds", Nil) =>
        Some(subagentIds(state, None))

      case FunctionCall("subagentIds", Seq(Argument(arg, None))) =>
        Some(
          arg.evalAsString
            .flatMap(idString =>
              subagentIds(state, Some(idString))))

      case _ =>
        super.evalFunctionCall(functionCall)
    }

  private def subagentIds(state: StateView, arg: Option[String]): Checked[ListValue] = {
    state.maybeAgentPath match {
      case None =>
        Left(Problem(
          "The subagentIds function is available only for ForkList statement " +
            "running at an Agent — use the agentPath argument!"))

      case Some(thisAgentPath) =>
        arg
          .match_ {
            case None =>
              Right(subagentIds(thisAgentPath))
            case Some(arg) =>
              SubagentSelectionId.checked(arg)
                .flatMap(subagentSelectionId =>
                  state.keyToItem(SubagentSelection).checked(subagentSelectionId))
                .map(_.subagentIds.map(_.string))
          }
          .map(strings =>
            ListValue(strings.map(StringValue(_)).toVector))
    }
  }

  private def subagentIds(agentPath: AgentPath): View[String] =
    state
      .keyToItem(SubagentItem).values.view
      .filter(_.agentPath == agentPath)
      .map(_.id.string)

  override def toString = "AgentsSubagentIdsScope"
}
