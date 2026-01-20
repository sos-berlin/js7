package js7.data.state

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentItem}
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{ListValue, StringValue, Value}
import scala.collection.View

final class AgentsSubagentIdsScope(engineState: EngineState) extends Scope:

  override def evalFunctionCall(functionCall: Expression.FunctionCall)(implicit scope: Scope)
  : Option[Checked[Value]] =
    functionCall match
      case FunctionCall("subagentIds", None | Some(Nil)) =>
        Some(subagentIds(engineState, None))

      case FunctionCall("subagentIds", Some(Seq(Argument(arg, None)))) =>
        Some(
          arg.evalAsString
            .flatMap(idString =>
              subagentIds(engineState, Some(idString))))

      case _ =>
        super.evalFunctionCall(functionCall)

  private def subagentIds(state: EngineState, arg: Option[String]): Checked[ListValue] =
    state.maybeAgentPath match
      case None =>
        Left(Problem(
          "The subagentIds function is available only for ForkList statement " +
            "running at an Agent — use the agentPath argument!"))

      case Some(thisAgentPath) =>
        arg
          .match
            case None =>
              Right(subagentIds(thisAgentPath))
            case Some(arg) =>
              SubagentBundleId.checked(arg)
                .flatMap(subagentBundleId =>
                  state.keyToItem(SubagentBundle).checked(subagentBundleId))
                .map(_.subagentIds.map(_.string))
          .map(strings =>
            ListValue(strings.toVector.sorted.map(StringValue(_))))

  private def subagentIds(agentPath: AgentPath): View[String] =
    engineState
      .keyToItem(SubagentItem).values.view
      .filter(_.agentPath == agentPath)
      .map(_.id.string)

  override def toString = "AgentsSubagentIdsScope"
