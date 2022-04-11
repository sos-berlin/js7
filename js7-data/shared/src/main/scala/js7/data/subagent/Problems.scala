package js7.data.subagent

import js7.base.problem.Problem

object Problems
{
  final case class SubagentIdMismatchProblem(
    requestedSubagentId: SubagentId,
    realSubagentId: SubagentId)
  extends Problem.Coded {
    def arguments = Map(
      "requestedSubagentId" -> requestedSubagentId.string,
      "realSubagentId" -> realSubagentId.string)
  }

  final case class SubagentRunIdMismatchProblem(subagentId: SubagentId)
  extends Problem.Coded {
    def arguments = Map("subagentId" -> subagentId.string)
  }

  type SubagentAlreadyDedicatedProblem = SubagentAlreadyDedicatedProblem.type
  case object SubagentAlreadyDedicatedProblem extends Problem.ArgumentlessCoded

  type SubagentNotDedicatedProblem = SubagentNotDedicatedProblem.type
  case object SubagentNotDedicatedProblem extends Problem.ArgumentlessCoded
}
