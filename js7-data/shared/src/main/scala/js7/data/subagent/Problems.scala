package js7.data.subagent

import js7.base.problem.Problem

object Problems:
  final case class SubagentIdMismatchProblem(
    requestedSubagentId: SubagentId,
    realSubagentId: SubagentId)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "requestedSubagentId" -> requestedSubagentId.string,
      "realSubagentId" -> realSubagentId.string)

  final case class SubagentRunIdMismatchProblem(subagentId: SubagentId)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "subagentId" -> subagentId.string)

  type SubagentAlreadyDedicatedProblem = SubagentAlreadyDedicatedProblem.type
  case object SubagentAlreadyDedicatedProblem extends Problem.ArgumentlessCoded

  type SubagentNotDedicatedProblem = SubagentNotDedicatedProblem.type
  case object SubagentNotDedicatedProblem extends Problem.ArgumentlessCoded

  type SubagentShutDownBeforeProcessStartProblem = SubagentShutDownBeforeProcessStartProblem.type
  case object SubagentShutDownBeforeProcessStartProblem extends Problem.ArgumentlessCoded

  type SubagentIsShuttingDownProblem = SubagentIsShuttingDownProblem.type
  case object SubagentIsShuttingDownProblem extends Problem.ArgumentlessCoded

  type DirectorIsSwitchingOverProblem = DirectorIsSwitchingOverProblem.type
  case object DirectorIsSwitchingOverProblem extends Problem.ArgumentlessCoded

  sealed trait ProcessLostProblem extends Problem.Coded

  // COMPATIBLE with v2.2
  type ProcessLostDueToUnknownReasonProblem = ProcessLostDueToUnknownReasonProblem.type
  case object ProcessLostDueToUnknownReasonProblem
  extends ProcessLostProblem, Problem.ArgumentlessCoded

  type ProcessLostDueToRestartProblem = ProcessLostDueToRestartProblem.type
  case object ProcessLostDueToRestartProblem
  extends ProcessLostProblem, Problem.ArgumentlessCoded

  type ProcessLostDueToResetProblem = ProcessLostDueToResetProblem.type
  case object ProcessLostDueToResetProblem
  extends ProcessLostProblem, Problem.ArgumentlessCoded

  type ProcessLostDueToShutdownProblem = ProcessLostDueToShutdownProblem.type
  case object ProcessLostDueToShutdownProblem
  extends ProcessLostProblem, Problem.ArgumentlessCoded

  type ProcessLostDueSubagentUriChangeProblem = ProcessLostDueSubagentUriChangeProblem.type
  case object ProcessLostDueSubagentUriChangeProblem
  extends ProcessLostProblem, Problem.ArgumentlessCoded

  type NoDirectorProblem = NoDirectorProblem.type
  case object NoDirectorProblem extends Problem.ArgumentlessCoded:
    override val httpStatusCode = 503 // Service Unavailable
