package js7.data.subagent

import js7.base.problem.Problem
import scala.collection.immutable.Map.Map1

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

  final case class SubagentAlreadyDedicatedProblem(reasons: String)
  extends Problem.Coded:
    def arguments = Map("reasons" -> reasons)

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

  /** ProcessLostDueSubagentDeletedProblem is not a ProcessLostProblem, because the job
    * should not be repeated.
    */
  final case class ProcessLostDueSubagentDeletedProblem(subagentId: SubagentId)
  extends Problem.Coded:
    def arguments = Map1("subagentId", subagentId.toString)

  type NoDirectorProblem = NoDirectorProblem.type
  case object NoDirectorProblem extends Problem.ArgumentlessCoded:
    override val httpStatusCode = 503 // Service Unavailable

  type ProcessCancelledBeforeStartProblem = ProcessCancelledBeforeStartProblem.type
  case object ProcessCancelledBeforeStartProblem extends Problem.ArgumentlessCoded
