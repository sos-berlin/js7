package js7.cluster

import akka.http.scaladsl.model.StatusCodes.ServiceUnavailable
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.data.cluster.{ClusterCommand, ClusterState}
import js7.data.node.NodeId
import scala.concurrent.duration.FiniteDuration

object Problems
{
  final case object ClusterNodeIsNotYetReadyProblem extends Problem.ArgumentlessCoded {
    override val httpStatusCode = ServiceUnavailable.intValue/*503*/
  }

  final case object ClusterNodeIsNotActiveProblem extends Problem.ArgumentlessCoded {
    override val httpStatusCode = ServiceUnavailable.intValue/*503*/
  }

  final case object BackupClusterNodeNotAppointed extends Problem.ArgumentlessCoded {
    override val httpStatusCode = ServiceUnavailable.intValue/*503*/
  }

  final case class MissingPassiveClusterNodeHeartbeatProblem(passiveId: NodeId, duration: FiniteDuration) extends Problem.Coded {
    override def arguments = Map(
      "passiveId" -> passiveId.toString,
      "duration" -> duration.pretty,
    )
  }

  final case class ClusterCommandInapplicableProblem(command: ClusterCommand, clusterState: ClusterState)
  extends Problem.Coded {
    override def arguments = Map(
      "command" -> command.toString,
      "clusterState" -> clusterState.toString)
  }

  object ClusterNodeIsNotBackupProblem extends Problem.ArgumentlessCoded

  object PrimaryClusterNodeMayNotBecomeBackupProblem extends Problem.ArgumentlessCoded

  final case object ClusterNodesAlreadyAppointed extends Problem.ArgumentlessCoded

  final case object ClusterSettingNotUpdatable extends Problem.ArgumentlessCoded
}
