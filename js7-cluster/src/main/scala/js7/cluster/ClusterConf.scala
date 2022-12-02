package js7.cluster

import cats.instances.either.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.configutils.Configs.*
import js7.base.generic.SecretString
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.web.Uri
import js7.common.http.configuration.{RecouplingStreamReaderConf, RecouplingStreamReaderConfs}
import js7.data.cluster.{ClusterSetting, ClusterTiming}
import js7.data.node.NodeId
import scala.jdk.CollectionConverters.*

final case class ClusterConf(
  ownId: NodeId,
  isBackup: Boolean,
  maybeClusterSetting: Option[ClusterSetting],
  peersUserAndPassword: Option[UserAndPassword],
  recouplingStreamReader: RecouplingStreamReaderConf,
  timing: ClusterTiming,
  testHeartbeatLossPropertyKey: Option[String] = None)
{
  def isPrimary = !isBackup
}

object ClusterConf
{
  val ClusterProductName = "js7.controller.cluster"

  def fromConfig(userId: UserId, config: Config): Checked[ClusterConf] = {
    val isBackup = config.getBoolean("js7.journal.cluster.node.is-backup")
    for {
      maybeIdToUri <- {
        val key = "js7.journal.cluster.nodes"
        if (!config.hasPath(key))
          Right(None)
        else if (isBackup)
          Left(Problem(s"Backup cluster node must node have a '$key' configuration"))
        else
          config.getObject(key)
            .asScala
            .map { case (k, v) =>
              v.unwrapped match {
                case v: String => NodeId.checked(k).flatMap(id => Uri.checked(v).map(id -> _))
                case _ => Left(Problem("A cluster node URI is expected to be configured as a string"))
              }
            }
            .toVector
            .sequence
            .map(o => Some(o.toMap))
      }
      nodeId = config.optionAs[NodeId]("js7.journal.cluster.node.id")
        .getOrElse(NodeId(
          if (config.getBoolean("js7.journal.cluster.node.is-backup"))
            "Backup"
           else
            "Primary"))
      userAndPassword <- config.checkedOptionAs[SecretString]("js7.auth.cluster.password")
        .map(_.map(UserAndPassword(userId, _)))
      recouplingStreamReaderConf <- RecouplingStreamReaderConfs.fromConfig(config)
      heartbeat <- Right(config.getDuration("js7.journal.cluster.heartbeat").toFiniteDuration)
      heartbeatTimeout <- Right(config.getDuration("js7.journal.cluster.heartbeat-timeout").toFiniteDuration)
      timing <- ClusterTiming.checked(heartbeat, heartbeatTimeout)
      testHeartbeatLoss <- Right(config.optionAs[String]("js7.journal.cluster.TEST-HEARTBEAT-LOSS"))
      setting <- maybeIdToUri.traverse(ClusterSetting.checked(_, nodeId, timing))
    } yield
      new ClusterConf(
        nodeId,
        isBackup = isBackup,
        setting,
        userAndPassword,
        recouplingStreamReaderConf.copy(
          timeout = heartbeat + (heartbeatTimeout - heartbeat) / 2),
        timing,
        testHeartbeatLoss)
  }
}
