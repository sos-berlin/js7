package js7.cluster

import cats.instances.either.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import js7.base.configutils.Configs.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.web.Uri
import js7.common.http.configuration.{RecouplingStreamReaderConf, RecouplingStreamReaderConfs}
import js7.data.cluster.{ClusterSetting, ClusterTiming, ClusterWatchId}
import js7.data.node.NodeId
import js7.journal.configuration.JournalConf
import scala.jdk.CollectionConverters.*

final case class ClusterConf(
  journalConf: JournalConf,
  ownId: NodeId,
  isBackup: Boolean,
  maybeClusterSetting: Option[ClusterSetting],
  recouplingStreamReader: RecouplingStreamReaderConf,
  timing: ClusterTiming,
  clusterWatchUniquenessMemorySize: Int,
  testHeartbeatLossPropertyKey: Option[String] = None,
  testAckLossPropertyKey: Option[String] = None,
  testDontHaltWhenPassiveLostRejected: Boolean = false,
  config: Config)
{
  def isPrimary = !isBackup
}

object ClusterConf
{
  val ClusterProductName = "js7.controller.cluster"

  def fromConfig(config: Config): Checked[ClusterConf] = {
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
                case _ => Left(Problem(
                  "A cluster node URI is expected to be configured as a string"))
              }
            }
            .toVector
            .sequence
            .map(o => Some(o.toMap))
      }
      nodeId = config.optionAs[NodeId]("js7.journal.cluster.node.id") getOrElse
        NodeId(if (isBackup) "Backup" else "Primary")
      recouplingStreamReaderConf <- RecouplingStreamReaderConfs.fromConfig(config)
      heartbeat = config.getDuration("js7.journal.cluster.heartbeat").toFiniteDuration
      heartbeatTimeout = config.getDuration("js7.journal.cluster.heartbeat-timeout").toFiniteDuration
      timing <- ClusterTiming.checked(heartbeat, heartbeatTimeout)
      clusterWatchId = config.optionAs[ClusterWatchId]("clusterWatchId")
      clusterWatchUniquenessMemorySize = config.getInt("js7.journal.cluster.watch.uniqueness-memory-size")
      testHeartbeatLoss = config.optionAs[String]("js7.journal.cluster.TEST-HEARTBEAT-LOSS")
      testAckLoss = config.optionAs[String]("js7.journal.cluster.TEST-ACK-LOSS")
      testDontHaltWhenPassiveLostRejected = config.getBoolean(
        "js7.journal.cluster.dont-halt-when-passive-lost-rejected", false)
      setting <- maybeIdToUri.traverse(ClusterSetting.checked(_, nodeId, timing, clusterWatchId))
    } yield
      new ClusterConf(
        JournalConf.fromConfig(config),
        nodeId,
        isBackup = isBackup,
        setting,
        recouplingStreamReaderConf.copy(
          timeout = heartbeat + (heartbeatTimeout - heartbeat) / 2),
        timing,
        clusterWatchUniquenessMemorySize,
        testHeartbeatLoss,
        testAckLoss,
        testDontHaltWhenPassiveLostRejected,
        config = config)
  }
}
