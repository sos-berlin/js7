package js7.controller.cluster

import cats.implicits._
import com.typesafe.config.Config
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.problem.{Checked, Problem}
import js7.base.web.Uri
import js7.common.configutils.Configs._
import js7.common.http.configuration.{RecouplingStreamReaderConf, RecouplingStreamReaderConfs}
import js7.common.time.JavaTimeConverters.AsScalaDuration
import js7.data.cluster.{ClusterNodeId, ClusterSetting}
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters._

final case class ClusterConf(
  ownId: ClusterNodeId,
  isBackup: Boolean,
  /** None if id is Backup. */
  maybeIdToUri: Option[Map[ClusterNodeId, Uri]],
  userAndPassword: Option[UserAndPassword],
  recouplingStreamReader: RecouplingStreamReaderConf,
  heartbeat: FiniteDuration,
  failAfter: FiniteDuration,
  agentUris: Seq[Uri],
  testHeartbeatLossPropertyKey: Option[String] = None)

object ClusterConf
{
  def fromConfig(userId: UserId, config: Config): Checked[ClusterConf] = {
    val isBackup = config.getBoolean("js7.controller.cluster.node.is-backup")
    for {
      ownId <- config.checkedOptionAs[ClusterNodeId]("js7.controller.cluster.node.id")
        .map(_ getOrElse ClusterNodeId(if (isBackup) "Backup" else "Primary"))
      maybeIdToUri <- {
        val key = "js7.controller.cluster.nodes"
        if (!config.hasPath(key))
          Right(None)
        else if (isBackup)
          Left(Problem(s"Backup cluster node must node have a '$key' configuration"))
        else
          config.getObject(key)
            .asScala
            .map { case (k, v) =>
              v.unwrapped match {
                case v: String => ClusterNodeId.checked(k).flatMap(id => Uri.checked(v).map(id -> _))
                case _ => Left(Problem("A cluster node URI is expected to be configured as a string"))
              }
            }
            .toVector
            .sequence
            .flatMap(idToUri =>
              ClusterSetting.checkUris(idToUri.toMap) map Some.apply)
      }
      userAndPassword <- config.checkedOptionAs[SecretString]("js7.auth.cluster.password")
        .map(_.map(UserAndPassword(userId, _)))
      recouplingStreamReaderConf <- RecouplingStreamReaderConfs.fromConfig(config)
      heartbeat <- Right(config.getDuration("js7.controller.cluster.heartbeat").toFiniteDuration)
      failAfter <- Right(config.getDuration("js7.controller.cluster.fail-after").toFiniteDuration)
      watchUris <- Right(config.getStringList("js7.controller.cluster.watches").asScala.toVector map Uri.apply)
      testHeartbeatLoss <- Right(config.optionAs[String]("js7.controller.cluster.TEST-HEARTBEAT-LOSS"))
    } yield
      new ClusterConf(
        ownId,
        isBackup = isBackup,
        maybeIdToUri,
        userAndPassword,
        recouplingStreamReaderConf.copy(
          timeout = heartbeat + (failAfter - heartbeat) / 2),
        heartbeat = heartbeat,
        failAfter = failAfter,
        watchUris,
        testHeartbeatLoss)
  }
}
