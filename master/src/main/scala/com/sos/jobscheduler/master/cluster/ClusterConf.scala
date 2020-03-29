package com.sos.jobscheduler.master.cluster

import cats.implicits._
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.configutils.Configs._
import com.sos.jobscheduler.common.http.configuration.RecouplingStreamReaderConf
import com.sos.jobscheduler.common.time.JavaTimeConverters.AsScalaDuration
import com.sos.jobscheduler.core.configuration.RecouplingStreamReaderConfs
import com.sos.jobscheduler.data.cluster.{ClusterNodeId, ClusterSetting}
import com.sos.jobscheduler.data.common.Uri
import com.typesafe.config.Config
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.concurrent.duration.FiniteDuration

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
    val isBackup = config.getBoolean("jobscheduler.master.cluster.node.is-backup")
    for {
      ownId <- config.checkedOptionAs[ClusterNodeId]("jobscheduler.master.cluster.node.id")
        .map(_ getOrElse ClusterNodeId(if (isBackup) "Backup" else "Primary"))
      maybeIdToUri <- {
        val key = "jobscheduler.master.cluster.nodes"
        if (!config.hasPath(key))
          Right(None)
        else if (isBackup)
          Left(Problem(s"Backup cluster node must node have a '$key' configuration"))
        else {
          val vector: Vector[Checked[(ClusterNodeId, Uri)]]/*Scala 2.12 requires type*/ =
            config.getObject(key)
              .asScala.toVector
              .map { case (k, v) =>
                v.unwrapped match {
                  case v: String => ClusterNodeId.checked(k).flatMap(id => Uri.checked(v).map(id -> _))
                  case _ => Left(Problem("A cluster node URI is expected to be configured as a string"))
                }
              }
          vector.sequence
            .flatMap(idToUri =>
              ClusterSetting.checkUris(idToUri.toMap) map Some.apply)
        }
      }
      userAndPassword <- config.checkedOptionAs[SecretString]("jobscheduler.auth.cluster.password")
        .map(_.map(UserAndPassword(userId, _)))
      recouplingStreamReaderConf <- RecouplingStreamReaderConfs.fromConfig(config)
      heartbeat <- Right(config.getDuration("jobscheduler.master.cluster.heartbeat").toFiniteDuration)
      failAfter <- Right(config.getDuration("jobscheduler.master.cluster.fail-after").toFiniteDuration)
      watchUris <- Right(config.getStringList("jobscheduler.master.cluster.watches").asScala.toVector map Uri.apply)
      testHeartbeatLoss <- Right(config.optionAs[String]("jobscheduler.master.cluster.TEST-HEARTBEAT-LOSS"))
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
