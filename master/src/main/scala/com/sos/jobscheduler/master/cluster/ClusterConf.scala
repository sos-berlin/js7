package com.sos.jobscheduler.master.cluster

import cats.implicits._
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.configutils.Configs._
import com.sos.jobscheduler.common.http.configuration.RecouplingStreamReaderConf
import com.sos.jobscheduler.common.time.JavaTimeConverters.AsScalaDuration
import com.sos.jobscheduler.core.configuration.RecouplingStreamReaderConfs
import com.sos.jobscheduler.data.cluster.ClusterNodeRole
import com.sos.jobscheduler.data.cluster.ClusterNodeRole.{Backup, Primary}
import com.sos.jobscheduler.data.common.Uri
import com.typesafe.config.Config
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.concurrent.duration.FiniteDuration

final case class ClusterConf(
  role: ClusterNodeRole,
  /** None if role is Backup. */
  maybeUris: Option[Seq[Uri]],
  userAndPassword: Option[UserAndPassword],
  recouplingStreamReader: RecouplingStreamReaderConf,
  heartbeat: FiniteDuration,
  failAfter: FiniteDuration,
  agentUris: Seq[Uri],
  testHeartbeatLossPropertyKey: Option[String] = None)

object ClusterConf
{
  def fromConfig(userId: UserId, config: Config): Checked[ClusterConf] =
    for {
      role <- config.getString("jobscheduler.master.cluster.role")
        .toLowerCase match {
          case "primary" => Right(Primary)
          case "backup" => Right(Backup)
          case _ => Left(Problem("jobscheduler.master.cluster.role must be 'Primary' or 'Backup'"))
        }
      maybeUris <- {
        val key = "jobscheduler.master.cluster.uris"
        if (!config.hasPath(key))
          Right(None)
        else if (role != Primary)
          Left(Problem("jobscheduler.master.cluster.backup.uri may only be set for role=Primary"))
        else
          config.getStringList(key).asScala.toList.map(Uri.checked).sequence.map(Some.apply)
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
        role,
        maybeUris,
        userAndPassword,
        recouplingStreamReaderConf.copy(
          timeout = heartbeat + (failAfter - heartbeat) / 2),
        heartbeat = heartbeat,
        failAfter = failAfter,
        watchUris,
        testHeartbeatLoss)
}
