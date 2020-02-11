package com.sos.jobscheduler.master.cluster

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
import scala.concurrent.duration.FiniteDuration

final case class ClusterConf(
  maybeRole: Option[ClusterNodeRole],
  maybeOwnUri: Option[Uri],
  userAndPassword: Option[UserAndPassword],
  recouplingStreamReader: RecouplingStreamReaderConf,
  heartbeat: FiniteDuration,
  failAfter: FiniteDuration)

object ClusterConf
{
  def fromConfigAndFile(userId: UserId, config: Config): Checked[ClusterConf] =
    for {
      maybeRoleString <- config.checkedOptionAs[String]("jobscheduler.master.cluster.this-node.role")
      maybeOwnUri <- config.checkedOptionAs[Uri]("jobscheduler.master.cluster.this-node.uri")
      maybeOtherUri <- config.checkedOptionAs[Uri]("jobscheduler.master.cluster.other-node.uri")
      maybeRole <- maybeRoleString.map(_.toLowerCase) match {
        case None => Right(None)
        case Some("primary") => Right(Some(Primary(maybeOtherUri)))
        case Some("backup") => maybeOtherUri match {
          case None => Left(Problem("Backup role requires the primary nodes URI (cluster.other-node.uri)"))
          case Some(otherUri) => Right(Some(Backup(otherUri)))
        }
      }
      userAndPassword <- config.checkedOptionAs[SecretString]("jobscheduler.auth.cluster.password")
        .map(_.map(UserAndPassword(userId, _)))
      recouplingStreamReaderConf <- RecouplingStreamReaderConfs.fromConfig(config)
      heartbeat <- Right(config.getDuration("jobscheduler.master.cluster.heartbeat").toFiniteDuration)
      failAfter <- Right(config.getDuration("jobscheduler.master.cluster.fail-after").toFiniteDuration)
      //_ <- if (heartbeat < failAfter) Right(()) else
      //  Left(Problem(s"jobscheduler.master.cluster.heartbeat=${heartbeat.pretty} must be shorter than jobscheduler.master.cluster.fail-after=${failAfter.pretty}"))
    } yield
      new ClusterConf(maybeRole, maybeOwnUri, userAndPassword,
        recouplingStreamReaderConf.copy(
          timeout = heartbeat + (failAfter - heartbeat) / 2),
        heartbeat = heartbeat,
        failAfter = failAfter)
}
