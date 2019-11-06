package com.sos.jobscheduler.master.cluster

import cats.syntax.semigroup._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.auth.SecretStringGenerator
import com.sos.jobscheduler.common.configutils.Configs._
import com.sos.jobscheduler.common.http.configuration.RecouplingStreamReaderConf
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.core.configuration.RecouplingStreamReaderConfs
import com.sos.jobscheduler.data.cluster.ClusterNodeRole.{Backup, Primary}
import com.sos.jobscheduler.data.cluster.{ClusterNodeId, ClusterNodeRole}
import com.sos.jobscheduler.data.common.Uri
import com.typesafe.config.Config
import java.nio.file.Files.exists
import java.nio.file.Path

final case class ClusterConf(
  role: ClusterNodeRole,
  nodeId: ClusterNodeId,
  recouplingStreamReader: RecouplingStreamReaderConf)

object ClusterConf
{
  def fromConfigAndFile(config: Config, nodeIdFile: Path): Checked[ClusterConf] =
    for {
      role <- config.checkedOptionAs[Uri]("jobscheduler.master.cluster.other-node-is-primary.uri")
        .flatMap {
          case Some(uri) => Right(Backup(uri))
          case None =>
            for {
              uri <- config.checkedOptionAs[Uri]("jobscheduler.master.cluster.other-node-is-backup.uri")
              nodeId <- config.checkedOptionAs[ClusterNodeId]("jobscheduler.master.cluster.other-node-is-backup.id")
            } yield Primary(nodeId, uri)
        }
      recouplingStreamReaderConf <- RecouplingStreamReaderConfs.fromConfig(config)
      id <- config.checkedOptionAs[ClusterNodeId]("jobscheduler.master.cluster.id")
      nodeId <-
        id.fold {
          val file = nodeIdFile
          if (!exists(file)) {
            file := SecretStringGenerator.newSecretString().string // Side effect !!!
          }
          ClusterNodeId.checked(file.contentString.stripLineEnd /*In case it has been edited*/)
            .mapProblem(Problem(s"File '$file' does not contain a valid ClusterNodeId:") |+| _)
        }(Right.apply)
    } yield new ClusterConf(role, nodeId, recouplingStreamReaderConf)
}
