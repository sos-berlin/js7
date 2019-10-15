package com.sos.jobscheduler.master.cluster

import com.sos.jobscheduler.base.problem.Checked
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
      role <- config.checkedOptionAs[Uri]("jobscheduler.master.cluster.primary-uri")
        .map(_.fold[ClusterNodeRole](Primary)(Backup.apply))
      recouplingStreamReaderConf <- RecouplingStreamReaderConfs.fromConfig(config)
    } yield {
      val nodeId = {
        val file = nodeIdFile
        if (!exists(file)) {
          file := SecretStringGenerator.newSecretString().string  // Side effect !!!
        }
        ClusterNodeId(file.contentString.stripLineEnd/*In case it has been edited*/)
      }
      new ClusterConf(role, nodeId, recouplingStreamReaderConf)
    }
}
