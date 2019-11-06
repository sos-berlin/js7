package com.sos.jobscheduler.master.cluster

import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.http.configuration.RecouplingStreamReaderConf
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryDirectory
import com.sos.jobscheduler.core.message.ProblemCodeMessages
import com.sos.jobscheduler.data.cluster.{ClusterNodeId, ClusterNodeRole}
import com.sos.jobscheduler.data.common.Uri
import com.typesafe.config.ConfigFactory
import java.nio.file.Files.exists
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterConfTest extends FreeSpec
{
  ProblemCodeMessages.initialize()

  private val streamReaderConf = RecouplingStreamReaderConf(timeout = 50.s, delay = 1.s)

  "fromConfigAndFile" - {
    "Minimum configuration" in {
      val config = ConfigFactory.parseString("""
        jobscheduler.web.client.polling-get-without-traffic-timeout = 50s
        jobscheduler.web.client.delay-between-polling-gets = 1s""")
      withTemporaryDirectory("ClusterConfTest-") { dir =>
        val file = dir  / "NodeId"
        val clusterConf = ClusterConf.fromConfigAndFile(config, file)  // Creates file
        assert(clusterConf == Right(
          ClusterConf(ClusterNodeRole.Primary(None, None), ClusterNodeId(file.contentString), streamReaderConf)))
      }
    }

    "Full configuration" in {
      val config = ConfigFactory.parseString("""
        jobscheduler.master.cluster.id = PRIMARY
        jobscheduler.master.cluster.other-node-is-backup.uri = "http://BACKUP"
        jobscheduler.master.cluster.other-node-is-backup.id = BACKUP
        jobscheduler.web.client.polling-get-without-traffic-timeout = 50s
        jobscheduler.web.client.delay-between-polling-gets = 1s""")
      withTemporaryDirectory("ClusterConfTest-") { dir =>
        val file = dir  / "NodeId"
        val checkedClusterConf = ClusterConf.fromConfigAndFile(config, file)  // Creates the file as a side-effect
        assert(checkedClusterConf == Right(
          ClusterConf(
            ClusterNodeRole.Primary(Some(ClusterNodeId("BACKUP")), Some(Uri("http://BACKUP"))),
            ClusterNodeId("PRIMARY"), streamReaderConf)))
        assert(!exists(file))
      }
    }

    "Invalid ClusterNodeId in file" in {
      val config = ConfigFactory.parseString("""
        jobscheduler.web.client.polling-get-without-traffic-timeout = 50s
        jobscheduler.web.client.delay-between-polling-gets = 1s""")
      withTemporaryDirectory("ClusterConfTest-") { dir =>
        val file = dir  / "NodeId"
        file := ""
        assert(ClusterConf.fromConfigAndFile(config, file) ==
          Left(Problem(s"File '$file' does not contain a valid ClusterNodeId: EmptyString: The empty string is not a value of the ClusterNodeId type")))
      }
    }
  }
}
