package com.sos.jobscheduler.master.cluster

import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.http.configuration.RecouplingStreamReaderConf
import com.sos.jobscheduler.core.message.ProblemCodeMessages
import com.sos.jobscheduler.data.cluster.ClusterNodeRole
import com.sos.jobscheduler.data.common.Uri
import com.typesafe.config.ConfigFactory
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
        jobscheduler.master.cluster.idle-get-timeout = 50s
        jobscheduler.master.cluster.delay-between-polling-gets = 1s
        jobscheduler.master.cluster.heartbeat = 7s""")
      val clusterConf = ClusterConf.fromConfigAndFile(UserId("USER"), config)  // Creates file
      assert(clusterConf == Right(
        ClusterConf(
          None,
          None,
          None,
          streamReaderConf,
          7.s)))
    }

    "Full configuration" in {
      val config = ConfigFactory.parseString("""
        jobscheduler.master.cluster.this-node.role = Primary
        jobscheduler.master.cluster.this-node.uri = "http://PRIMARY"
        jobscheduler.master.cluster.other-node.uri = "http://BACKUP"
        jobscheduler.master.cluster.idle-get-timeout = 50s
        jobscheduler.master.cluster.delay-between-polling-gets = 1s
        jobscheduler.master.cluster.heartbeat = 7s
        jobscheduler.auth.cluster.password = "PASSWORD" """)
      val checkedClusterConf = ClusterConf.fromConfigAndFile(UserId("USER"), config)  // Creates the file as a side-effect
      assert(checkedClusterConf == Right(
        ClusterConf(
          Some(ClusterNodeRole.Primary(Some(Uri("http://BACKUP")))),
          Some(Uri("http://PRIMARY")),
          Some(UserAndPassword(UserId("USER"), SecretString("PASSWORD"))),
          streamReaderConf,
          7.s)))
    }
  }
}
