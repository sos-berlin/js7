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

  "fromConfig" - {
    "Minimum configuration" in {
      val config = ConfigFactory.parseString("""
        jobscheduler.master.cluster.heartbeat = 7s
        jobscheduler.master.cluster.fail-after = 5s
        jobscheduler.master.cluster.role = Primary
        jobscheduler.master.cluster.watches = [ "http://AGENT-1", "http://AGENT-2" ]
        jobscheduler.web.client.idle-get-timeout = 50s
        jobscheduler.web.client.delay-between-polling-gets = 1s""")
      val clusterConf = ClusterConf.fromConfig(UserId("USER"), config)
      assert(clusterConf == Right(
        ClusterConf(
          ClusterNodeRole.Primary,
          None,
          None,
          RecouplingStreamReaderConf(
            timeout = 6.s,  // Between 5s and 7s
            delay = 1.s),
          7.s,
          5.s,
          Uri("http://AGENT-1") :: Uri("http://AGENT-2") :: Nil)))
    }

    "Full configuration" in {
      val config = ConfigFactory.parseString("""
        jobscheduler.master.cluster.role = Primary
        jobscheduler.master.cluster.uris = [ "http://PRIMARY", "http://BACKUP" ]
        jobscheduler.master.cluster.watches = [ "http://AGENT" ]
        jobscheduler.master.cluster.heartbeat = 7s
        jobscheduler.master.cluster.fail-after = 5s
        jobscheduler.auth.cluster.password = "PASSWORD"
        jobscheduler.web.client.idle-get-timeout = 50s
        jobscheduler.web.client.delay-between-polling-gets = 1s""")
      val checkedClusterConf = ClusterConf.fromConfig(UserId("USER"), config)
      assert(checkedClusterConf == Right(
        ClusterConf(
          ClusterNodeRole.Primary,
          Some(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil),
          Some(UserAndPassword(UserId("USER"), SecretString("PASSWORD"))),
          RecouplingStreamReaderConf(
            timeout = 6.s,  // Between 5s and 7s
            delay = 1.s),
          7.s,
          5.s,
          Uri("http://AGENT") :: Nil)))
    }
  }
}
