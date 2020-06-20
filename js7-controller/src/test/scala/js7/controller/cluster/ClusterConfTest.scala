package js7.controller.cluster

import com.typesafe.config.ConfigFactory
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.core.message.ProblemCodeMessages
import js7.data.cluster.ClusterNodeId
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterConfTest extends AnyFreeSpec
{
  ProblemCodeMessages.initialize()

  "fromConfig" - {
    "Minimum configuration" in {
      val config = ConfigFactory.parseString("""
        js7.journal.cluster.node.is-backup = no
        js7.journal.cluster.heartbeat = 7s
        js7.journal.cluster.fail-after = 5s
        js7.journal.cluster.watches = [ "http://AGENT-1", "http://AGENT-2" ]
        js7.web.client.idle-get-timeout = 50s
        js7.web.client.delay-between-polling-gets = 1s""")
      val clusterConf = ClusterConf.fromConfig(UserId("USER"), config)
      assert(clusterConf == Right(
        ClusterConf(
          ClusterNodeId("Primary"),
          isBackup = false,
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
        js7.journal.cluster.node.id = A
        js7.journal.cluster.node.is-backup = no
        js7.journal.cluster.nodes = {
          A: "http://A"
          B: "http://B"
        }
        js7.journal.cluster.watches = [ "http://AGENT" ]
        js7.journal.cluster.heartbeat = 7s
        js7.journal.cluster.fail-after = 5s
        js7.auth.cluster.password = "PASSWORD"
        js7.web.client.idle-get-timeout = 50s
        js7.web.client.delay-between-polling-gets = 1s""")
      val checkedClusterConf = ClusterConf.fromConfig(UserId("USER"), config)
      assert(checkedClusterConf == Right(
        ClusterConf(
          ClusterNodeId("A"),
          isBackup = false,
          Some(Map(
            ClusterNodeId("A") -> Uri("http://A"),
            ClusterNodeId("B") -> Uri("http://B"))),
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
