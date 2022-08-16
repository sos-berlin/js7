package js7.cluster

import js7.base.auth.{UserAndPassword, UserId}
import js7.base.configutils.Configs.*
import js7.base.generic.SecretString
import js7.base.test.Test
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.common.message.ProblemCodeMessages
import js7.data.cluster.{ClusterSetting, ClusterTiming}
import js7.data.node.NodeId

/**
  * @author Joacim Zschimmer
  */
final class ClusterConfTest extends Test
{
  ProblemCodeMessages.initialize()

  "fromConfig" - {
    "Minimum configuration" in {
      val config = config"""
        js7.journal.cluster.node.is-backup = no
        js7.journal.cluster.heartbeat = 7s
        js7.journal.cluster.heartbeat-timeout = 5s
        js7.journal.cluster.watches = [ "https://AGENT-1", "https://AGENT-2" ]
        js7.web.client.idle-get-timeout = 50s
        js7.web.client.polling-delay = 1s
        js7.web.client.failure-delay = 5s"""
      val clusterConf = ClusterConf.fromConfig(UserId("USER"), config)
      assert(clusterConf == Right(
        ClusterConf(
          NodeId("Primary"),
          isBackup = false,
          None,
          None,
          RecouplingStreamReaderConf(
            timeout = 6.s,  // Between 5s and 7s
            delay = 1.s,
            failureDelay = 5.s),
          ClusterTiming(7.s, 5.s))))
    }

    "Full configuration" in {
      val config = config"""
        js7.journal.cluster.node.id = PRIMARY
        js7.journal.cluster.node.is-backup = no
        js7.journal.cluster.nodes = {
          PRIMARY: "https://PRIMARY"
          Backup: "https://BACKUP"
        }
        js7.journal.cluster.watches = [ "https://CLUSTER-WATCH" ]
        js7.journal.cluster.heartbeat = 7s
        js7.journal.cluster.heartbeat-timeout = 5s
        js7.auth.cluster.password = "PASSWORD"
        js7.web.client.idle-get-timeout = 50s
        js7.web.client.polling-delay = 1s
        js7.web.client.failure-delay = 5s"""
      val checkedClusterConf = ClusterConf.fromConfig(UserId("USER"), config)
      assert(checkedClusterConf == Right(
        ClusterConf(
          NodeId("PRIMARY"),
          isBackup = false,
          Some(ClusterSetting(
            Map(
              NodeId("PRIMARY") -> Uri("https://PRIMARY"),
              NodeId("Backup") -> Uri("https://BACKUP")),
            NodeId("PRIMARY"),
            Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
            ClusterTiming(7.s, 5.s))),
          Some(UserAndPassword(UserId("USER"), SecretString("PASSWORD"))),
          RecouplingStreamReaderConf(
            timeout = 6.s,  // Between 5s and 7s
            delay = 1.s,
            failureDelay = 5.s),
          ClusterTiming(7.s, 5.s))))
    }
  }
}
