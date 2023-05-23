package js7.cluster

import js7.base.auth.{UserAndPassword, UserId}
import js7.base.configutils.Configs.*
import js7.base.generic.SecretString
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.common.message.ProblemCodeMessages
import js7.data.cluster.{ClusterSetting, ClusterTiming}
import js7.data.node.NodeId
import js7.journal.configuration.JournalConfTest

/**
  * @author Joacim Zschimmer
  */
final class ClusterConfTest extends OurTestSuite
{
  ProblemCodeMessages.initialize()

  "fromConfig" - {
    "Minimum configuration" in {
      val config = config"""
        js7.journal.cluster.node.is-backup = no
        js7.journal.cluster.heartbeat = 7s
        js7.journal.cluster.heartbeat-timeout = 5s
        js7.journal.cluster.watch.uniqueness-memory-size = 100
        js7.web.client.idle-get-timeout = 50s
        js7.web.client.polling-delay = 1s
        js7.web.client.failure-delay = 5s
        """.withFallback(JournalConfTest.config)
      val clusterConf = ClusterConf.fromConfig(UserId("USER"), config)
      assert(clusterConf == Right(
        ClusterConf(
          JournalConfTest.journalConf,
          NodeId("Primary"),
          isBackup = false,
          None,
          None,
          RecouplingStreamReaderConf(
            timeout = 6.s,  // Between 5s and 7s
            delay = 1.s,
            failureDelay = 5.s),
          ClusterTiming(7.s, 5.s),
          clusterWatchUniquenessMemorySize = 100,
          config = config)))
    }

    "Full configuration" in {
      val config = config"""
        js7.journal.cluster.node.id = PRIMARY
        js7.journal.cluster.node.is-backup = no
        js7.journal.cluster.nodes = {
          PRIMARY: "https://PRIMARY"
          Backup: "https://BACKUP"
        }
        js7.journal.cluster.watch.uniqueness-memory-size = 100
        js7.journal.cluster.heartbeat = 7s
        js7.journal.cluster.heartbeat-timeout = 5s
        js7.auth.cluster.password = "PASSWORD"
        js7.web.client.idle-get-timeout = 50s
        js7.web.client.polling-delay = 1s
        js7.web.client.failure-delay = 5s
        """.withFallback(JournalConfTest.config)
      val checkedClusterConf = ClusterConf.fromConfig(UserId("USER"), config)
      assert(checkedClusterConf == Right(
        ClusterConf(
          JournalConfTest.journalConf,
          NodeId("PRIMARY"),
          isBackup = false,
          Some(ClusterSetting(
            Map(
              NodeId("PRIMARY") -> Uri("https://PRIMARY"),
              NodeId("Backup") -> Uri("https://BACKUP")),
            NodeId("PRIMARY"),
            ClusterTiming(7.s, 5.s),
            clusterWatchId = None)),
          Some(UserAndPassword(UserId("USER"), SecretString("PASSWORD"))),
          RecouplingStreamReaderConf(
            timeout = 6.s,  // Between 5s and 7s
            delay = 1.s,
            failureDelay = 5.s),
          ClusterTiming(7.s, 5.s),
          clusterWatchUniquenessMemorySize = 100,
          config = config)))
    }
  }
}
