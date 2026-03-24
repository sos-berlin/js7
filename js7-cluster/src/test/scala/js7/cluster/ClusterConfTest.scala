package js7.cluster

import js7.base.configutils.Configs.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.DelayConf
import js7.base.web.Uri
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.common.message.ProblemCodeMessages
import js7.data.cluster.{ClusterSetting, ClusterTiming}
import js7.data.node.NodeId
import js7.journal.configuration.JournalConfTest

/**
  * @author Joacim Zschimmer
  */
final class ClusterConfTest extends OurTestSuite:

  ProblemCodeMessages.initialize()

  "fromConfig" - {
    "Minimum configuration" in:
      val config = config"""
        js7.journal.cluster.node.is-backup = no
        js7.journal.cluster.heartbeat = 3s
        js7.journal.cluster.heartbeat-timeout = 10s
        js7.journal.cluster.consent-timeout = 6s
        js7.journal.cluster.watch.uniqueness-memory-size = 100
        js7.journal.cluster.retry-delays = [ 1s ]
        js7.journal.cluster.suppress-failover = false
        js7.web.client.idle-get-timeout = 50s
        js7.web.client.keep-alive = 1s
        js7.web.client.polling-delay = 1s
        js7.web.client.failure-delays = [ 5s ]
        """.withFallback(JournalConfTest.config)
      val clusterConf = ClusterConf.fromConfig(config)
      assert(clusterConf == Right(
        ClusterConf(
          JournalConfTest.journalConf,
          NodeId("Primary"),
          isBackup = false,
          None,
          RecouplingStreamReaderConf(
            timeout = Some(6500.ms),  // Between 3s and 10s
            keepAlive = 1.s,
            delay = 1.s,
            DelayConf(5.s)),
          ClusterTiming(3.s, 10.s, 6.s),
          clusterWatchUniquenessMemorySize = 100,
          delayConf = DelayConf(1.s),
          config = config)))

    "Full configuration" in:
      val config = config"""
        js7.journal.cluster.node.id = PRIMARY
        js7.journal.cluster.node.is-backup = no
        js7.journal.cluster.nodes = {
          PRIMARY: "https://PRIMARY"
          Backup: "https://BACKUP"
        }
        js7.journal.cluster.watch.uniqueness-memory-size = 100
        js7.journal.cluster.heartbeat = 3s
        js7.journal.cluster.heartbeat-timeout = 10s
        js7.journal.cluster.consent-timeout = 6s
        js7.journal.cluster.retry-delays = [ 1s ]
        js7.journal.cluster.suppress-failover = true
        js7.web.client.idle-get-timeout = 50s
        js7.web.client.keep-alive = 1s
        js7.web.client.polling-delay = 1s
        js7.web.client.failure-delays = [ 5s ]
        """.withFallback(JournalConfTest.config)
      val checkedClusterConf = ClusterConf.fromConfig(config)
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
            ClusterTiming(3.s, 10.s, 6.s),
            clusterWatchId = None)),
          RecouplingStreamReaderConf(
            timeout = Some(6500.ms),  // Between 3s and 10s
            keepAlive = 1.s,
            delay = 1.s,
            DelayConf(5.s)),
          ClusterTiming(3.s, 10.s, 6.s),
          clusterWatchUniquenessMemorySize = 100,
          suppressFailover = true,
          delayConf = DelayConf(1.s),
          config = config)))
  }
