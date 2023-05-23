package js7.journal.configuration

import js7.base.configutils.Configs.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.journal.configuration.JournalConfTest.*

/**
  * @author Joacim Zschimmer
  */
final class JournalConfTest extends OurTestSuite
{
  "JournalConf" in {
    assert(JournalConf.fromConfig(config) == journalConf)
  }

  "delay is maximum of delay and sync-delay if sync-delay=on (1)" in {
    val c = config"""
      js7.journal.delay = 1ms
      js7.journal.sync-delay = 222ms"""
    assert(JournalConf.fromConfig(c withFallback config) ==
      journalConf.copy(delay = 222.ms))
  }

  "delay is maximum of delay and sync-delay if sync-delay=on (2)" in {
    val c = config"""
      js7.journal.delay = 333ms
      js7.journal.sync-delay = 1ms"""
    assert(JournalConf.fromConfig(c withFallback config) ==
      journalConf.copy(delay = 333.ms))
  }

  "On sync=off sync-delay is not respected" in {
    val c = config"""
      js7.journal.sync = off
      js7.journal.delay = 1ms
      js7.journal.sync-delay = 222ms"""
    assert(JournalConf.fromConfig(c withFallback config) ==
      journalConf.copy(syncOnCommit = false, delay = 1.ms))
  }
}

object JournalConfTest
{
  val config = config"""
     js7.journal {
       sync = on
       delay = 1ms
       sync-delay = 2ms
       coalesce-event-limit = 6
       slow-check-state = true  // Set by build.sbt, anyway
       snapshot.log-period = 4s
       snapshot.period = 7h
       snapshot.when-bigger-than = 8G
       snapshot.estimate-event-threshold = 1000
       persist-warn-durations = [ 20s, 30s ]
       remove-obsolete-files = true
       users-allowed-to-release-events = []
       log.ack-warn-durations = [ 10s, 15s ]
       log.info-events = ["MyEvent"]
     }"""

  val journalConf = JournalConf(
    syncOnCommit = true,
    simulateSync = None,
    delay = 2.ms,
    coalesceEventLimit = 6,
    snapshotPeriod = 7.h,
    snapshotSizeLimit = 8*1000*1000*1000L,
    snapshotSizeEstimateEventThreshold = 1000L,
    ackWarnDurations = List(10.s, 15.s),
    persistWarnDurations = List(20.s, 30.s),
    deleteObsoleteFiles = true,
    slowCheckState = true/*set by build.sbt*/,
    infoLogEvents = Set("MyEvent"))
}
