package js7.journal.configuration

import js7.base.time.ScalaTime._
import js7.common.configutils.Configs._
import js7.journal.configuration.JournalConfTest._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalConfTest extends AnyFreeSpec
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
  private val config = config"""
     js7.journal {
       sync = on
       delay = 1ms
       sync-delay = 2ms
       coalesce-event-limit = 6
       slow-check-state = true  // Set by build.sbt, anyway
       snapshot.log-period = 4s
       snapshot.log-actor-limit = 5
       snapshot.period = 7h
       snapshot.when-bigger-than = 8G
       snapshot.log-period = 9ms
       snapshot.log-actor-limit = 11
       ack-warn-durations = [ 10s, 15s ]
       persist-warn-durations = [ 20s, 30s ]
       remove-obsolete-files = true
       users-allowed-to-release-events = []
     }"""

  private val journalConf = JournalConf(
    syncOnCommit = true,
    simulateSync = None,
    delay = 2.ms,
    coalesceEventLimit = 6,
    snapshotPeriod = 7.h,
    snapshotSizeLimit = 8*1000*1000*1000L,
    snapshotLogProgressPeriod = 9.ms,
    snapshotLogProgressActorLimit = 11,
    ackWarnDurations = List(10.s, 15.s),
    persistWarnDurations = List(20.s, 30.s),
    deleteObsoleteFiles = true,
    slowCheckState = true/*set by build.sbt*/)
}
