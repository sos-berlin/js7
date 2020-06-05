package js7.core.event.journal

import com.typesafe.config.ConfigFactory
import js7.base.time.ScalaTime._
import js7.core.event.journal.JournalConfTest._
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
    val c = ConfigFactory.parseString("""
      js7.journal.delay = 1ms
      js7.journal.sync-delay = 222ms""")
    assert(JournalConf.fromConfig(c withFallback config) ==
      journalConf.copy(delay = 222.ms))
  }

  "delay is maximum of delay and sync-delay if sync-delay=on (2)" in {
    val c = ConfigFactory.parseString("""
      js7.journal.delay = 333ms
      js7.journal.sync-delay = 1ms""")
    assert(JournalConf.fromConfig(c withFallback config) ==
      journalConf.copy(delay = 333.ms))
  }

  "On sync=off sync-delay is not respected" in {
    val c = ConfigFactory.parseString("""
      js7.journal.sync = off
      js7.journal.delay = 1ms
      js7.journal.sync-delay = 222ms""")
    assert(JournalConf.fromConfig(c withFallback config) ==
      journalConf.copy(syncOnCommit = false, delay = 1.ms))
  }
}

object JournalConfTest
{
  private[journal] val config = ConfigFactory.parseString("""
     js7.journal {
       sync = on
       delay = 1ms
       sync-delay = 2ms
       event-buffer-size = 6
       use-journaled-state-as-snapshot = false
       slow-check-state = true  // Set by build.sbt, anyway
       snapshot.log-period = 4s
       snapshot.log-actor-limit = 5
       snapshot.period = 7h
       snapshot.when-bigger-than = 8G
       snapshot.log-period = 9ms
       snapshot.log-actor-limit = 11
       ack-warn-durations = [ 10s, 15s ]
       remove-obsolete-files = true
       users-allowed-to-release-events = []
     }""")

  private val journalConf = JournalConf(
    syncOnCommit = true,
    simulateSync = None,
    delay = 2.ms,
    eventLimit = 6,
    snapshotPeriod = 7.h,
    snapshotSizeLimit = 8*1000*1000*1000L,
    snapshotLogProgressPeriod = 9.ms,
    snapshotLogProgressActorLimit = 11,
    ackWarnDurations = List(10.s, 15.s),
    deleteObsoleteFiles = true,
    slowCheckState = true/*set by build.sbt*/)
}
