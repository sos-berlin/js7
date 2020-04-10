package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.core.event.journal.JournalConfTest._
import com.typesafe.config.ConfigFactory
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
      jobscheduler.journal.delay = 1ms
      jobscheduler.journal.sync-delay = 222ms""")
    assert(JournalConf.fromConfig(c withFallback config) ==
      journalConf.copy(delay = 222.ms))
  }

  "delay is maximum of delay and sync-delay if sync-delay=on (2)" in {
    val c = ConfigFactory.parseString("""
      jobscheduler.journal.delay = 333ms
      jobscheduler.journal.sync-delay = 1ms""")
    assert(JournalConf.fromConfig(c withFallback config) ==
      journalConf.copy(delay = 333.ms))
  }

  "On sync=off sync-delay is not respected" in {
    val c = ConfigFactory.parseString("""
      jobscheduler.journal.sync = off
      jobscheduler.journal.delay = 1ms
      jobscheduler.journal.sync-delay = 222ms""")
    assert(JournalConf.fromConfig(c withFallback config) ==
      journalConf.copy(syncOnCommit = false, delay = 1.ms))
  }
}

object JournalConfTest
{
  private[journal] val config = ConfigFactory.parseString("""
     jobscheduler.journal {
       sync = on
       delay = 1ms
       sync-delay = 2ms
       event-buffer-size = 6
       snapshot.log-period = 4s
       snapshot.log-actor-limit = 5
       snapshot.period = 7h
       snapshot.when-bigger-than = 8G
       snapshot.log-period = 9ms
       snapshot.log-actor-limit = 11
       ack-warn-duration = 12s
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
    ackWarnDuration = 12.s,
    deleteObsoleteFiles = true)
}
