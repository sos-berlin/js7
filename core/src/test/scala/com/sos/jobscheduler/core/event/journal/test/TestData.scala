package com.sos.jobscheduler.core.event.journal.test

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.test.TestJsonCodecs.TestKeyedEventJsonCodec
import com.typesafe.config.ConfigFactory
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[event] object TestData
{
  val TestConfig = ConfigFactory.parseString("""
     |jobscheduler.journal.sync = on
     |jobscheduler.journal.delay = 0s
     |jobscheduler.journal.sync-delay = 0s
     |jobscheduler.journal.simulate-sync = 1ms
     |jobscheduler.journal.snapshot.log-period = 10ms
     |jobscheduler.journal.snapshot.log-actor-limit = 1
     |jobscheduler.journal.event-buffer-size = 1000
     |jobscheduler.journal.use-journaled-state-as-snapshot = true
     |jobscheduler.journal.slow-check-journaled-state = true
     |jobscheduler.journal.snapshot.period = 1h
     |jobscheduler.journal.snapshot.when-bigger-than = 1G
     |jobscheduler.journal.ack-warn-duration = 10s
     |jobscheduler.journal.remove-obsolete-files = false  # DIFFERS FROM DEFAULT TO ALLOW AWAITNG FOR OLD EVENTS !
     |jobscheduler.journal.users-allowed-to-release-events = []
     |""".stripMargin)

  val SnapshotJsonFormat = TypedJsonCodec[Any](
    Subtype[TestAggregate])

  def testJournalMeta(fileBase: Path) =
    new JournalMeta(SnapshotJsonFormat, TestKeyedEventJsonCodec, fileBase)
}
