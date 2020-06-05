package js7.core.event.journal.test

import com.typesafe.config.ConfigFactory
import java.nio.file.Path
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.core.event.journal.data.JournalMeta
import js7.core.event.journal.test.TestJsonCodecs.TestKeyedEventJsonCodec

/**
  * @author Joacim Zschimmer
  */
private[event] object TestData
{
  val TestConfig = ConfigFactory.parseString("""
     |js7.journal.sync = on
     |js7.journal.delay = 0s
     |js7.journal.sync-delay = 0s
     |js7.journal.simulate-sync = 1ms
     |js7.journal.snapshot.log-period = 10ms
     |js7.journal.snapshot.log-actor-limit = 1
     |js7.journal.event-buffer-size = 1000
     |js7.journal.use-journaled-state-as-snapshot = true
     |js7.journal.slow-check-state = true
     |js7.journal.snapshot.period = 1h
     |js7.journal.snapshot.when-bigger-than = 1G
     |js7.journal.ack-warn-durations = [ 10s ]
     |js7.journal.remove-obsolete-files = false  # DIFFERS FROM DEFAULT TO ALLOW AWAITNG FOR OLD EVENTS !
     |js7.journal.users-allowed-to-release-events = []
     |""".stripMargin)

  val SnapshotJsonFormat = TypedJsonCodec[Any](
    Subtype[TestAggregate])

  def testJournalMeta(fileBase: Path) =
    new JournalMeta(SnapshotJsonFormat, TestKeyedEventJsonCodec, fileBase)
}
