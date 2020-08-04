package js7.core.event.journal.test

import js7.common.configutils.Configs._
import java.nio.file.Path
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.core.event.journal.data.JournalMeta
import js7.core.event.journal.test.TestJsonCodecs.TestKeyedEventJsonCodec

/**
  * @author Joacim Zschimmer
  */
private[event] object TestData
{
  val TestConfig = config"""
    js7.journal.sync = on
    js7.journal.delay = 0s
    js7.journal.sync-delay = 0s
    js7.journal.simulate-sync = 1ms
    js7.journal.snapshot.log-period = 10ms
    js7.journal.snapshot.log-actor-limit = 1
    js7.journal.coalesce-event-limit = 1000
    js7.journal.use-journaled-state-as-snapshot = true
    js7.journal.slow-check-state = true
    js7.journal.snapshot.period = 1h
    js7.journal.snapshot.when-bigger-than = 1G
    js7.journal.ack-warn-durations = [ 10s ]
    js7.journal.persist-warn-durations = [ 10s ]
    js7.journal.users-allowed-to-release-events = []
    js7.journal.release-events-delay = 0s
    js7.journal.remove-obsolete-files = false  # DIFFERS FROM DEFAULT TO ALLOW AWAITNG FOR OLD EVENTS !
    """

  val SnapshotJsonFormat = TypedJsonCodec[Any](
    Subtype[TestAggregate])

  def testJournalMeta(fileBase: Path) =
    new JournalMeta(SnapshotJsonFormat, TestKeyedEventJsonCodec, fileBase)
}
