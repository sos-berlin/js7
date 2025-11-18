package js7.journal.test

import com.typesafe.config.Config
import java.nio.file.Path
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.config.Js7Config
import js7.base.configutils.Configs.*
import js7.journal.data.JournalLocation

/**
  * @author Joacim Zschimmer
  */
private[journal] object TestData:
  val TestConfig: Config = config"""
    js7.journal.sync = on
    js7.journal.delay = 0s
    js7.journal.sync-delay = 0s
    js7.journal.simulate-sync = 1ms
    js7.journal.snapshot.log-period = 10ms
    js7.journal.snapshot.log-actor-limit = 1
    js7.journal.slow-check-state = true
    js7.journal.release-events-delay = 0s
    js7.journal.remove-obsolete-files = false  # DIFFERS FROM DEFAULT TO ALLOW AWAITING FOR OLD EVENTS !
  """.withFallback(Js7Config.defaultConfig)

  val SnapshotJsonFormat = TypedJsonCodec[Any](
    Subtype[TestAggregate])

  def testJournalMeta(fileBase: Path) =
    JournalLocation(TestState, fileBase)
