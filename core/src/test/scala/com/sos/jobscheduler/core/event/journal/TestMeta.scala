package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.core.event.journal.TestJsonCodecs.TestKeyedEventJsonCodec
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[journal] object TestMeta
{
  val SnapshotJsonFormat = TypedJsonCodec[Any](
    Subtype[TestAggregate])

  def testJournalMeta(fileBase: Path) =
    new JournalMeta[TestEvent](SnapshotJsonFormat, TestKeyedEventJsonCodec, fileBase)
}
