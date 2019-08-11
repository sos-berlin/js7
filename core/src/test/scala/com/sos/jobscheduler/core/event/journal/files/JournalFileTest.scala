package com.sos.jobscheduler.core.event.journal.files

import java.nio.file.Paths
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalFileTest extends FreeSpec
{
  "toFile" in {
    assert(JournalFile.toFile(Paths.get("DIR/NAME"), 123) == Paths.get("DIR/NAME--123.journal"))
  }

  "maybeJournalfile" in {
    val matcher = new JournalFile.Matcher(Paths.get("NAME"))
    assert(matcher.checkedEventId(Paths.get("NAME--0.journal")) == Right(0))
    assert(matcher.checkedEventId(Paths.get("NAME--1112223334445556667.journal")) == Right(1112223334445556667L))
    assert(matcher.checkedEventId(Paths.get("NAME---1_journal")).isLeft)
    assert(matcher.checkedEventId(Paths.get("NAME--0_journal")).isLeft)
    assert(matcher.checkedEventId(Paths.get("NAME--X.journal")).isLeft)
    assert(matcher.checkedEventId(Paths.get("NAME--.journal")).isLeft)
    assert(matcher.checkedEventId(Paths.get("OTHER--0.journal")).isLeft)
    assert(matcher.checkedEventId(Paths.get("--0.journal")).isLeft)
  }
}
