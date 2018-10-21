package com.sos.jobscheduler.core.event.journal.files

import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.event.EventId
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

  "pattern" in {
    val pattern = JournalFile.pattern(Paths.get("NAME"))
    def matchFile(string: String): Option[EventId] = {
      val matcher = pattern.matcher(string)
      matcher.matches ? matcher.group(1).toLong
    }
    assert(matchFile("NAME--0.journal") == Some(0))
    assert(matchFile("NAME--1112223334445556667.journal") == Some(1112223334445556667L))
    assert(matchFile("NAME--0_journal") == None)
    assert(matchFile("NAME--X.journal") == None)
    assert(matchFile("NAME--.journal") == None)
    assert(matchFile("OTHER--0.journal") == None)
    assert(matchFile("--0.journal") == None)
  }
}
