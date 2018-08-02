package com.sos.jobscheduler.core.event.journal.files

import com.sos.jobscheduler.data.event.EventId
import java.nio.file.Path
import java.util.regex.Pattern

/**
  * @author Joacim Zschimmer
  */
private[journal] final case class JournalFile private[journal](afterEventId: EventId, file: Path)
{
  override def toString = file.getFileName.toString
}

object JournalFile {
  def fromFileBase(fileBase: Path, afterEventId: EventId) =
    JournalFile(afterEventId, toFile(fileBase, afterEventId))

  def toFile(fileBase: Path, afterEventId: EventId, extraSuffix: String = ""): Path =
    fileBase resolveSibling s"${fileBase.getFileName}--$afterEventId.journal$extraSuffix"

  def pattern(fileBase: Path): Pattern =
    Pattern.compile(Pattern.quote(fileBase.toString) + """--([0-9]+)\.journal""")
}
