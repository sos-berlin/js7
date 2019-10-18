package com.sos.jobscheduler.core.event.journal.files

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.data.event.EventId
import java.nio.file.Files.exists
import java.nio.file.{Files, Path}
import scala.collection.JavaConverters._

/**
  * @author Joacim Zschimmer
  */
object JournalFiles
{
  def currentFile(journalFileBase: Path): Checked[Path] =
    listJournalFiles(journalFileBase).lastOption.map(_.file) toChecked Problem(s"No journal under '$journalFileBase'")

  def listJournalFiles(journalFileBase: Path): Vector[JournalFile] = {
    val directory = journalFileBase.getParent
    if (!exists(directory))
      Vector.empty
    else
      journalFileBase.getFileName match {
        case null => Vector.empty
        case baseFilename =>
          autoClosing(Files.list(directory)) { stream =>
            val matcher = new JournalFile.Matcher(baseFilename)
            stream.iterator.asScala
              .flatMap(file => matcher.checkedJournalFile(file).toOption)
              .toVector.sortBy(_.afterEventId)
          }
      }
  }

  implicit final class JournalMetaOps(private val underlying: JournalMeta) extends AnyVal
  {
    def file(after: EventId): Path =
      JournalFile.toFile(underlying.fileBase, after)
  }
}
