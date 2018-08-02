package com.sos.jobscheduler.core.event.journal.files

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.data.event.{Event, EventId}
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
      autoClosing(Files.list(directory)) { stream ⇒
        val pattern = JournalFile.pattern(journalFileBase.getFileName)
        stream.iterator.asScala.flatMap { file ⇒
          val matcher = pattern.matcher(file.getFileName.toString)
          matcher.matches ? JournalFile(afterEventId = matcher.group(1).toLong, file)
        } .toVector.sortBy(_.afterEventId)
      }
  }

  implicit final class JournalMetaOps[E <: Event](private val underlying: JournalMeta[E]) extends AnyVal
  {
    def file(after: EventId, extraSuffix: String = ""): Path =
      JournalFile.toFile(underlying.fileBase, after, extraSuffix)
  }
}
