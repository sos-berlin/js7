package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.data.event.EventId
import java.nio.file.Files.exists
import java.nio.file.{Files, Path}
import java.util.regex.Pattern
import scala.collection.JavaConverters._

/**
  * @author Joacim Zschimmer
  */
object JournalFiles
{
  def journalFile(journalFileBase: Path, after: EventId, extraSuffix: String = ""): Path =
    journalFileBase resolveSibling s"${journalFileBase.getFileName}--$after.journal$extraSuffix"

  def currentFile(journalFileBase: Path): Checked[Path] =
    listJournalFiles(journalFileBase).lastOption.map(_.file) toChecked Problem(s"No journal under '$journalFileBase'")

  def listJournalFiles(journalFileBase: Path): Vector[HistoricJournalFile] = {
    val directory = journalFileBase.getParent
    if (!exists(directory))
      Vector.empty
    else
      autoClosing(Files.list(directory)) { stream ⇒
        val pattern = Pattern.compile(Pattern.quote(journalFileBase.toString) + """--([0-9]+)\.journal""")
        stream.iterator.asScala.flatMap { file ⇒
          val matcher = pattern.matcher(file.toString)
          matcher.matches ? HistoricJournalFile(afterEventId = matcher.group(1).toLong, file)
        } .toVector.sortBy(_.afterEventId)
      }
  }
}
