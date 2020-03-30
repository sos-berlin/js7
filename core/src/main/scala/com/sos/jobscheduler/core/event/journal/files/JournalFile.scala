package com.sos.jobscheduler.core.event.journal.files

import com.sos.jobscheduler.base.generic.GenericString.EmptyStringProblem
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.data.event.EventId
import java.io.RandomAccessFile
import java.nio.file.Path
import java.util.regex.Pattern

/**
  * @author Joacim Zschimmer
  */
private[journal] final case class JournalFile private[journal](afterEventId: EventId, file: Path)
{
  override def toString = file.getFileName.toString

  def properLength: Long =
    autoClosing(new RandomAccessFile(file, "r")) { f =>
      var truncated = f.length
      while (truncated > 0) {
        f.seek(truncated - 1)
        if (f.read() == '\n') return truncated
        truncated -= 1
      }
      0
    }
}

object JournalFile
{
  def fromFileBase(fileBase: Path, afterEventId: EventId) =
    JournalFile(afterEventId, toFile(fileBase, afterEventId))

  def toFile(fileBase: Path, afterEventId: EventId): Path =
    fileBase resolveSibling s"${fileBase.getFileName}--$afterEventId.journal"

  def checkedEventId(fileBase: Path, file: Path): Checked[EventId] =
    new Matcher(fileBase).checkedEventId(file)

  final class Matcher(fileBase: Path) {
    private val pattern = Pattern.compile(Pattern.quote(fileBase.toString) + """--([0-9]+)\.journal""")

    def checkedJournalFile(file: Path): Checked[JournalFile] =
      checkedEventId(file).map(JournalFile(_, file))

    def checkedEventId(file: Path): Checked[EventId] =
      file.getFileName match {
        case null => Left(EmptyStringProblem("File"))
        case filename =>
          val matcher = pattern.matcher(filename.toString)
          (matcher.matches ? matcher.group(1).toLong)
            .toChecked(Problem(s"Filename does not match a journal filename: $filename"))
      }
  }

  private final class NotAJournalFileProblem(file: Path) extends Problem.Coded {
    def arguments = Map("file" -> file.toString)
  }
}
