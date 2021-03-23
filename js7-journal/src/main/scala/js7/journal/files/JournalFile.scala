package js7.journal.files

import java.io.RandomAccessFile
import java.nio.file.Path
import java.util.regex.Pattern
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.EventId

/**
  * @author Joacim Zschimmer
  */
private[journal] final case class JournalFile private[journal](afterEventId: EventId, file: Path)
{
  override def toString = file.getFileName.toString

  def properLength: Long =
    autoClosing(new RandomAccessFile(file.toFile, "r")) { f =>
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

  private[files] def garbagePattern(fileBase: Path): Pattern =
    Pattern.compile(Pattern.quote(fileBase.toString) +
    """--([0-9]+)\.journal.tmp""")
    // Keep truncated data: """--([0-9]+)\.journal(?:\.tmp|~.*)""")

  final class Matcher(fileBase: Path) {
    private val pattern = Pattern.compile(Pattern.quote(fileBase.toString) + """--([0-9]+)\.journal""")

    def checkedJournalFile(file: Path): Checked[JournalFile] =
      checkedEventId(file).map(JournalFile(_, file))

    def checkedEventId(file: Path): Checked[EventId] =
      file.getFileName match {
        case null => Left(EmptyStringProblem("File"))
        case filename =>
          val matcher = pattern.matcher(filename.toString)
          (matcher.matches() ? matcher.group(1).toLong)
            .toChecked(Problem(s"Filename does not match a journal filename: $filename"))
      }
  }
}
