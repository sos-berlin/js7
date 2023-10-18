package js7.journal.files

import java.io.RandomAccessFile
import java.nio.file.Path
import java.util.regex.Pattern
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.EventId
import org.jetbrains.annotations.TestOnly
import scala.util.boundary

/**
  * @param fileEventId EventId of the last Event before this journal file
  */
private[journal] final case class JournalFile private[journal](fileEventId: EventId, file: Path):
  override def toString = file.getFileName.toString

  @TestOnly // Not used
  private[files] def properLength: Long =
    boundary[Long]:
      autoClosing(new RandomAccessFile(file.toFile, "r")) { f =>
        var truncated = f.length
        while truncated > 0 do
          f.seek(truncated - 1)
          if f.read() == '\n' then boundary.break(truncated)
          truncated -= 1
        0
      }


object JournalFile:
  def toFile(fileBase: Path, fileEventId: EventId): Path =
    fileBase resolveSibling s"${fileBase.getFileName}--$fileEventId.journal"

  private[files] def anyJournalFilePattern(fileBase: Path): Pattern =
    Pattern.compile(
      Pattern.quote(fileBase.toString) + """-(-[0-9]+\.journal(\.tmp|(~.*))?|journal)""")

  private[files] def garbagePattern(fileBase: Path): Pattern =
    Pattern.compile(Pattern.quote(fileBase.toString) +
    """--([0-9]+)\.journal.tmp""")
    // Keep truncated data: """--([0-9]+)\.journal(?:\.tmp|~.*)""")

  final class Matcher(fileBase: Path):
    private val pattern = Pattern.compile(
      Pattern.quote(fileBase.toString) + """--([0-9]+)\.journal""")

    def checkedJournalFile(file: Path): Checked[JournalFile] =
      checkedFileEventId(file).map(JournalFile(_, file))

    def checkedFileEventId(file: Path): Checked[EventId] =
      file.getFileName match
        case null => Left(EmptyStringProblem("File"))
        case filename =>
          val matcher = pattern.matcher(filename.toString)
          (matcher.matches() ? matcher.group(1).toLong)
            .toChecked(Problem(s"Filename does not match a journal filename: $filename"))
