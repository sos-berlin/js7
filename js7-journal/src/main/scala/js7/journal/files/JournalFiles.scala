package js7.journal.files

import java.nio.file.Files.{createSymbolicLink, delete, exists}
import java.nio.file.LinkOption.NOFOLLOW_LINKS
import java.nio.file.{Files, Path, Paths}
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.autoClosing
import js7.data.event.EventId
import js7.journal.data.JournalMeta
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
object JournalFiles
{
  def currentFile(journalFileBase: Path): Checked[Path] =
    listJournalFiles(journalFileBase).lastOption.map(_.file) toChecked Problem(s"No journal under '$journalFileBase'")

  def listJournalFiles(journalFileBase: Path): Vector[JournalFile] =
    listFiles(journalFileBase) { iterator =>
      val matcher = new JournalFile.Matcher(journalFileBase.getFileName)
      iterator
        .flatMap(file => matcher.checkedJournalFile(file).toOption)
        .toVector.sortBy(_.afterEventId)
    }

  def listGarbageFiles(journalFileBase: Path, untilEventId: EventId): Vector[Path] =
    listFiles(journalFileBase) { iterator =>
      val pattern = JournalFile.garbagePattern(journalFileBase.getFileName)
      iterator.filter { file =>
        val matcher = pattern.matcher(file.getFileName.toString)
        matcher.matches &&
          Try(matcher.group(1).toLong < untilEventId).getOrElse(false)
      }
      .toVector
      .sorted
    }

  private def listFiles[A](journalFileBase: Path)(body: Iterator[Path] => Vector[A]): Vector[A] = {
    val directory = journalFileBase.getParent
    if (!exists(directory))
      Vector.empty
    else if (journalFileBase.getFileName == null)
      Vector.empty
    else
      autoClosing(Files.list(directory)) { stream =>
        body(stream.iterator.asScala)
      }
  }

  implicit final class JournalMetaOps(private val underlying: JournalMeta) extends AnyVal
  {
    def file(after: EventId): Path =
      JournalFile.toFile(underlying.fileBase, after)

    def updateSymbolicLink(toFile: Path): Unit = {
      val symLink = Paths.get(s"${underlying.fileBase}-journal")  // We preserve the suffix ".journal" for the real journal files
      Try { if (exists(symLink, NOFOLLOW_LINKS)) delete(symLink) }
      Try { createSymbolicLink(symLink, toFile.getFileName) }
    }
  }

  def updateSymbolicLink(fileBase: Path, toFile: Path): Unit = {
    assertThat(toFile.toString startsWith fileBase.toString)
    val symLink = Paths.get(s"$fileBase-journal")  // We preserve the suffix ".journal" for the real journal files
    Try { if (exists(symLink, NOFOLLOW_LINKS)) delete(symLink) }
    Try { createSymbolicLink(symLink, toFile.getFileName) }
  }
}
