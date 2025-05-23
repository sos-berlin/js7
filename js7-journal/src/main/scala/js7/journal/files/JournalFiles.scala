package js7.journal.files

import java.io.IOException
import java.nio.file.Files.{createSymbolicLink, delete, exists}
import java.nio.file.LinkOption.NOFOLLOW_LINKS
import java.nio.file.{Files, Path, Paths}
import js7.base.io.file.FileUtils.touchFile
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.data.event.EventId
import js7.journal.data.JournalLocation
import scala.jdk.CollectionConverters.*
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
object JournalFiles:
  private val logger = Logger[this.type]

  def currentFile(journalFileBase: Path): Checked[Path] =
    listJournalFiles(journalFileBase).lastOption.map(_.file)
      .toChecked(Problem(s"No journal under '$journalFileBase'"))

  def listJournalFiles(journalFileBase: Path): Vector[JournalFile] =
    listFiles(journalFileBase): iterator =>
      val matcher = new JournalFile.Matcher(journalFileBase.getFileName)
      iterator
        .flatMap: file =>
          matcher.checkedJournalFile(file).toOption
        .toVector
        .sortBy(_.fileEventId)

  def listGarbageFiles(journalFileBase: Path, untilFileEventId: EventId): Vector[Path] =
    val pattern = JournalFile.garbagePattern(journalFileBase.getFileName)
    listFiles(journalFileBase):
      _.filter: file =>
        val matcher = pattern.matcher(file.getFileName.toString)
        matcher.matches() &&
          Try(matcher.group(1).toLong < untilFileEventId).getOrElse(false)
      .toVector
      .sorted

  private def listFiles[A](journalFileBase: Path)(body: Iterator[Path] => Vector[A]): Vector[A] =
    val directory = journalFileBase.getParent
    if !exists(directory) then
      Vector.empty
    else if journalFileBase.getFileName == null then
      Vector.empty
    else
      autoClosing(Files.list(directory)): stream =>
        body(stream.iterator.asScala)

  private[files] def deleteJournalIfMarked(fileBase: Path): Checked[Unit] =
    try
      val markerFile = deletionMarkerFile(fileBase)
      if exists(markerFile) then
        logger.debug(s"Marker file found: $markerFile")
        logger.warn("DELETE JOURNAL DUE TO JOURNAL RESET IN PREVIOUS RUN")
        deleteJournal(fileBase)
      Checked.unit
    catch case e: IOException =>
      Left(Problem.pure(e.toStringWithCauses))

  private[files] def deleteJournal(fileBase: Path, ignoreFailure: Boolean = false): Unit =
    val matches: String => Boolean = string =>
      JournalFile.anyJournalFilePattern(fileBase.getFileName).matcher(string).matches
    val markerFile = deletionMarkerFile(fileBase)
    if !exists(markerFile)/*required for test*/ then touchFile(markerFile)
    var failed = false
    for file <- listFiles(fileBase)(_.filter(file => matches(file.getFileName.toString)).toVector) do
      try
        logger.info(s"DELETE JOURNAL FILE $file")
        delete(file)
      catch case e: IOException if ignoreFailure =>
        logger.warn(s"Delete journal file: $file => ${e.toStringWithCauses}")
        failed = true
    if failed then
      logger.warn("Journal files will be deleted at next start")
    else
      delete(markerFile)

  private[files] def deletionMarkerFile(fileBase: Path): Path =
    Paths.get(s"$fileBase-DELETE!")

  object extensions:
    extension (journalLocation: JournalLocation)
      def file(after: EventId): Path =
        JournalFile.toFile(journalLocation.fileBase, after)

      def temporaryFile(after: EventId): Path =
        JournalLocation.toTemporaryFile(file(after = after))

      def currentFile: Checked[Path] =
        JournalFiles.currentFile(journalLocation.fileBase)

      def listJournalFiles: Vector[JournalFile] =
        JournalFiles.listJournalFiles(journalLocation.fileBase)

      def listGarbageFiles(untilFileEventId: EventId): Vector[Path] =
        JournalFiles.listGarbageFiles(journalLocation.fileBase, untilFileEventId)

      def updateSymbolicLink(toFile: Path): Unit =
        val symLink = Paths.get(s"${journalLocation.fileBase}-journal")  // We preserve the suffix ".journal" for the real journal files
        Try:
          if exists(symLink, NOFOLLOW_LINKS) then delete(symLink)
        Try:
          createSymbolicLink(symLink, toFile.getFileName)

      def deleteJournalIfMarked(): Checked[Unit] =
        JournalFiles.deleteJournalIfMarked(journalLocation.fileBase)

      def deleteJournal(ignoreFailure: Boolean = false): Unit =
        logger.warn("DELETE JOURNAL FILES DUE TO AGENT RESET")
        JournalFiles.deleteJournal(journalLocation.fileBase, ignoreFailure)

  def updateSymbolicLink(fileBase: Path, toFile: Path): Unit =
    assertThat(toFile.toString.startsWith(fileBase.toString))
    val symLink = Paths.get(s"$fileBase-journal")  // We preserve the suffix ".journal" for the real journal files
    Try:
      if exists(symLink, NOFOLLOW_LINKS) then delete(symLink)
    Try:
      createSymbolicLink(symLink, toFile.getFileName)
