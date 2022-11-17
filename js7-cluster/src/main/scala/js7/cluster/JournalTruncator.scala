package js7.cluster

import java.io.RandomAccessFile
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.StandardOpenOption.{CREATE, TRUNCATE_EXISTING, WRITE}
import java.nio.file.{Files, Path, Paths}
import js7.base.log.Logger
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.JournalPosition
import js7.journal.files.JournalFiles
import scala.annotation.tailrec

private object JournalTruncator
{
  private val logger = Logger[this.type]

  private[cluster] def truncateJournal(
    journalFileBase: Path,
    failedAt: JournalPosition,
    keepTruncatedRest: Boolean)
  : Option[Path] = {
    var truncated = false
    val lastTwoJournalFiles = JournalFiles.listJournalFiles(journalFileBase) takeRight 2
    val journalFile =
      if (lastTwoJournalFiles.last.fileEventId == failedAt.fileEventId)
        lastTwoJournalFiles.last
      else if (lastTwoJournalFiles.lengthIs == 2 &&
        lastTwoJournalFiles.head.fileEventId == failedAt.fileEventId &&
        lastTwoJournalFiles.last.fileEventId > failedAt.fileEventId) {
        truncated = true
        val deleteFile = lastTwoJournalFiles.last.file
        logger.info(s"Removing journal file written after failover: ${deleteFile.getFileName}")
        // Keep the file for debugging
        Files.move(deleteFile, Paths.get(s"$deleteFile~DELETED-AFTER-FAILOVER"), REPLACE_EXISTING)
        JournalFiles.updateSymbolicLink(journalFileBase, lastTwoJournalFiles.head.file)
        lastTwoJournalFiles.head
      } else
        sys.error("Failed-over node's JournalPosition does not match local journal files:" +
          s" $failedAt <-> ${lastTwoJournalFiles.map(_.file.getFileName).mkString(", ")}")
    assertThat(journalFile.fileEventId == failedAt.fileEventId)

    val file = journalFile.file
    val fileSize = Files.size(file)
    if (fileSize != failedAt.position) {
      if (fileSize < failedAt.position)
        sys.error(s"Journal file '${journalFile.file.getFileName} is shorter than the failed-over position ${failedAt.position}")
      logger.info(s"Truncating journal file at failover position ${failedAt.position} " +
        s"(${fileSize - failedAt.position} bytes): ${journalFile.file.getFileName}")
      truncated = true
      truncateFile(file, failedAt.position, keep = keepTruncatedRest)
    }
    truncated ? file
  }

  private[cluster] def truncateFile(file: Path, position: Long, keep: Boolean): Unit =
    autoClosing(new RandomAccessFile(file.toFile, "rw")) { f =>
      val channel = f.getChannel
      assertCutLineEnd(file, position, channel)
      if (keep) {
        copyTruncatedRest(file, position, channel)
      } else {
        logTruncatedRest(file, position, f)
      }
      channel.truncate(position)
    }

  private def assertCutLineEnd(file: Path, position: Long, in: FileChannel): Unit = {
    val buffer = ByteBuffer.allocate(1)
    in.position(position - 1)
    in.read(buffer)
    buffer.flip()
    if (!buffer.hasRemaining || buffer.get() != '\n')
      sys.error(s"Invalid failed-over position=$position in '${file.getFileName} journal file")
  }

  /** Save the truncated part for debugging. */
  private def copyTruncatedRest(file: Path, position: Long, channel: FileChannel): Unit =
    autoClosing(FileChannel.open(toTruncatedFilePath(file), WRITE, CREATE, TRUNCATE_EXISTING)) { out =>
      channel.position(position)
      val buffer = ByteBuffer.allocate(4096)
      buffer.clear()
      buffer.flip()
      var eof = false
      while (!eof) {
        if (buffer.hasRemaining) out.write(buffer)
        buffer.clear()
        eof = channel.read(buffer) <= 0
        buffer.flip()
      }
    }

  private def toTruncatedFilePath(file: Path): Path =
    Paths.get(s"$file~TRUNCATED-AFTER-FAILOVER")

  private def logTruncatedRest(file: Path, position: Long, f: RandomAccessFile): Unit = {
    f.seek(position)
    var logged = false

    @tailrec def loop(): Unit = {
      f.readLine() match {
        case null =>
        case line =>
          if (!logged) logger.info(s"Records truncated off ${file.getFileName}:")
          logged = true
          logger.info(line)
          loop()
      }
    }

    loop()
  }
}
