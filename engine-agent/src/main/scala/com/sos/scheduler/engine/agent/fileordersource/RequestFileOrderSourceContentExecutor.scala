package com.sos.scheduler.engine.agent.fileordersource

import com.sos.scheduler.engine.agent.data.commandresponses.FileOrderSourceContent
import com.sos.scheduler.engine.agent.data.commands.RequestFileOrderSourceContent
import com.sos.scheduler.engine.agent.fileordersource.RequestFileOrderSourceContentExecutor._
import com.sos.scheduler.engine.common.scalautil.AutoClosing._
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.nio.file.Files._
import java.nio.file._
import java.time.Instant.now
import javax.inject.{Inject, Singleton}
import scala.concurrent._
import scala.util.control.NonFatal
import scala.util.matching.Regex

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class RequestFileOrderSourceContentExecutor @Inject private(implicit ec: ExecutionContext) extends HasCloser {

  def apply(command: RequestFileOrderSourceContent) = Future[FileOrderSourceContent] {
    autoClosing(new SingleCommandExecutor(command)) { _.apply() }
  }
}

object RequestFileOrderSourceContentExecutor {
  private val logger = Logger(getClass)

  private class SingleCommandExecutor(command: RequestFileOrderSourceContent) extends HasCloser {
    private val regex = new Regex(command.regex)
    private val directory = Paths.get(command.directory)
    private val blockingDirectoryWatcher = new BlockingDirectoryWatcher(Paths.get(command.directory), pathMatches).closeWithCloser
    private val until = now() + command.duration

    def apply(): FileOrderSourceContent =
      getFileOrderSourceContent(command) match {
        case o if !o.isEmpty ⇒ o
        case _ ⇒
          try blockingDirectoryWatcher.waitForMatchingDirectoryChange(until)
          catch { case NonFatal(t) ⇒
            t match {
              case t: UnsupportedOperationException ⇒ logger.debug(t.toString)  // File system does not support notification
              case _ ⇒ logger.warn(t.toString, t)
            }
            val remaining = until - now()
            if (remaining > 0.s) {
              blocking { sleep(remaining) }
            }
          }
          getFileOrderSourceContent(command)
      }

    private def getFileOrderSourceContent(command: RequestFileOrderSourceContent): FileOrderSourceContent = {
      val entries = autoClosing(Files.list(directory)) { javaStream ⇒
        (javaStream.toIterator filter pathMatches flatMap toEntryOption).toVector
      }
      FileOrderSourceContent(entries sortBy { _.lastModifiedTime })
    }

    private def toEntryOption(file: Path): Option[FileOrderSourceContent.Entry] =
      try Some(FileOrderSourceContent.Entry(file.toString, getLastModifiedTime(file).toMillis))
      catch { case _: NoSuchFileException ⇒ None }

    private def pathMatches(path: Path): Boolean =
      regex.findFirstIn(path.getFileName.toString).isDefined && !command.knownFiles(path.toString)
  }
}
