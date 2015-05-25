package com.sos.scheduler.engine.agent.fileordersource

import com.sos.scheduler.engine.agent.data.commands.{FileOrderSourceContent, RequestFileOrderSourceContent}
import com.sos.scheduler.engine.common.scalautil.AutoClosing._
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import java.nio.file.Files._
import java.nio.file.{Files, NoSuchFileException, Path, Paths}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._

/**
 * @author Joacim Zschimmer
 */
object RequestFileOrderSourceContentExecutor {

  def apply(command: RequestFileOrderSourceContent): Future[FileOrderSourceContent] =
    Future {
      getFileOrderSourceContent(command) match {
        case o if o.files.nonEmpty ⇒ o
        case _ ⇒
          blocking {
            Thread.sleep(command.durationMillis) // Blocks a thread!!!
          }
          getFileOrderSourceContent(command)
      }
    }

  private def getFileOrderSourceContent(request: RequestFileOrderSourceContent): FileOrderSourceContent = {
    val regex = request.regex match {
      case "" ⇒ ".*".r
      case o ⇒ o.r
    }
    val entries = autoClosing(Files.list(Paths.get(request.directory))) { javaStream ⇒
      (javaStream.toIterator
        filter { f ⇒ regex.findFirstIn(f.getFileName.toString).isDefined && !request.knownFiles(f.toString) }
        flatMap toEntryOption
        ).toVector
    }
    FileOrderSourceContent(entries sortBy { _.lastModifiedTime })
  }

  private def toEntryOption(file: Path): Option[FileOrderSourceContent.Entry] =
    try Some(FileOrderSourceContent.Entry(file.toString, getLastModifiedTime(file).toMillis))
    catch { case _: NoSuchFileException ⇒ None }
}
