package com.sos.jobscheduler.agent.scheduler.job

import com.sos.jobscheduler.agent.scheduler.job.FilePool._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.taskserver.task.process.RichProcess.tryDeleteFiles
import java.nio.file.Path
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private[job] final class FilePool(jobConfiguration: JobConfiguration) extends AutoCloseable {

  import jobConfiguration.{taskLimit, path ⇒ jobPath}

  private var free = List[FileSet]()
  private val used = mutable.Set[FileSet]()

  def get(): FileSet = {
    val fileSet = free match {
      case head :: tail ⇒
        free = tail
        head.clear()
        head
      case Nil ⇒
        if (used.size == taskLimit) throw new IllegalStateException(s"FilePool taskLimit=$taskLimit exceeded: ${used.size}")
        val fileSet = FileSet(new ShellReturnValuesProvider)
        logger.debug(s"$jobPath: Using file ${fileSet.shellReturnValuesProvider.file} for order variables")
        fileSet
    }
    used += fileSet
    fileSet
  }

  def release(fileSet: FileSet): Unit = {
    val removed = used.remove(fileSet)
    require(removed, "Releasing unknown FileSet")
    free = fileSet :: free
  }

  def close() = {
    if (used.nonEmpty) {
      logger.debug(s"$jobPath: Closing while files are in use: $used")
    }
    tryDeleteFiles((free ++ used) flatMap { _.files })
  }

  override def toString = s"FilePool($jobPath ${used.size} used and ${free.size} free file sets)"
}

private[job] object FilePool {
  private val logger = Logger(getClass)

  final case class FileSet(shellReturnValuesProvider: ShellReturnValuesProvider) {
    def clear() = {
      shellReturnValuesProvider.clear()
    }

    def files: Seq[Path] =
      shellReturnValuesProvider.file :: Nil
  }
}
