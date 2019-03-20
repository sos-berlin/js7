package com.sos.jobscheduler.agent.scheduler.job

import com.sos.jobscheduler.agent.scheduler.job.FilePool._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.job.JobKey
import com.sos.jobscheduler.taskserver.task.process.RichProcess.tryDeleteFiles
import java.nio.file.Path
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private[job] final class FilePool(jobKey: JobKey, temporaryDirectory: Path) extends AutoCloseable {

  private var free = List[FileSet]()
  private val used = mutable.Set[FileSet]()

  def get(): FileSet = {
    val fileSet = free match {
      case head :: tail =>
        free = tail
        head.clear()
        head
      case Nil =>
        val fileSet = FileSet(new ShellReturnValuesProvider(temporaryDirectory))
        logger.debug(s"Job '${jobKey.keyName}': Using file ${fileSet.shellReturnValuesProvider.file} for order keyValues")
        fileSet
    }
    used += fileSet
    fileSet
  }

  def release(fileSet: FileSet): Unit = {
    val removed = used.remove(fileSet)
    require(removed, s"Job '${jobKey.keyName}': Releasing unknown FileSet")
    free = fileSet :: free
  }

  def close() = {
    if (used.nonEmpty) {
      logger.debug(s"Job '${jobKey.keyName}': Closing while files are in use: $used")
    }
    tryDeleteFiles((free ++ used) flatMap { _.files })
  }

  override def toString = s"FilePool(Job '${jobKey.keyName}' ${used.size} used and ${free.size} free file sets)"
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
