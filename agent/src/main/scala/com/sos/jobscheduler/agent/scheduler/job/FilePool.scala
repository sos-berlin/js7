package js7.agent.scheduler.job

import js7.agent.scheduler.job.FilePool._
import js7.common.scalautil.Logger
import js7.data.job.JobKey
import js7.taskserver.task.process.RichProcess.tryDeleteFiles
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
        logger.trace(s"Job '${jobKey.keyName}': Using file ${fileSet.shellReturnValuesProvider.file} for order keyValues")
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
