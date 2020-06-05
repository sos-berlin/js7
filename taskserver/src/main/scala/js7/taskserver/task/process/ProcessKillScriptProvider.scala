package js7.taskserver.task.process

import js7.agent.data.ProcessKillScript
import js7.base.time.ScalaTime._
import js7.base.utils.HasCloser
import js7.common.log.LazyScalaLogger.AsLazyScalaLogger
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Logger
import js7.common.system.OperatingSystem._
import js7.common.time.WaitForCondition.waitForCondition
import js7.common.utils.Exceptions.ignoreException
import js7.common.utils.JavaResource
import js7.taskserver.task.process.ProcessKillScriptProvider._
import java.nio.file.Files.{delete, exists}
import java.nio.file.Path

/**
 * @author Joacim Zschimmer
 */
final class ProcessKillScriptProvider extends HasCloser
{
  def provideTo(directory: Path): ProcessKillScript = {
    val resource = if (isWindows) WindowsScriptResource else UnixScriptResource
    val file = directory / resource.simpleName
    val content = resource.contentBytes
    if (!isUnchanged(file, content)) {
      file := content
      file.makeExecutable()
      onClose {
        ignoreException(logger.asLazy.error) { delete(file) }
      }
    }

    ProcessKillScript(file)
  }

  /**
    * For parallel running integration tests.
    * Under production, the function should return immediately with `true`.
    */
  private def isUnchanged(file: Path, content: Array[Byte]): Boolean = {
    waitForCondition(1.s, 100.ms) {
      val okay = !exists(file) || file.contentBytes.sameElements(content)
      if (!okay) logger.debug("Kill script has been written concurrently")
      okay
    }
    exists(file) && file.contentBytes.sameElements(content)
  }
}

object ProcessKillScriptProvider {
  private val logger = Logger(getClass)
  private val WindowsScriptResource = JavaResource("js7/taskserver/task/process/scripts/windows/kill_task.cmd")
  private val UnixScriptResource = JavaResource("js7/taskserver/task/process/scripts/unix/kill_task.sh")
}
