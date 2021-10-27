package js7.launcher.process

import java.io.IOException
import java.nio.file.Files.{delete, exists, setPosixFilePermissions}
import java.nio.file.Path
import java.nio.file.attribute.PosixFilePermissions
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax._
import js7.base.log.LazyScalaLogger.AsLazyScalaLogger
import js7.base.log.Logger
import js7.base.system.OperatingSystem.{isUnix, isWindows}
import js7.base.time.ScalaTime._
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.HasCloser
import js7.common.utils.Exceptions.ignoreException
import js7.launcher.configuration.ProcessKillScript
import js7.launcher.process.ProcessKillScriptProvider._

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
      if (isUnix) {
        try setPosixFilePermissions(file, PosixFilePermissions.fromString("rw-------"))
        catch { case _: IOException => }
      }
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
  private val WindowsScriptResource = JavaResource("js7/launcher/process/scripts/windows/kill_task.cmd")
  private val UnixScriptResource = JavaResource("js7/launcher/process/scripts/unix/kill_task.sh")
}
