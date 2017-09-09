package com.sos.jobscheduler.taskserver.task.process

import com.sos.jobscheduler.agent.data.ProcessKillScript
import com.sos.jobscheduler.common.log.LazyScalaLogger.AsLazyScalaLogger
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.{HasCloser, Logger}
import com.sos.jobscheduler.common.system.OperatingSystem._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.common.utils.Exceptions.ignoreException
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.taskserver.task.process.ProcessKillScriptProvider._
import java.nio.file.Files.{delete, exists, setPosixFilePermissions}
import java.nio.file.Path
import java.nio.file.attribute.PosixFilePermission.{OWNER_EXECUTE, OWNER_READ}
import scala.collection.JavaConverters._

/**
 * @author Joacim Zschimmer
 */
final class ProcessKillScriptProvider extends HasCloser {

  def provideTo(directory: Path): ProcessKillScript = {
    val resource = if (isWindows) WindowsScriptResource else UnixScriptResource
    val file = directory / resource.simpleName
    val content = resource.contentBytes
    if (!isOkay(file, content)) {
      file.contentBytes = content
      onClose {
        ignoreException(logger.asLazy.error) { delete(file) }
      }
      if (isUnix)  {
        setPosixFilePermissions(file, Set(OWNER_READ, OWNER_EXECUTE).asJava)
      }
    }

    ProcessKillScript(file)
  }

  /**
    * For parallel running integration tests.
    */
  private def isOkay(file: Path, content: Array[Byte]): Boolean = {
    waitForCondition(1.s, 100.ms) { !exists(file) || file.contentBytes.sameElements(content)}
    exists(file) && file.contentBytes.sameElements(content)
  }
}

object ProcessKillScriptProvider {
  private val logger = Logger(getClass)
  private val WindowsScriptResource = JavaResource("com/sos/jobscheduler/taskserver/task/process/scripts/windows/kill_task.cmd")
  private val UnixScriptResource = JavaResource("com/sos/jobscheduler/taskserver/task/process/scripts/unix/kill_task.sh")
}
