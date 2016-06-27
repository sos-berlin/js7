package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.agent.data.ProcessKillScript
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.common.utils.Exceptions.ignoreException
import com.sos.scheduler.engine.common.utils.JavaResource
import com.sos.scheduler.engine.taskserver.task.process.ProcessKillScriptProvider._
import java.nio.file.Files.{delete, exists, setPosixFilePermissions}
import java.nio.file.Path
import java.nio.file.attribute.PosixFilePermission.{OWNER_EXECUTE, OWNER_READ}
import scala.collection.JavaConversions._

/**
 * @author Joacim Zschimmer
 */
final class ProcessKillScriptProvider extends HasCloser {

  def provideTo(directory: Path): ProcessKillScript = {
    val resource = if (isWindows) WindowsScriptResource else UnixScriptResource
    val file = directory / resource.simpleName
    val content = resource.contentBytes
    if (!exists(file) || !file.contentBytes.sameElements(content)) {
      file.contentBytes = content
      onClose {
        ignoreException(logger.error) { delete(file) }
      }
      if (isUnix)  {
        setPosixFilePermissions(file, Set(OWNER_READ, OWNER_EXECUTE))
      }
    }

    ProcessKillScript(file)
  }
}

object ProcessKillScriptProvider {
  private val logger = Logger(getClass)
  private val WindowsScriptResource = JavaResource("com/sos/scheduler/engine/taskserver/task/process/scripts/windows/kill_task.cmd")
  private val UnixScriptResource = JavaResource("com/sos/scheduler/engine/taskserver/task/process/scripts/unix/kill_task.sh")
}
