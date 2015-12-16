package com.sos.scheduler.engine.taskserver.task.process

import com.google.common.io.Files.asByteSink
import com.google.common.io.Resources
import com.sos.scheduler.engine.agent.data.ProcessKillScript
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.common.utils.Exceptions.ignoreException
import com.sos.scheduler.engine.common.utils.JavaResource
import com.sos.scheduler.engine.taskserver.task.process.ProcessKillScriptProvider._
import java.nio.file.Files.{createFile, delete, deleteIfExists}
import java.nio.file.Path

/**
 * @author Joacim Zschimmer
 */
final class ProcessKillScriptProvider extends HasCloser {

  def provideTo(directory: Path): ProcessKillScript = {
    val resource = if (isWindows) WindowsScriptResource else UnixScriptResource
    val file = directory / resource.simpleName
    deleteIfExists(file)
    createFile(file, Processes.shellFileAttributes: _*)
    Resources.asByteSource(resource.url) copyTo asByteSink(file)
    onClose {
      ignoreException(logger.error) { delete(file) }
    }
    ProcessKillScript(file)
  }
}

object ProcessKillScriptProvider {
  private val logger = Logger(getClass)
  private val WindowsScriptResource = JavaResource("com/sos/scheduler/engine/taskserver/task/process/scripts/windows/jobscheduler_agent_kill_task.cmd")
  private val UnixScriptResource = JavaResource("com/sos/scheduler/engine/taskserver/task/process/scripts/unix/jobscheduler_agent_kill_task.sh")
}
