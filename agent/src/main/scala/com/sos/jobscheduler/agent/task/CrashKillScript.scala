package com.sos.scheduler.engine.agent.task

import com.google.common.base.StandardSystemProperty.LINE_SEPARATOR
import com.sos.scheduler.engine.agent.data.{AgentTaskId, ProcessKillScript}
import com.sos.scheduler.engine.agent.task.CrashKillScript._
import com.sos.scheduler.engine.common.log.LazyScalaLogger.AsLazyScalaLogger
import com.sos.scheduler.engine.common.process.Processes.Pid
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.utils.Exceptions.ignoreException
import com.sos.scheduler.engine.data.job.TaskId
import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}
import java.nio.charset.Charset.defaultCharset
import java.nio.file.Files.{createFile, deleteIfExists, move}
import java.nio.file.Path
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class CrashKillScript(killScript: ProcessKillScript, file: Path) {

  private val tasks = new mutable.HashMap[AgentTaskId, (String, TaskId, Option[Pid])]

  deleteIfExists(file)
  createFile(file)

  def add(id: AgentTaskId, pid: Option[Pid], taskId: TaskId, jobPath: String): Unit =
    synchronized {
      tasks.put(id, (jobPath, taskId, pid))
      ignoreException(logger.asLazy.warn) {
        file.append(idToKillCommand(id, pid, jobPath = jobPath, taskId), defaultCharset)
      }
    }

  def remove(id: AgentTaskId): Unit =
    synchronized {
      for (_ ← tasks.remove(id)) {
        ignoreException(logger.asLazy.warn) {
          rewriteFile()
        }
      }
    }

  private def rewriteFile(): Unit =
    if (tasks.isEmpty) {
      deleteIfExists(file)
    } else {
      val tmp = file.getParent resolve s"~${file.getFileName}.tmp"
      autoClosing(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(tmp), defaultCharset))) { writer ⇒
        for ((id, (jobPath, taskId, pid)) ← tasks) {
          writer.write(idToKillCommand(id, pid, jobPath = jobPath, taskId))
        }
      }
      move(tmp, file, REPLACE_EXISTING)
    }

  private def idToKillCommand(id: AgentTaskId, pid: Option[Pid], jobPath: String, taskId: TaskId) = {
    val args = killScript.toCommandArguments(id, pid, jobPath = jobPath, taskId)
    val cleanTail = args.tail collect { case CleanArgument(o) ⇒ o }
    ((s""""${args.head}"""" +: cleanTail) mkString " ") + LineSeparator
  }
}

object CrashKillScript {
  private val logger = Logger(getClass)
  private val LineSeparator = sys.props(LINE_SEPARATOR.key)
  private val CleanArgument = "([A-Za-z0-9=,;:.+_/#-]*)".r      // No shell meta characters
}
