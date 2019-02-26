package com.sos.jobscheduler.agent.task

import com.google.common.base.StandardSystemProperty.LINE_SEPARATOR
import com.sos.jobscheduler.agent.data.{AgentTaskId, ProcessKillScript}
import com.sos.jobscheduler.agent.task.CrashKillScript._
import com.sos.jobscheduler.common.log.LazyScalaLogger.AsLazyScalaLogger
import com.sos.jobscheduler.common.process.Processes.Pid
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.Exceptions.ignoreException
import com.sos.jobscheduler.data.job.TaskId
import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter, Writer}
import java.nio.charset.Charset.defaultCharset
import java.nio.file.Files.{createFile, deleteIfExists, move}
import java.nio.file.Path
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class CrashKillScript(killScript: ProcessKillScript, file: Path)
extends AutoCloseable {

  private val tasks = new mutable.HashMap[AgentTaskId, Entry]

  private object writer {
    private var w: Writer = null

    def write(id: AgentTaskId, entry: Entry): Unit = {
      if (w == null) {
        w = open(file, append = true)
      }
      w.write(idToKillCommand(id, entry))
      w.flush()
    }

    def close(): Unit =
      if (w != null) {
        w.close()
        w = null
      }
  }

  deleteIfExists(file)
  createFile(file)

  def close(): Unit = {
    writer.close()
    if (tasks.isEmpty) {
      ignoreException(logger.asLazy.warn) {
        deleteIfExists(file)
      }
    } else {
      for ((agentTaskId, e) <- tasks) logger.warn(s"CrashKillScript left with task $agentTaskId $e")
    }
  }

  def add(id: AgentTaskId, pid: Option[Pid], taskId: TaskId): Unit =
    add(id, Entry(taskId, pid))

  private def add(id: AgentTaskId, entry: Entry): Unit =
    synchronized {
      tasks.put(id, entry)
      ignoreException(logger.asLazy.warn) {
        writer.write(id, entry)
      }
    }

  def remove(id: AgentTaskId): Unit =
    synchronized {
      for (_ <- tasks.remove(id)) {
        ignoreException(logger.asLazy.warn) {
          rewriteFile()
        }
      }
    }

  private def rewriteFile(): Unit = {
    writer.close()
    if (tasks.isEmpty) {
      deleteIfExists(file)
    } else {
      val tmp = file.getParent resolve s"~${file.getFileName}.tmp"
      autoClosing(open(tmp)) { writer =>
        for ((id, entry) <- tasks) {
          writer.write(idToKillCommand(id, entry))
        }
      }
      move(tmp, file, REPLACE_EXISTING)
    }
  }

  private def idToKillCommand(id: AgentTaskId, entry: Entry) = {
    val args = killScript.toCommandArguments(id, entry.pidOption, entry.taskId)
    val cleanTail = args.tail collect { case CleanArgument(o) => o }
    ((s""""${args.head}"""" +: cleanTail) mkString " ") + LineSeparator
  }
}

object CrashKillScript {
  private val logger = Logger(getClass)
  private val LineSeparator = sys.props(LINE_SEPARATOR.key)
  private val CleanArgument = "([A-Za-z0-9=,;:.+_/#-]*)".r      // No shell meta characters

  private case class Entry(taskId: TaskId, pidOption: Option[Pid]) {
    override def toString = s"$taskId ${pidOption getOrElse ""}".trim
  }

  private def open(path: Path, append: Boolean = false): Writer =
    new BufferedWriter(new OutputStreamWriter(new FileOutputStream(path, append), defaultCharset))
}
