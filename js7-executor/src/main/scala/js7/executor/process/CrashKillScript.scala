package js7.executor.process

import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter, Writer}
import java.nio.charset.Charset.defaultCharset
import java.nio.file.Files.{createFile, deleteIfExists, move}
import java.nio.file.Path
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import js7.base.utils.AutoClosing.autoClosing
import js7.common.log.LazyScalaLogger.AsLazyScalaLogger
import js7.common.process.Processes.Pid
import js7.common.scalautil.Logger
import js7.common.utils.Exceptions.ignoreException
import js7.data.job.TaskId
import js7.executor.configuration.ProcessKillScript
import js7.executor.process.CrashKillScript._
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class CrashKillScript(killScript: ProcessKillScript, file: Path)
extends AutoCloseable {

  private val tasks = new mutable.HashMap[TaskId, Entry]

  private object writer {
    private var w: Writer = null

    def write(id: TaskId, entry: Entry): Unit = {
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
      for ((taskId, e) <- tasks) logger.warn(s"CrashKillScript left with task $taskId $e")
    }
  }

  def add(id: TaskId, pid: Option[Pid]): Unit =
    add(id, Entry(id, pid))

  private def add(id: TaskId, entry: Entry): Unit =
    synchronized {
      tasks.put(id, entry)
      ignoreException(logger.asLazy.warn) {
        writer.write(id, entry)
      }
    }

  def remove(id: TaskId): Unit =
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

  private def idToKillCommand(id: TaskId, entry: Entry) = {
    val args = killScript.toCommandArguments(id, entry.pidOption)
    val cleanTail = args.tail collect { case CleanArgument(o) => o }
    ((s""""${args.head}"""" +: cleanTail) mkString " ") + LineSeparator
  }
}

object CrashKillScript {
  private val logger = Logger(getClass)
  private val LineSeparator = sys.props("line.separator")
  private val CleanArgument = "([A-Za-z0-9=,;:.+_/#-]*)".r      // No shell meta characters

  private case class Entry(taskId: TaskId, pidOption: Option[Pid]) {
    override def toString = s"${taskId.string} ${pidOption getOrElse "?"}".trim
  }

  private def open(path: Path, append: Boolean = false): Writer =
    new BufferedWriter(new OutputStreamWriter(new FileOutputStream(path.toFile, append), defaultCharset))
}
