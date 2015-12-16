package com.sos.scheduler.engine.agent.task

import com.google.common.base.StandardSystemProperty.LINE_SEPARATOR
import com.sos.scheduler.engine.agent.data.{ProcessKillScript, AgentTaskId}
import com.sos.scheduler.engine.agent.task.CrashKillScript._
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.{Logger, ScalaConcurrentHashMap}
import com.sos.scheduler.engine.common.utils.Exceptions.ignoreException
import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}
import java.nio.charset.Charset.defaultCharset
import java.nio.file.Files.{createFile, deleteIfExists, move}
import java.nio.file.Path
import java.nio.file.StandardCopyOption.REPLACE_EXISTING

/**
  * @author Joacim Zschimmer
  */
final class CrashKillScript(killScript: ProcessKillScript, file: Path) {

  private val taskIds = new ScalaConcurrentHashMap[AgentTaskId, Unit]

  deleteIfExists(file)
  createFile(file)

  def add(id: AgentTaskId) = {
    taskIds.put(id, ())
    ignoreException(logger.warn) {
      file.append(idToKillCommand(id), defaultCharset)
    }
  }

  def remove(id: AgentTaskId): Unit = {
    taskIds -= id
    ignoreException(logger.warn) {
      rewriteFile()
    }
  }

  private def rewriteFile(): Unit = {
    val tmp = file.getParent resolve s"~${file.getFileName}.tmp"
    autoClosing(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(tmp), defaultCharset))) { writer ⇒
      for (id ← taskIds.keysIterator) {
        writer.write(idToKillCommand(id))
      }
    }
    move(tmp, file, REPLACE_EXISTING)
  }

  private def idToKillCommand(id: AgentTaskId) = (killScript.toCommandArguments(id) mkString " ") + LineSeparator
}

object CrashKillScript {
  private val logger = Logger(getClass)
  private val LineSeparator = sys.props(LINE_SEPARATOR.key)
}
