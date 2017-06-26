package com.sos.jobscheduler.agent.fileordersource

import com.sos.jobscheduler.agent.data.commandresponses.EmptyResponse
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{DeleteFile, FileCommand, MoveFile}
import java.nio.file.Files._
import java.nio.file.StandardCopyOption._
import java.nio.file.{Files, Paths}

/**
 * @author Joacim Zschimmer
 */
object FileCommandExecutor {

  def executeCommand(command: FileCommand): EmptyResponse.type =
    command match {
      case DeleteFile(path) ⇒
        Files.delete(Paths.get(path))
        EmptyResponse
      case MoveFile(pathString, toDirectoryString) ⇒
        val path = Paths.get(pathString)
        val toDirectory = Paths.get(toDirectoryString)
        require(isDirectory(toDirectory), s"Move destination is not a directory: $toDirectory")
        Files.move(path, toDirectory resolve path.getFileName, REPLACE_EXISTING)
        EmptyResponse
    }
}
