package js7.controller.tests

import java.io.IOException
import java.nio.file.Files.{createDirectories, createDirectory, deleteIfExists, exists}
import java.nio.file.Path
import js7.common.scalautil.FileUtils.deleteDirectoryContentRecursively
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Logger
import js7.controller.tests.TestEnvironment._
import js7.data.agent.AgentName
import js7.data.item.{SourceType, ItemPath}

/**
  * @author Joacim Zschimmer
  */
final class TestEnvironment(agentNames: Seq[AgentName], temporaryDirectory: Path)
extends AutoCloseable {

  if (exists(temporaryDirectory)) {
    logger.warn(s"Deleting $temporaryDirectory")
    deleteDirectoryContentRecursively(temporaryDirectory)
  }

  createDirectories(controllerDir / "config/private")
  createDirectories(controllerDir / "data")
  for (agentName <- agentNames) {
    createDirectories(agentDir(agentName) / "config/private")
    createDirectories(agentDir(agentName) / "config/executables")
    createDirectory(agentDir(agentName) / "data")
  }

  def close(): Unit = {
    deleteDirectoryContentRecursively(temporaryDirectory)
    try deleteIfExists(temporaryDirectory)
    catch { case t: IOException => logger.debug(s"Delete $temporaryDirectory: $t", t)}
  }

   def controllerDir: Path =
    temporaryDirectory / "controller"

  def agentFile(agentName: AgentName, path: ItemPath, t: SourceType): Path =
    agentDir(agentName) / "config/live" resolve path.toFile(t)

  def agentDir(name: AgentName): Path =
    agentsDir / name.string

  def agentsDir = temporaryDirectory / "agents"
}

object TestEnvironment {
  private val logger = Logger(getClass)
}
