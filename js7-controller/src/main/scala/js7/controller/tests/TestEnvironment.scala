package js7.controller.tests

import java.io.IOException
import java.nio.file.Files.{createDirectories, createDirectory, deleteIfExists, exists}
import java.nio.file.Path
import js7.common.scalautil.FileUtils.deleteDirectoryContentRecursively
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Logger
import js7.controller.tests.TestEnvironment._
import js7.data.agent.AgentRefPath
import js7.data.folder.FolderPath
import js7.data.item.{SourceType, TypedPath}

/**
  * @author Joacim Zschimmer
  */
final class TestEnvironment(agentRefPaths: Seq[AgentRefPath], temporaryDirectory: Path)
extends AutoCloseable {

  if (exists(temporaryDirectory)) {
    logger.warn(s"Deleting $temporaryDirectory")
    deleteDirectoryContentRecursively(temporaryDirectory)
  }

  createDirectories(controllerDir / "config/private")
  createDirectories(controllerDir / "data")
  for (agentRefPath <- agentRefPaths) {
    createDirectories(agentDir(agentRefPath) / "config/private")
    createDirectories(agentDir(agentRefPath) / "config/executables")
    createDirectory(agentDir(agentRefPath) / "data")
  }

  def close(): Unit = {
    deleteDirectoryContentRecursively(temporaryDirectory)
    try deleteIfExists(temporaryDirectory)
    catch { case t: IOException => logger.debug(s"Delete $temporaryDirectory: $t", t)}
  }

   def controllerDir: Path =
    temporaryDirectory / "controller"

  def agentFile(agentRefPath: AgentRefPath, path: TypedPath, t: SourceType): Path =
    agentDir(agentRefPath) / "config/live" resolve path.toFile(t)

  def agentDir(agentRefPath: AgentRefPath): Path = {
    require(FolderPath.parentOf(agentRefPath) == FolderPath.Root, "Directory layout is not suitable for nested AgentRef paths")
    agentsDir / agentRefPath.withoutStartingSlash
  }

  def agentsDir = temporaryDirectory / "agents"
}

object TestEnvironment {
  private val logger = Logger(getClass)
}
