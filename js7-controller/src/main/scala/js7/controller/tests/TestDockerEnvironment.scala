package js7.controller.tests

import java.io.IOException
import java.nio.file.Files.{createDirectories, createDirectory, deleteIfExists, exists}
import java.nio.file.Path
import js7.base.io.file.FileUtils.deleteDirectoryContentRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.controller.tests.TestDockerEnvironment.*
import js7.data.agent.AgentPath
import js7.data.item.{SourceType, VersionedItemPath}

/**
  * @author Joacim Zschimmer
  */
final class TestDockerEnvironment(agentPaths: Seq[AgentPath], temporaryDirectory: Path)
extends AutoCloseable:

  if exists(temporaryDirectory) then
    logger.warn(s"Deleting $temporaryDirectory")
    deleteDirectoryContentRecursively(temporaryDirectory)

  createDirectories(controllerDir / "config/private")
  createDirectories(controllerDir / "data")
  for agentPath <- agentPaths do
    createDirectories(agentDir(agentPath) / "config/private")
    createDirectories(agentDir(agentPath) / "config/executables")
    createDirectory(agentDir(agentPath) / "data")

  def close(): Unit =
    deleteDirectoryContentRecursively(temporaryDirectory)
    try deleteIfExists(temporaryDirectory)
    catch { case t: IOException => logger.debug(s"Delete $temporaryDirectory: $t", t)}

  def controllerDir: Path =
    temporaryDirectory / "controller"

  def agentFile(agentPath: AgentPath, path: VersionedItemPath, t: SourceType): Path =
    agentDir(agentPath) / "config/live" resolve path.toFile(t)

  def agentDir(name: AgentPath): Path =
    agentsDir / name.string

  def agentsDir: Path =
    temporaryDirectory / "agents"


object TestDockerEnvironment:
  private val logger = Logger[this.type]
