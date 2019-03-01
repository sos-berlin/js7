package com.sos.jobscheduler.master.tests

import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryContentRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.filebased.{SourceType, TypedPath}
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.master.tests.TestEnvironment._
import java.io.IOException
import java.nio.file.Files.{createDirectories, createDirectory, deleteIfExists, exists}
import java.nio.file.Path
import scala.collection.immutable._

/**
  * @author Joacim Zschimmer
  */
final class TestEnvironment(agentRefPaths: Seq[AgentRefPath], temporaryDirectory: Path)
extends AutoCloseable {

  if (exists(temporaryDirectory)) {
    logger.warn(s"Deleting $temporaryDirectory")
    deleteDirectoryContentRecursively(temporaryDirectory)
  }

  createDirectories(masterDir / "config/private")
  createDirectories(masterDir / "data")
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

   def masterDir: Path =
    temporaryDirectory / "master"

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
