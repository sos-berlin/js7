package com.sos.jobscheduler.master.tests

import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryContentRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.TypedPath
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.master.tests.TestEnvironment._
import java.io.IOException
import java.nio.file.Files.{createDirectories, createDirectory, deleteIfExists, exists}
import java.nio.file.Path
import scala.collection.immutable._

/**
  * @author Joacim Zschimmer
  */
final class TestEnvironment(agentPaths: Seq[AgentPath], temporaryDirectory: Path)
extends AutoCloseable {

  if (exists(temporaryDirectory)) {
    logger.warn(s"Deleting $temporaryDirectory")
    deleteDirectoryContentRecursively(temporaryDirectory)
  }

  createDirectories(masterDir / "config/live")
  for (agentPath ← agentPaths) {
    createDirectories(agentDir(agentPath) / "config/live")
    createDirectory(agentDir(agentPath) / "data")
  }

  def close(): Unit = {
    deleteDirectoryContentRecursively(temporaryDirectory)
    try deleteIfExists(temporaryDirectory)
    catch { case t: IOException ⇒ logger.debug(s"Delete $temporaryDirectory: $t", t)}
  }

  def xmlFile(path: TypedPath): Path =
    masterDir / "config/live" resolve path.xmlFile

  def jsonFile(path: TypedPath): Path =
    masterDir / "config/live" resolve path.jsonFile

  def masterDir: Path =
    temporaryDirectory / "master"

  def agentXmlFile(agentPath: AgentPath, path: TypedPath): Path =
    agentDir(agentPath) / "config/live" resolve path.xmlFile

  def agentDir(agentPath: AgentPath): Path = {
    require(FolderPath.parentOf(agentPath) == FolderPath.Root, "Directory layout is not suitable for nested Agent paths")
    temporaryDirectory / s"agents/${agentPath.withoutStartingSlash}"
  }
}

object TestEnvironment {
  private val logger = Logger(getClass)
}
