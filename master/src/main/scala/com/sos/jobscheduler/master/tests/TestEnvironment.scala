package com.sos.jobscheduler.master.tests

import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.TypedPath
import com.sos.jobscheduler.data.folder.FolderPath
import java.nio.file.Files.{createDirectories, createDirectory}
import java.nio.file.Path
import scala.collection.immutable._

/**
  * @author Joacim Zschimmer
  */
final class TestEnvironment(agentPaths: Seq[AgentPath], temporaryDirectory: Path)
extends AutoCloseable {

  createDirectories(masterDir / "config/live")
  for (agentPath ← agentPaths) {
    createDirectories(agentDir(agentPath) / "config/live")
    createDirectory(agentDir(agentPath) / "data")
  }

  def close(): Unit = {
    deleteDirectoryRecursively(temporaryDirectory)
  }

  def xmlFile(path: TypedPath): Path =
    masterDir / "config/live" resolve path.xmlFile

  def masterDir: Path =
    temporaryDirectory / "master"

  def agentXmlFile(agentPath: AgentPath, path: TypedPath): Path =
    agentDir(agentPath) / "config/live" resolve path.xmlFile

  def agentDir(agentPath: AgentPath): Path = {
    require(FolderPath.parentOf(agentPath) == FolderPath.Root, "Directory layout is not suitable for nested Agent paths")
    temporaryDirectory / s"agents/${agentPath.withoutStartingSlash}"
  }
}
