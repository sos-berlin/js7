package com.sos.jobscheduler.master.tests

import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.TypedPath
import com.sos.jobscheduler.data.folder.FolderPath
import java.nio.file.Files.{createDirectories, createTempDirectory}
import java.nio.file.Path
import scala.collection.immutable._

/**
  * @author Joacim Zschimmer
  */
final class TestEnvironment(agentPaths: Seq[AgentPath]) extends HasCloser {

  private val dir = createTempDirectory("test-") withCloser deleteDirectoryRecursively

  closeOnError(closer) {
    createDirectories(masterDir / "config/live")
    for (agentPath ← agentPaths) {
      createDirectories(agentDir(agentPath) / "config/live")
    }
  }

  def xmlFile(path: TypedPath): Path =
    masterDir / "config/live" resolve path.xmlFile

  def masterDir: Path =
    dir / "master"

  def agentXmlFile(agentPath: AgentPath, path: TypedPath): Path =
    agentDir(agentPath) / "config/live" resolve path.xmlFile

  def agentDir(agentPath: AgentPath): Path = {
    require(FolderPath.parentOf(agentPath) == FolderPath.Root, "Directory layout is not suitable for nested Agent paths")
    dir / s"agents/${agentPath.withoutStartingSlash}"
  }
}
