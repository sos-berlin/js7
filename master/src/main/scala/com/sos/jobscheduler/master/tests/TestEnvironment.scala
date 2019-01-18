package com.sos.jobscheduler.master.tests

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryContentRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.{SourceType, TypedPath}
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.master.scheduledorder.ScheduledOrderGeneratorPath
import com.sos.jobscheduler.master.tests.TestEnvironment._
import io.circe.{Json, ObjectEncoder}
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

  createDirectories(masterDir / "config/order-generators")
  createDirectories(masterDir / "config/private")
  createDirectories(masterDir / "data")
  for (agentPath ← agentPaths) {
    createDirectories(agentDir(agentPath) / "config/private")
    createDirectories(agentDir(agentPath) / "config/executables")
    createDirectory(agentDir(agentPath) / "data")
  }

  def close(): Unit = {
    deleteDirectoryContentRecursively(temporaryDirectory)
    try deleteIfExists(temporaryDirectory)
    catch { case t: IOException ⇒ logger.debug(s"Delete $temporaryDirectory: $t", t)}
  }

  def writeJson[A: ObjectEncoder](path: TypedPath, a: A): Unit =
    file(path, SourceType.Json).contentString = Json.fromJsonObject(implicitly[ObjectEncoder[A]].encodeObject(a)).toPrettyString

  def writeTxt(path: TypedPath, content: String): Unit =
    file(path, SourceType.Txt).contentString = content

  def file(path: TypedPath, t: SourceType): Path = {
    val dir = if (path.companion == ScheduledOrderGeneratorPath) "order-generators" else "live"
    masterDir / "config" / dir resolve path.toFile(t)
  }

  def masterPrivateConf = masterDir / "config/private/private.conf"

  def masterDir: Path =
    temporaryDirectory / "master"

  def agentFile(agentPath: AgentPath, path: TypedPath, t: SourceType): Path =
    agentDir(agentPath) / "config/live" resolve path.toFile(t)

  def agentDir(agentPath: AgentPath): Path = {
    require(FolderPath.parentOf(agentPath) == FolderPath.Root, "Directory layout is not suitable for nested Agent paths")
    agentsDir / agentPath.withoutStartingSlash
  }

  def agentsDir = temporaryDirectory / "agents"
}

object TestEnvironment {
  private val logger = Logger(getClass)
}
