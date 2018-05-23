package com.sos.jobscheduler.tests

import akka.http.scaladsl.model.Uri
import com.google.inject.Module
import com.google.inject.util.Modules.EMPTY_MODULE
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.scheduler.job.{JobConfiguration, JobScript}
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.common.scalautil.AutoClosing.{closeOnError, multipleAutoClosing}
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.data.filebased.{FileBased, SourceType, TypedPath}
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.tests.DirectoryProvider._
import io.circe.syntax.EncoderOps
import io.circe.{Json, ObjectEncoder}
import java.nio.file.Files.createTempDirectory
import java.nio.file.{Files, Path}
import java.time.Duration
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.BeforeAndAfterAll
import scala.collection.immutable.{IndexedSeq, Seq}
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
final class DirectoryProvider(agentPaths: Seq[AgentPath]) extends HasCloser {

  val directory = createTempDirectory("test-") withCloser deleteDirectoryRecursively
  val master = new Tree(directory / "master")
  val agentToTree: Map[AgentPath, AgentTree] = agentPaths.map { o ⇒ o → new AgentTree(directory, o) }.toMap
  val agents = agentPaths.toVector map agentToTree
  closeOnError(this) {
    master.createDirectories()
    for (a ← agentToTree.values) {
      a.createDirectories()
      val file = master.fileBasedDirectory / s"${a.agentPath.withoutStartingSlash}.agent.json"
      Files.createDirectories(file.getParent)
      file.contentString = Agent(AgentPath.NoId, uri = a.conf.localUri.toString).asJson.toPrettyString
    }
  }

  def run(body: (RunningMaster, IndexedSeq[RunningAgent]) ⇒ Unit)(implicit s: Scheduler): Unit =
    runAgents()(agents ⇒
      runMaster()(master ⇒
        body(master, agents)))

  def runMaster(eventCollector: Option[TestEventCollector] = None)(body: RunningMaster ⇒ Unit)(implicit s: Scheduler): Unit =
    RunningMaster.runForTest(directory, eventCollector)(body)

  def startMaster(module: Module = EMPTY_MODULE)(implicit s: Scheduler): Task[RunningMaster] =
    RunningMaster(RunningMaster.newInjector(directory, module))

  def runAgents()(body: IndexedSeq[RunningAgent] ⇒ Unit)(implicit ec: ExecutionContext): Unit =
    multipleAutoClosing(agents map (_.conf) map RunningAgent.startForTest await 10.s) { agents ⇒
      body(agents)
      agents map (_.terminate()) await 99.s
    }

  def startAgents()(implicit ec: ExecutionContext): Future[Seq[RunningAgent]] =
    Future.sequence(agents map (_.conf) map RunningAgent.startForTest)

  def agent(agentName: String) = new Tree(directory / agentName)
}

object DirectoryProvider {
  trait ForScalaTest extends BeforeAndAfterAll with HasCloser {
    this: org.scalatest.Suite ⇒

    protected def agentPaths: Seq[AgentPath]

    import Scheduler.Implicits.global

    protected lazy val directoryProvider = new DirectoryProvider(agentPaths)
    protected lazy val agents: Seq[RunningAgent] = directoryProvider.startAgents() await 99.s
    protected lazy val agent: RunningAgent = agents.head
    protected val masterModule: Module = EMPTY_MODULE
    protected lazy val master: RunningMaster = directoryProvider.startMaster(masterModule) await 99.s

    override def beforeAll() = {
      super.beforeAll()
      agents
      master
    }

    override def afterAll() = {
      closer.close()
      for (a ← agents) a.close()
      master.close()
      super.afterAll()
      directoryProvider.close()
    }
  }

  sealed class Tree(val directory: Path) {
    val config = directory / "config"
    val fileBasedDirectory = config / "live"
    lazy val orderGenerators = {
      val dir = config / "order-generators"
      Files.createDirectory(dir)
      dir
    }
    val data = directory / "data"

    def createDirectories(): Unit = {
      Files.createDirectories(fileBasedDirectory)
      Files.createDirectory(data)
    }

    def writeJson[A <: FileBased { type Self = A }: ObjectEncoder](fileBased: A): Unit = {
      require(!fileBased.id.path.isAnonymous, "writeJson: Missing path")
      require(fileBased.id.versionId.isAnonymous, "writeJson accepts only VersionId.Anonymous")
      file(fileBased.path, SourceType.Json).contentString =
        Json.fromJsonObject(implicitly[ObjectEncoder[A]].encodeObject(fileBased.withoutId)).toPrettyString
    }

    def writeTxt(path: TypedPath, content: String): Unit =
      file(path, SourceType.Txt).contentString = content

    def file(path: TypedPath, t: SourceType): Path =
      fileBasedDirectory resolve path.toFile(t)
  }

  final class AgentTree(rootDirectory: Path, val agentPath: AgentPath) extends Tree(rootDirectory / agentPath.name) {
    lazy val conf = AgentConfiguration.forTest(Some(directory)).copy(name = agentPath.name)
    lazy val localUri = Uri("http://127.0.0.1:" + conf.http.get.address.getPort)
  }

  def jobJson(duration: Duration = 0.s, variables: Map[String, String] = Map.empty, resultVariable: Option[String] = None) =
    JobConfiguration(
      JobPath.NoId,
      JobScript(script(duration, resultVariable)),
      variables,
      taskLimit = 10
    ).asJson.toPrettyString

  val StdoutOutput = if (isWindows) "TEST\r\n" else "TEST ☘\n"

  private def script(duration: Duration, resultVariable: Option[String]) =
    if (isWindows)
      (s"""
        |@echo off
        |echo ${StdoutOutput.trim}
        |ping -n ${1 + (duration + 999999.µs).toMillis / 1000} 127.0.0.1 >nul""" +
        resultVariable.fold("")(o ⇒ s"""|echo result=SCRIPT-VARIABLE-%SCHEDULER_PARAM_${o.toUpperCase}% >>"%SCHEDULER_RETURN_VALUES%"""")
      ).stripMargin
    else
      (s"""
        |echo ${StdoutOutput.trim}
        |sleep ${duration.toSecondsString}""" +
        resultVariable.fold("")(o ⇒ s"""|echo "result=SCRIPT-VARIABLE-$$SCHEDULER_PARAM_${o.toUpperCase}" >>"$$SCHEDULER_RETURN_VALUES"""")
      ).stripMargin
}
