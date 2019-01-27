package com.sos.jobscheduler.tests.provider

import cats.data.Validated.Invalid
import cats.syntax.option._
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.{deleteDirectoryRecursively, writeToFile}
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.IOExecutor.Implicits.globalIOX
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.system.OperatingSystem.isMac
import com.sos.jobscheduler.core.filebased.{FileBasedReader, Repo, TypedPaths}
import com.sos.jobscheduler.core.signature.PGPSigner.writeSecretKeyAsAscii
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, FileBasedEvent}
import com.sos.jobscheduler.data.filebased.{SourceType, VersionId}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.provider.configuration.ProviderConfiguration
import com.sos.jobscheduler.provider.{DirectoryToJobscheduler, Provider}
import com.sos.jobscheduler.tests.DirectoryProvider
import com.sos.jobscheduler.tests.provider.ProviderTest._
import com.typesafe.config.ConfigFactory
import java.nio.file.Files.{createDirectories, createTempDirectory, delete}
import java.nio.file.Paths
import java.util.concurrent._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class ProviderTest extends FreeSpec with DirectoryProvider.ForScalaTest
{
  protected val agentPaths = Nil

  private lazy val directory = createTempDirectory("test-")
  private lazy val live = directory / "live"
  private lazy val providerConfiguration = ProviderConfiguration.fromCommandLine(
    "-config-directory=" + directory ::
    "-master-uri=" + master.localUri :: Nil)

  override def beforeAll() = {
    directoryProvider.master.config / "private" / "private.conf" ++=
     s"""jobscheduler.auth.users {
        |  $loginName {
        |    password = "plain:$loginPassword"
        |    permissions = [ UpdateRepo ]
        |  }
        |}
      """.stripMargin

    createDirectories(directory / "private")
    createDirectories(directory / "live" / "folder")

    super.beforeAll()
  }

  override def afterAll() = {
    deleteDirectoryRecursively(directory)
    super.afterAll()
  }

  "updateMaster" - {
    // We reuse the DirectoryProvider's PGPSecretKey, whose public key is already known to the Master
    import directoryProvider.{pgpPassword, pgpSecretKey}

    lazy val provider = new Provider(providerConfiguration)

    "(start)" in {
      writeToFile(directory / "private" / "private-pgp-key.asc")(writeSecretKeyAsAscii(pgpSecretKey, _))
      directory / "private" / "private.conf" :=
        s"""jobscheduler.provider.master.user = "$loginName"
           |jobscheduler.provider.master.password = "$loginPassword"
           |jobscheduler.provider.pgp.password = "${pgpPassword.string}"
           |""".stripMargin
      provider
    }

    "Initially, JobScheduler's Repo is empty" in {
      assert(repo.idToFileBased.isEmpty)
    }

    "Write two workflows" in {
      writeFile(AWorkflowPath)
      writeFile(BWorkflowPath)
      provider.updateMaster(V1.some).await(99.seconds).orThrow
      assert(master.fileBasedApi.stampedRepo.await(99.seconds).value.idToFileBased == Map(
        (AWorkflowPath % V1) → Some(TestWorkflow.withId(AWorkflowPath % V1)),
        (BWorkflowPath % V1) → Some(TestWorkflow.withId(BWorkflowPath % V1))))
    }

    "Duplicate VersionId" in {
      assert(provider.updateMaster(V1.some).await(99.seconds) == Invalid(Problem("Duplicate VersionId '1'")))
    }

    "An unknown and some invalid files" in {
      writeFile(AWorkflowPath)
      writeFile(CWorkflowPath)
      (live / "UNKNOWN.tmp") := "?"
      (live / "NO-JSON.workflow.json") := "INVALID JSON"
      (live / "ERROR-1.workflow.json") := """{ "something": "different" }"""
      (live / "ERROR-2.workflow.json") := """{ "instructions": 0 }"""
      assert(provider.updateMaster(V2.some).await(99.seconds) ==
        Invalid(Problem.Multiple(Set(
          TypedPaths.AlienFileProblem(Paths.get("UNKNOWN.tmp"))))))  // Only the unknown file is noticed
    }

    "Some invalid files" in {
      delete(live / "UNKNOWN.tmp")
      assert(provider.updateMaster(V2.some).await(99.seconds) ==
        Invalid(Problem.Multiple(Set(
          FileBasedReader.SourceProblem(WorkflowPath("/NO-JSON"), SourceType.Json, Problem("expected json value got I (line 1, column 1)")),
          FileBasedReader.SourceProblem(WorkflowPath("/ERROR-1"), SourceType.Json, Problem("Attempt to decode value on failed cursor: DownField(instructions)")),
          FileBasedReader.SourceProblem(WorkflowPath("/ERROR-2"), SourceType.Json, Problem("C[A]: DownField(instructions)"))))))
    }

    "Delete invalid files" in {
      delete(live / "NO-JSON.workflow.json")
      delete(live / "ERROR-1.workflow.json")
      delete(live / "ERROR-2.workflow.json")
      provider.updateMaster(V2.some).await(99.seconds).orThrow
      assert(master.fileBasedApi.stampedRepo.await(99.seconds).value.idToFileBased == Map(
        (AWorkflowPath % V1) → Some(TestWorkflow.withId(AWorkflowPath % V1)),
        (BWorkflowPath % V1) → Some(TestWorkflow.withId(BWorkflowPath % V1)),
        (CWorkflowPath % V2) → Some(TestWorkflow.withId(CWorkflowPath % V2))))
    }

    "Delete a Workflow" in {
      delete(live / "B.workflow.json")
      provider.updateMaster(V3.some).await(99.seconds).orThrow
      assert(repo.versions == V3 :: V2 :: V1 :: VersionId("INITIAL") :: Nil)
      assert(repo.idToFileBased == Map(
        (AWorkflowPath % V1) → Some(TestWorkflow.withId(AWorkflowPath % V1)),
        (BWorkflowPath % V1) → Some(TestWorkflow.withId(BWorkflowPath % V1)),
        (BWorkflowPath % V3) → None,
        (CWorkflowPath % V2) → Some(TestWorkflow.withId(CWorkflowPath % V2))))
    }

    "updateMaster without VersionId" in {
      writeFile(DWorkflowPath)
      provider.updateMaster(None).await(99.seconds).orThrow
      assert(repo.currentTyped[Workflow].apply(DWorkflowPath).id.versionId.string startsWith "#")  // Master has generated a VersionId
    }
  }

  "DirectoryToJobscheduler.observe" - {
    lazy val config = ConfigFactory.parseString(
      s"""jobscheduler.provider.file-watch.minimum-silence = 50ms
         |jobscheduler.provider.file-watch.poll-interval = ${if (isMac) "100ms" else "300s"}
         |""".stripMargin)
    lazy val myConf = providerConfiguration.copy(config = config withFallback providerConfiguration.config)
    lazy val watcher = new DirectoryToJobscheduler(myConf)
    lazy val whenTerminated = watcher.observe().onCancelTriggerError foreach { _ ⇒ }
    var lastEventId = EventId.BeforeFirst

    "Delete file" in {
      whenTerminated
      lastEventId = master.eventWatch.lastAddedEventId
      delete(live resolve CWorkflowPath.toFile(SourceType.Json))
      assert(master.eventWatch.await[FileBasedEvent](after = lastEventId).map(_.value) ==
        Vector(NoKey <-: FileBasedDeleted(CWorkflowPath)))
    }

    "Add a file" in {
      lastEventId = master.eventWatch.lastAddedEventId
      writeFile(CWorkflowPath)
      assert(master.eventWatch.await[FileBasedEvent](after = lastEventId).map(_.value) ==
        Vector(NoKey <-: FileBasedAdded(TestWorkflow withId CWorkflowPath)))
    }

    "Change a file" in {
      lastEventId = master.eventWatch.lastAddedEventId
      live.resolve(CWorkflowPath toFile SourceType.Json) := ChangedWorkflowJson
      assert(master.eventWatch.await[FileBasedEvent](after = lastEventId).map(_.value) ==
        Vector(NoKey <-: FileBasedChanged(ChangedWorkflow withId CWorkflowPath)))
    }

    "cancel" in {
      assert(!whenTerminated.isCompleted)
      whenTerminated.cancel()
      intercept[CancellationException] {  // Due to onCancelTriggerError
        whenTerminated await 9.seconds
      }
    }
  }

  private def repo: Repo = master.fileBasedApi.stampedRepo.await(99.seconds).value

  private def writeFile(workflowPath: WorkflowPath): Unit =
    live.resolve(workflowPath.toFile(SourceType.Json)) := TestWorkflowJson
}

object ProviderTest
{
  private val loginName = "ProviderTest"
  private val loginPassword = "ProviderTest-PASSWORD"

  private val AWorkflowPath = WorkflowPath("/A")
  private val BWorkflowPath = WorkflowPath("/B")
  private val CWorkflowPath = WorkflowPath("/C")
  private val DWorkflowPath = WorkflowPath("/D")

  private val V1 = VersionId("1")
  private val V2 = VersionId("2")
  private val V3 = VersionId("3")

  private val TestWorkflowJson = json"""
    {
      "instructions": [
        { "TYPE": "Execute.Anonymous", "job": { "agentPath": "/AGENT", "executablePath": "/EXECUTABLE", "taskLimit": 1 }}
      ]
    }"""
  private val TestWorkflow = TestWorkflowJson.as[Workflow].orThrow
  private val ChangedWorkflowJson = json"""
    {
      "instructions": [
        { "TYPE": "Execute.Anonymous", "job": { "agentPath": "/AGENT", "executablePath": "/OTHER-EXECUTABLE", "taskLimit": 1 }}
      ]
    }"""
  private val ChangedWorkflow = ChangedWorkflowJson.as[Workflow].orThrow
}
