package com.sos.jobscheduler.tests.provider

import cats.data.Validated.Invalid
import cats.syntax.option._
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.IOExecutor.Implicits.globalIOX
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.system.OperatingSystem.isMac
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.crypt.silly.SillySigner
import com.sos.jobscheduler.core.filebased.{FileBasedReader, Repo, TypedPaths}
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, FileBasedEvent, VersionAdded}
import com.sos.jobscheduler.data.filebased.{SourceType, VersionId}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.provider.Provider
import com.sos.jobscheduler.provider.configuration.ProviderConfiguration
import com.sos.jobscheduler.tests.provider.ProviderTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProviderForScalaTest
import com.typesafe.config.ConfigFactory
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{createDirectories, delete}
import java.nio.file.Paths
import java.util.concurrent._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class ProviderTest extends FreeSpec with DirectoryProviderForScalaTest
{
  protected val agentPaths = Nil
  protected val fileBased = Nil

  private lazy val privateKeyPassword = SecretString("")
  private val signer = SillySigner.checked("SILLY".getBytes(UTF_8), privateKeyPassword).orThrow
  //Regenerated PGP signatures are not comparable:
  //private lazy val privateKeyPassword = SecretString(Random.nextString(10))
  //val signer = PgpSigner(generateSecretKey(SignerId("TEST"), privateKeyPassword, keySize = 1024/*fast for test*/), privateKeyPassword).orThrow

  override protected def useMessageSigner = master => {
    // DirectoryProvider derives the public key from this MessageSigner
    val keyFile = master.config / "private" / "trusted-silly-keys.txt"
    master.config / "master.conf" ++= s"""jobscheduler.configuration.trusted-signature-keys.Silly = "$keyFile"""" + "\n"
    keyFile := signer.publicKey
    signer
  }

  private lazy val providerDirectory = directoryProvider.directory / "provider"
  private lazy val live = providerDirectory / "config/live"
  private lazy val providerConfiguration = ProviderConfiguration.fromCommandLine(
    "-config-directory=" + providerDirectory / "config" ::
    "-master-uri=" + master.localUri :: Nil,
    testConfig)
  private lazy val provider = Provider(providerConfiguration).orThrow
  private lazy val v1Timestamp = now
  import provider.fileBasedSigner.toSigned

  override def beforeAll() = {
    createDirectories(providerDirectory / "config" / "private")
    createDirectories(live)
    providerDirectory / "config" / "private" / "private-silly-keys.txt" := signer.privateKey
    providerDirectory / "config" / "private" / "private.conf" :=
      s"""jobscheduler.provider.sign-with = Silly
         |jobscheduler.provider.private-signature-keys.Silly {
         |  key = $${jobscheduler.config-directory}"/private/private-silly-keys.txt"
         |  password = "${privateKeyPassword.string}"
         |}
         |jobscheduler.provider.master.user = "$loginName"
         |jobscheduler.provider.master.password = "$loginPassword"
         """.stripMargin

    directoryProvider.master.config / "private" / "private.conf" ++=
     s"""jobscheduler.auth.users {
        |  $loginName {
        |    password = "plain:$loginPassword"
        |    permissions = [ UpdateRepo ]
        |  }
        |}
      """.stripMargin

    createDirectories(providerDirectory / "private")
    createDirectories(providerDirectory / "live" / "folder")
    super.beforeAll()
  }

  override def afterAll() = {
    provider.close()
    deleteDirectoryRecursively(providerDirectory)
    super.afterAll()
  }

  "updateMasterConfiguration" - {
    "(start provider)" in {
      provider
    }

    "Initially, JobScheduler's Repo is empty" in {
      assert(repo.idToSignedFileBased.isEmpty)
    }

    "Write two workflows" in {
      writeFile(AWorkflowPath)
      writeFile(BWorkflowPath)
      v1Timestamp
      provider.updateMasterConfiguration(V1.some).await(99.seconds).orThrow
      assert(master.fileBasedApi.stampedRepo.await(99.seconds).value.idToSignedFileBased == Map(
        (AWorkflowPath % V1) → Some(toSigned(TestWorkflow.withId(AWorkflowPath % V1))),
        (BWorkflowPath % V1) → Some(toSigned(TestWorkflow.withId(BWorkflowPath % V1)))))
    }

    "Duplicate VersionId" in {
      assert(provider.updateMasterConfiguration(V1.some).await(99.seconds) == Invalid(Problem("Duplicate VersionId '1'")))
    }

    "An unknown and some invalid files" in {
      sleep(v1Timestamp + 1.1.seconds - now)  // File system timestamps may have only a one second precision
      writeFile(AWorkflowPath)
      writeFile(CWorkflowPath)
      (live / "UNKNOWN.tmp") := "?"
      (live / "NO-JSON.workflow.json") := "INVALID JSON"
      (live / "ERROR-1.workflow.json") := """{ "something": "different" }"""
      (live / "ERROR-2.workflow.json") := """{ "instructions": 0 }"""
      assert(provider.updateMasterConfiguration(V2.some).await(99.seconds) ==
        Invalid(Problem.Multiple(Set(
          TypedPaths.AlienFileProblem(Paths.get("UNKNOWN.tmp"))))))  // Only the unknown file is noticed
    }

    "Some invalid files" in {
      delete(live / "UNKNOWN.tmp")
      assert(provider.updateMasterConfiguration(V2.some).await(99.seconds) ==
        Invalid(Problem.Multiple(Set(
          FileBasedReader.SourceProblem(WorkflowPath("/NO-JSON"), SourceType.Json, Problem("expected json value got I (line 1, column 1)")),
          FileBasedReader.SourceProblem(WorkflowPath("/ERROR-1"), SourceType.Json, Problem("Attempt to decode value on failed cursor: DownField(instructions)")),
          FileBasedReader.SourceProblem(WorkflowPath("/ERROR-2"), SourceType.Json, Problem("C[A]: DownField(instructions)"))))))
    }

    "Delete invalid files" in {
      delete(live / "NO-JSON.workflow.json")
      delete(live / "ERROR-1.workflow.json")
      delete(live / "ERROR-2.workflow.json")
      provider.updateMasterConfiguration(V2.some).await(99.seconds).orThrow
      assert(master.fileBasedApi.stampedRepo.await(99.seconds).value.idToSignedFileBased == Map(
        (AWorkflowPath % V1) → Some(toSigned(TestWorkflow.withId(AWorkflowPath % V1))),
        (AWorkflowPath % V2) → Some(toSigned(TestWorkflow.withId(AWorkflowPath % V2))),
        (BWorkflowPath % V1) → Some(toSigned(TestWorkflow.withId(BWorkflowPath % V1))),
        (CWorkflowPath % V2) → Some(toSigned(TestWorkflow.withId(CWorkflowPath % V2)))))
    }

    "Delete a Workflow" in {
      delete(live / "B.workflow.json")
      provider.updateMasterConfiguration(V3.some).await(99.seconds).orThrow
      assert(repo.versions == V3 :: V2 :: V1 :: Nil)
      assert(repo.idToSignedFileBased == Map(
        (AWorkflowPath % V1) → Some(toSigned(TestWorkflow.withId(AWorkflowPath % V1))),
        (AWorkflowPath % V2) → Some(toSigned(TestWorkflow.withId(AWorkflowPath % V2))),
        (BWorkflowPath % V1) → Some(toSigned(TestWorkflow.withId(BWorkflowPath % V1))),
        (BWorkflowPath % V3) → None,
        (CWorkflowPath % V2) → Some(toSigned(TestWorkflow.withId(CWorkflowPath % V2)))))
    }

    "closeTask" in {
      provider.closeTask await 99.s
    }
  }

  "observe" - {
    lazy val whenObserved = Provider.observe(providerConfiguration).orThrow
      .onCancelTriggerError
      .foreach { _ ⇒ }
    var lastEventId = EventId.BeforeFirst

    // Observer does not call replaceMasterConfiguration because MasterOrderKeeper does not support change of Agents !!!
    "Initial observation with file added" in {
      lastEventId = master.eventWatch.lastAddedEventId
      writeFile(BWorkflowPath)

      whenObserved
      val events = master.eventWatch.await[FileBasedEvent](after = lastEventId).map(_.value)
      val versionId = master.eventWatch.await[VersionAdded](after = lastEventId).head.value.event.versionId
      assert(events == Vector(BWorkflowPath)
        .map(path => NoKey <-: FileBasedAdded(path, sign(TestWorkflow withId path % versionId))))
    }

    "Delete a file" in {
      whenObserved
      lastEventId = master.eventWatch.lastAddedEventId
      delete(live resolve CWorkflowPath.toFile(SourceType.Json))
      assert(master.eventWatch.await[FileBasedEvent](after = lastEventId).map(_.value) ==
        Vector(NoKey <-: FileBasedDeleted(CWorkflowPath)))
    }

    "Add a file" in {
      assert(!whenObserved.isCompleted)
      lastEventId = master.eventWatch.lastAddedEventId
      writeFile(CWorkflowPath)
      val versionId = master.eventWatch.await[VersionAdded](after = lastEventId).head.value.event.versionId
      assert(master.eventWatch.await[FileBasedEvent](after = lastEventId).map(_.value) ==
        Vector(NoKey <-: FileBasedAdded(CWorkflowPath, sign(TestWorkflow withId CWorkflowPath % versionId))))
    }

    "Change a file" in {
      assert(!whenObserved.isCompleted)
      lastEventId = master.eventWatch.lastAddedEventId
      live.resolve(CWorkflowPath toFile SourceType.Json) := ChangedWorkflowJson
      val versionId = master.eventWatch.await[VersionAdded](after = lastEventId).head.value.event.versionId
      assert(master.eventWatch.await[FileBasedEvent](after = lastEventId).map(_.value) ==
        Vector(NoKey <-: FileBasedChanged(CWorkflowPath, sign(ChangedWorkflow withId CWorkflowPath % versionId))))
    }

    "cancel" in {
      assert(!whenObserved.isCompleted)
      assert(!whenObserved.isCompleted)
      whenObserved.cancel()
      intercept[CancellationException] {  // Due to onCancelTriggerError
        whenObserved await 9.seconds
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
  private val testConfig = ConfigFactory.parseString(
    s"""jobscheduler.provider.file-watch.minimum-silence = 50ms
       |jobscheduler.provider.file-watch.poll-interval = ${if (isMac) "100ms" else "300s"}
       |""".stripMargin)

  private val AWorkflowPath = WorkflowPath("/A")
  private val BWorkflowPath = WorkflowPath("/B")
  private val CWorkflowPath = WorkflowPath("/C")

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
