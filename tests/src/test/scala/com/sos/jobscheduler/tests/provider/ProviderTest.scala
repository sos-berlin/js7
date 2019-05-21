package com.sos.jobscheduler.tests.provider

import cats.data.Validated.Invalid
import cats.syntax.option._
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.IOExecutor.Implicits.globalIOX
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits._
import com.sos.jobscheduler.common.system.OperatingSystem.isMac
import com.sos.jobscheduler.core.crypt.silly.SillySigner
import com.sos.jobscheduler.core.filebased.{FileBasedReader, FileBaseds, Repo, TypedPaths}
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, FileBasedEvent, VersionAdded}
import com.sos.jobscheduler.data.filebased.{SourceType, VersionId}
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.OrderAdded
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.provider.Provider
import com.sos.jobscheduler.provider.configuration.ProviderConfiguration
import com.sos.jobscheduler.tests.provider.ProviderTest._
import com.sos.jobscheduler.tests.testenv.MasterAgentForScalaTest
import com.typesafe.config.ConfigFactory
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{createDirectories, delete}
import java.nio.file.{Files, Paths}
import java.util.concurrent._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class ProviderTest extends FreeSpec with MasterAgentForScalaTest
{
  override protected def suppressRepoInitialization = true
  protected val agentRefPaths = agentRefPath :: Nil
  protected val fileBased = Nil
  private lazy val agentRef = directoryProvider.agentRefs.head
  private lazy val privateKeyPassword = SecretString("")
  override protected val signer = SillySigner.checked("SILLY".getBytes(UTF_8), privateKeyPassword).orThrow

  private lazy val providerDirectory = directoryProvider.directory / "provider"
  private lazy val live = providerDirectory / "config/live"
  private lazy val orderGeneratorsDir = providerDirectory / "config/order-generators"
  private lazy val providerConfiguration = ProviderConfiguration.fromCommandLine(
    "-config-directory=" + providerDirectory / "config" ::
    "-master-uri=" + master.localUri :: Nil,
    testConfig)
  private lazy val provider = Provider(providerConfiguration).orThrow
  private lazy val v1Time = now
  import provider.fileBasedSigner.toSigned

  override def beforeAll() = {
    createDirectories(providerDirectory / "config" / "private")
    createDirectories(live)
    createDirectories(orderGeneratorsDir)

    providerDirectory / "config" / "provider.conf" :=
      """jobscheduler.provider.add-orders-every = 0.1s
        |jobscheduler.provider.add-orders-earlier = 0.1s
        |""".stripMargin
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

    directoryProvider.agents.head.writeExecutable(ExecutablePath("/EXECUTABLE"), ":")

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

    "Start with one AgentRef and two workflows" in {
      live.resolve(agentRef.path.toFile(SourceType.Json)) := agentRef
      writeWorkflowFile(AWorkflowPath)
      writeWorkflowFile(BWorkflowPath)
      v1Time

      // `initiallyUpdateMasterConfiguration` will send this diff to the Master
      assert(provider.testMasterDiff.await(99.seconds).orThrow == FileBaseds.Diff(
        added = agentRef :: TestWorkflow.withId(AWorkflowPath) :: TestWorkflow.withId(BWorkflowPath) :: Nil))

      provider.initiallyUpdateMasterConfiguration(V1.some).await(99.seconds).orThrow
      assert(master.fileBasedApi.stampedRepo.await(99.seconds).value.idToSignedFileBased == Map(
        (agentRef.path ~ V1) -> Some(toSigned(agentRef withVersion V1)),
        (AWorkflowPath ~ V1) -> Some(toSigned(TestWorkflow.withId(AWorkflowPath ~ V1))),
        (AWorkflowPath ~ V1) -> Some(toSigned(TestWorkflow.withId(AWorkflowPath ~ V1))),
        (BWorkflowPath ~ V1) -> Some(toSigned(TestWorkflow.withId(BWorkflowPath ~ V1)))))

      assert(provider.testMasterDiff.await(99.seconds).orThrow.isEmpty)
    }

    "Duplicate VersionId" in {
      assert(provider.updateMasterConfiguration(V1.some).await(99.seconds) == Invalid(Problem("Duplicate VersionId '1'")))
    }

    "An unknown and some invalid files" in {
      sleep((v1Time + 1.1.seconds).timeLeftOrZero)  // File system timestamps may have only a one second precision
      writeWorkflowFile(AWorkflowPath)
      writeWorkflowFile(CWorkflowPath)
      (live / "UNKNOWN.tmp") := "?"
      (live / "NO-JSON.workflow.json") := "INVALID JSON"
      (live / "ERROR-1.workflow.json") := json"""{ "something": "different" }"""
      (live / "ERROR-2.workflow.json") := json"""{ "instructions": 0 }"""
      assert(provider.updateMasterConfiguration(V2.some).await(99.seconds) ==
        Invalid(Problem.Multiple(Set(
          TypedPaths.AlienFileProblem(Paths.get("UNKNOWN.tmp")),
          FileBasedReader.SourceProblem(WorkflowPath("/NO-JSON"), SourceType.Json, Problem("expected json value got 'INVALI...' (line 1, column 1)")),
          FileBasedReader.SourceProblem(WorkflowPath("/ERROR-1"), SourceType.Json, Problem("Attempt to decode value on failed cursor: DownField(instructions)")),
          FileBasedReader.SourceProblem(WorkflowPath("/ERROR-2"), SourceType.Json, Problem("C[A]: DownField(instructions)"))))))
    }

    "Delete invalid files" in {
      delete(live / "UNKNOWN.tmp")
      delete(live / "NO-JSON.workflow.json")
      delete(live / "ERROR-1.workflow.json")
      delete(live / "ERROR-2.workflow.json")
      provider.updateMasterConfiguration(V2.some).await(99.seconds).orThrow
      assert(master.fileBasedApi.stampedRepo.await(99.seconds).value.idToSignedFileBased == Map(
        (agentRef.path ~ V1) -> Some(toSigned(agentRef withVersion V1)),
        (AWorkflowPath ~ V1) -> Some(toSigned(TestWorkflow.withId(AWorkflowPath ~ V1))),
        (AWorkflowPath ~ V2) -> Some(toSigned(TestWorkflow.withId(AWorkflowPath ~ V2))),
        (BWorkflowPath ~ V1) -> Some(toSigned(TestWorkflow.withId(BWorkflowPath ~ V1))),
        (CWorkflowPath ~ V2) -> Some(toSigned(TestWorkflow.withId(CWorkflowPath ~ V2)))))
    }

    "Delete a Workflow" in {
      delete(live / "B.workflow.json")
      provider.updateMasterConfiguration(V3.some).await(99.seconds).orThrow
      assert(repo.versions == V3 :: V2 :: V1 :: Nil)
      assert(repo.idToSignedFileBased == Map(
        (agentRef.path ~ V1) -> Some(toSigned(agentRef withVersion V1)),
        (AWorkflowPath ~ V1) -> Some(toSigned(TestWorkflow.withId(AWorkflowPath ~ V1))),
        (AWorkflowPath ~ V2) -> Some(toSigned(TestWorkflow.withId(AWorkflowPath ~ V2))),
        (BWorkflowPath ~ V1) -> Some(toSigned(TestWorkflow.withId(BWorkflowPath ~ V1))),
        (BWorkflowPath ~ V3) -> None,
        (CWorkflowPath ~ V2) -> Some(toSigned(TestWorkflow.withId(CWorkflowPath ~ V2)))))
    }

    "Workflow notation (including a try-instruction)" in {
      val notation =
        """define workflow {
             try fail;
             catch {}
           }"""
      val workflowPath = WorkflowPath("/NOTATION")
      val workflow = WorkflowParser.parse(workflowPath, notation).orThrow
      live.resolve(workflowPath.toFile(SourceType.Txt)) := notation

      assert(provider.testMasterDiff.await(99.seconds).orThrow == FileBaseds.Diff(added = workflow :: Nil))
      provider.updateMasterConfiguration(V4.some).await(99.seconds).orThrow
      assert(provider.testMasterDiff.await(99.seconds).orThrow.isEmpty)
    }

    "closeTask" in {
      provider.closeTask await 99.s
      provider.close()
    }
  }

  "observe" - {
    lazy val whenObserved = Provider.observe(providerConfiguration).orThrow
      .onCancelTriggerError
      .foreach { _ => }
    var lastEventId = EventId.BeforeFirst

    "Initial observation with a workflow and an agentRef added" in {
      lastEventId = master.eventWatch.lastAddedEventId
      writeWorkflowFile(BWorkflowPath)
      live.resolve(agentRefPath.toFile(SourceType.Json)) := AgentRef(agentRefPath, uri = agent.localUri.toString)

      whenObserved
      val versionId = master.eventWatch.await[VersionAdded](after = lastEventId).head.value.event.versionId
      val events = master.eventWatch.await[FileBasedEvent](after = lastEventId).map(_.value)
      assert(events == Vector(BWorkflowPath)
        .map(path => NoKey <-: FileBasedAdded(path, sign(TestWorkflow withId path ~ versionId))))
    }

    "Delete a workflow" in {
      whenObserved
      lastEventId = master.eventWatch.lastAddedEventId
      delete(live resolve CWorkflowPath.toFile(SourceType.Json))
      assert(master.eventWatch.await[FileBasedEvent](after = lastEventId).map(_.value) ==
        Vector(NoKey <-: FileBasedDeleted(CWorkflowPath)))
    }

    "Add a workflow" in {
      assert(!whenObserved.isCompleted)
      lastEventId = master.eventWatch.lastAddedEventId
      writeWorkflowFile(CWorkflowPath)
      val versionId = master.eventWatch.await[VersionAdded](after = lastEventId).head.value.event.versionId
      assert(master.eventWatch.await[FileBasedEvent](after = lastEventId).map(_.value) ==
        Vector(NoKey <-: FileBasedAdded(CWorkflowPath, sign(TestWorkflow withId CWorkflowPath ~ versionId))))
    }

    "Change a workflow" in {
      assert(!whenObserved.isCompleted)
      lastEventId = master.eventWatch.lastAddedEventId
      live.resolve(CWorkflowPath toFile SourceType.Json) := ChangedWorkflowJson
      val versionId = master.eventWatch.await[VersionAdded](after = lastEventId).head.value.event.versionId
      assert(master.eventWatch.await[FileBasedEvent](after = lastEventId).map(_.value) ==
        Vector(NoKey <-: FileBasedChanged(CWorkflowPath, sign(ChangedWorkflow withId CWorkflowPath ~ versionId))))
    }

    "Add an order generator" in {
      lastEventId = master.eventWatch.lastAddedEventId
      (orderGeneratorsDir / "test.order.xml").xml =
        <order job_chain="/A">
          <run_time>
            <period absolute_repeat="1"/>
          </run_time>
        </order>
      master.eventWatch.await[OrderAdded](_.event.workflowId.path == AWorkflowPath, after = lastEventId)
      lastEventId = master.eventWatch.lastAddedEventId
      master.eventWatch.await[OrderAdded](_.event.workflowId.path == AWorkflowPath, after = lastEventId)
    }

    "Replace an order generator" in {
      (orderGeneratorsDir / "test.order.xml").xml =
        <order job_chain="/B">
          <run_time>
            <period absolute_repeat="1"/>
          </run_time>
        </order>
      master.eventWatch.await[OrderAdded](_.event.workflowId.path == BWorkflowPath, after = lastEventId)
      lastEventId = master.eventWatch.lastAddedEventId
      master.eventWatch.await[OrderAdded](_.event.workflowId.path == BWorkflowPath, after = lastEventId)
    }

    "Delete an order generator" in {
      Files.delete(orderGeneratorsDir / "test.order.xml")
      master.eventWatch.await[OrderAdded](_.event.workflowId.path == BWorkflowPath, after = lastEventId)  // TIMEOUT
    }

    "Cancel" in {
      assert(!whenObserved.isCompleted)
      assert(!whenObserved.isCompleted)
      whenObserved.cancel()
      intercept[CancellationException] {  // Due to onCancelTriggerError
        whenObserved await 99.seconds
      }
    }
  }

  private def repo: Repo = master.fileBasedApi.stampedRepo.await(99.seconds).value

  private def writeWorkflowFile(workflowPath: WorkflowPath): Unit =
    live.resolve(workflowPath.toFile(SourceType.Json)) := TestWorkflowJson
}

object ProviderTest
{
  private val loginName = "ProviderTest"
  private val loginPassword = "ProviderTest-PASSWORD"
  private val testConfig = ConfigFactory.parseString(
    s"""jobscheduler.provider.directory-watch.minimum-silence = 50ms
       |jobscheduler.provider.directory-watch.poll-interval = ${if (isMac) "100ms" else "300s"}
       |""".stripMargin)

  private val agentRefPath = AgentRefPath("/AGENT")
  private val AWorkflowPath = WorkflowPath("/A")
  private val BWorkflowPath = WorkflowPath("/B")
  private val CWorkflowPath = WorkflowPath("/C")

  private val V1 = VersionId("1")
  private val V2 = VersionId("2")
  private val V3 = VersionId("3")
  private val V4 = VersionId("4")

  private val TestWorkflowJson = json"""
    {
      "instructions": [
        {
          "TYPE": "Execute.Anonymous",
          "job": {
            "agentRefPath": "/AGENT",
            "executable": {
              "TYPE": "ExecutablePath",
              "path": "/EXECUTABLE"
            },
            "taskLimit": 1
          }
        }
      ]
    }"""
  private val TestWorkflow = TestWorkflowJson.as[Workflow].orThrow
  private val ChangedWorkflowJson = json"""
    {
      "instructions": [
        {
          "TYPE": "Execute.Anonymous",
          "job": {
            "agentRefPath": "/AGENT",
            "executable": {
              "TYPE": "ExecutablePath",
              "path": "/OTHER-EXECUTABLE"
            },
            "taskLimit": 1
          }
        }
      ]
    }"""
  private val ChangedWorkflow = ChangedWorkflowJson.as[Workflow].orThrow
}
