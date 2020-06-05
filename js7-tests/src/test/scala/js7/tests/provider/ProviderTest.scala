package js7.tests.provider

import cats.syntax.option._
import js7.base.circeutils.CirceUtils._
import js7.base.crypt.silly.SillySigner
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.RichThrowableEither
import js7.common.scalautil.FileUtils.deleteDirectoryRecursively
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.IOExecutor.Implicits.globalIOX
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.scalautil.xmls.ScalaXmls.implicits._
import js7.common.system.OperatingSystem.isMac
import js7.core.filebased.{FileBasedReader, FileBaseds, TypedPaths}
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.filebased.Repo.Entry
import js7.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, FileBasedEvent, VersionAdded}
import js7.data.filebased.{Repo, SourceType, VersionId}
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.OrderAdded
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.provider.Provider
import js7.provider.configuration.ProviderConfiguration
import js7.tests.provider.ProviderTest._
import js7.tests.testenv.MasterAgentForScalaTest
import com.typesafe.config.ConfigFactory
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{createDirectories, delete}
import java.nio.file.{Files, Paths}
import java.util.concurrent._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class ProviderTest extends AnyFreeSpec with MasterAgentForScalaTest
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
      """js7.provider.add-orders-every = 0.1s
        |js7.provider.add-orders-earlier = 0.1s
        |""".stripMargin
    providerDirectory / "config" / "private" / "private-silly-keys.txt" := signer.privateKey
    providerDirectory / "config" / "private" / "private.conf" :=
      s"""js7.provider.sign-with = Silly
         |js7.provider.private-signature-keys.Silly {
         |  key = $${js7.config-directory}"/private/private-silly-keys.txt"
         |  password = "${privateKeyPassword.string}"
         |}
         |js7.provider.master.user = "$loginName"
         |js7.provider.master.password = "$loginPassword"
         """.stripMargin

    directoryProvider.master.configDir / "private" / "private.conf" ++=
     s"""js7.auth.users {
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

    "Initially, JS7's Repo is empty" in {
      assert(checkedRepo.map(_.pathToVersionToSignedFileBased.isEmpty) == Right(true))
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
      assert(master.fileBasedApi.checkedRepo.await(99.seconds).map(_.pathToVersionToSignedFileBased) == Right(Map(
        agentRef.path -> List(
          Entry(V1, Some(toSigned(agentRef withVersion V1)))),
        AWorkflowPath -> List(
          Entry(V1, Some(toSigned(TestWorkflow.withId(AWorkflowPath ~ V1))))),
        AWorkflowPath -> List(
          Entry(V1, Some(toSigned(TestWorkflow.withId(AWorkflowPath ~ V1))))),
        BWorkflowPath -> List(
          Entry(V1, Some(toSigned(TestWorkflow.withId(BWorkflowPath ~ V1))))))))

      assert(provider.testMasterDiff.await(99.seconds).orThrow.isEmpty)
    }

    "Duplicate VersionId" in {
      assert(provider.updateMasterConfiguration(V1.some).await(99.seconds) == Left(Problem("Duplicate VersionId '1'")))
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
        Left(Problem.Combined(Set(
          TypedPaths.AlienFileProblem(Paths.get("UNKNOWN.tmp")),
          FileBasedReader.SourceProblem(WorkflowPath("/NO-JSON"), SourceType.Json, Problem("JSON ParsingFailure: expected json value got 'INVALI...' (line 1, column 1)")),
          FileBasedReader.SourceProblem(WorkflowPath("/ERROR-1"), SourceType.Json, Problem("JSON DecodingFailure at .instructions: Attempt to decode value on failed cursor")),
          FileBasedReader.SourceProblem(WorkflowPath("/ERROR-2"), SourceType.Json, Problem("JSON DecodingFailure at .instructions: C[A]"))))))
    }

    "Delete invalid files" in {
      delete(live / "UNKNOWN.tmp")
      delete(live / "NO-JSON.workflow.json")
      delete(live / "ERROR-1.workflow.json")
      delete(live / "ERROR-2.workflow.json")
      provider.updateMasterConfiguration(V2.some).await(99.seconds).orThrow
      assert(master.fileBasedApi.checkedRepo.await(99.seconds).map(_.pathToVersionToSignedFileBased) == Right(Map(
        agentRef.path -> List(
          Entry(V1, Some(toSigned(agentRef withVersion V1)))),
        AWorkflowPath -> List(
          Entry(V2, Some(toSigned(TestWorkflow.withId(AWorkflowPath ~ V2)))),
          Entry(V1, Some(toSigned(TestWorkflow.withId(AWorkflowPath ~ V1))))),
        BWorkflowPath -> List(
          Entry(V1, Some(toSigned(TestWorkflow.withId(BWorkflowPath ~ V1))))),
        CWorkflowPath -> List(
          Entry(V2, Some(toSigned(TestWorkflow.withId(CWorkflowPath ~ V2))))))))
    }

    "Delete a Workflow" in {
      delete(live / "B.workflow.json")
      provider.updateMasterConfiguration(V3.some).await(99.seconds).orThrow
      assert(checkedRepo.map(_.versions) == Right(V3 :: V2 :: V1 :: Nil))
      assert(checkedRepo.map(_.pathToVersionToSignedFileBased) == Right(Map(
        agentRef.path -> List(
          Entry(V1, Some(toSigned(agentRef withVersion V1)))),
        AWorkflowPath -> List(
          Entry(V2, Some(toSigned(TestWorkflow.withId(AWorkflowPath ~ V2)))),
          Entry(V1, Some(toSigned(TestWorkflow.withId(AWorkflowPath ~ V1))))),
        BWorkflowPath -> List(
          Entry(V3, None),
          Entry(V1, Some(toSigned(TestWorkflow.withId(BWorkflowPath ~ V1))))),
        CWorkflowPath -> List(
          Entry(V2, Some(toSigned(TestWorkflow.withId(CWorkflowPath ~ V2))))))))
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
      live.resolve(agentRefPath.toFile(SourceType.Json)) := AgentRef(agentRefPath, uri = agent.localUri)

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

  private def checkedRepo: Checked[Repo] =
    master.fileBasedApi.checkedRepo.await(99.seconds)

  private def writeWorkflowFile(workflowPath: WorkflowPath): Unit =
    live.resolve(workflowPath.toFile(SourceType.Json)) := TestWorkflowJson
}

object ProviderTest
{
  private val loginName = "ProviderTest"
  private val loginPassword = "ProviderTest-PASSWORD"
  private val testConfig = ConfigFactory.parseString(
    s"""js7.provider.directory-watch.minimum-silence = 50ms
       |js7.provider.directory-watch.poll-interval = ${if (isMac) "100ms" else "300s"}
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
