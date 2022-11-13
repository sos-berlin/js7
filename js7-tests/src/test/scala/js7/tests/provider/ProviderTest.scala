package js7.tests.provider

import cats.syntax.option.*
import java.nio.file.Files.{createDirectories, delete}
import java.nio.file.Paths
import js7.base.circeutils.CirceUtils.*
import js7.base.configutils.Configs.*
import js7.base.crypt.silly.SillySigner
import js7.base.data.ByteArray
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.system.OperatingSystem.isMac
import js7.base.thread.Futures.implicits.*
import js7.base.test.OurTestSuite
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.core.item.{ItemPaths, VersionedItemReader}
import js7.data.agent.AgentPath
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded, VersionedItemChanged, VersionedItemEvent, VersionedItemRemoved}
import js7.data.item.{InventoryItemDiff, InventoryItemKey, InventoryItemPath, Repo, SourceType, VersionId}
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.OrderAdded
import js7.data.subagent.SubagentId
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath, WorkflowPathControlPath}
import js7.provider.Provider
import js7.provider.configuration.ProviderConfiguration
import js7.tests.provider.ProviderTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.Promise
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
final class ProviderTest extends OurTestSuite with ControllerAgentForScalaTest
{
  override protected def doNotAddItems = true
  protected val agentPaths = agentPath :: Nil
  protected val items = Nil
  private lazy val agentRef = directoryProvider.agentRefs.head
  private lazy val subagentItem = directoryProvider.subagentItems.head
  private lazy val privateKeyPassword = SecretString("")
  private val privateKey = ByteArray("SILLY")
  override protected val signer: SillySigner =
    SillySigner.checked(privateKey, privateKeyPassword).orThrow
  override protected val verifier = signer.toVerifier

  private lazy val providerDirectory = directoryProvider.directory / "provider"
  private lazy val live = providerDirectory / "config/live"
  private lazy val orderGeneratorsDir = providerDirectory / "config/order-generators"
  private lazy val providerConfiguration = ProviderConfiguration.fromCommandLine(Seq(
    "--config-directory=" + providerDirectory / "config",
    "--controller-uri=" + controller.localUri),
    testConfig)
  private lazy val provider = Provider(providerConfiguration).orThrow
  private lazy val v1Time = now

  override def beforeAll() = {
    createDirectories(providerDirectory / "config" / "private")
    createDirectories(live)
    createDirectories(orderGeneratorsDir)

    providerDirectory / "config" / "provider.conf" :=
       """js7.provider.add-orders-every = 0.1s
         |js7.provider.add-orders-earlier = 0.1s
         |""".stripMargin
    providerDirectory / "config" / "private" / "private-silly-keys.txt" := privateKey
    providerDirectory / "config" / "private" / "private.conf" :=
      s"""js7.provider.sign-with = Silly
         |js7.provider.private-signature-keys.Silly {
         |  key = $${js7.config-directory}"/private/private-silly-keys.txt"
         |  password = "${privateKeyPassword.string}"
         |}
         |js7.provider.controller.user = "$loginName"
         |js7.provider.controller.password = "$loginPassword"
         """.stripMargin

    directoryProvider.controller.configDir / "private" / "private.conf" ++=
     s"""js7.auth.users {
        |  $loginName {
        |    password = "plain:$loginPassword"
        |    permissions = [ UpdateItem ]
        |  }
        |}
      """.stripMargin

    directoryProvider.agents.head.writeExecutable(RelativePathExecutable("EXECUTABLE.cmd"), ":")

    createDirectories(providerDirectory / "private")
    createDirectories(providerDirectory / "live" / "folder")
    super.beforeAll()
  }

  override def afterAll() = {
    provider.close()
    deleteDirectoryRecursively(providerDirectory)
    super.afterAll()
  }

  "updateControllerConfiguration" - {
    "(start provider)" in {
      provider
    }

    "Initially, JS7's Repo is empty" in {
      assert(controllerState.repo.pathToVersionToSignedItems.isEmpty)
    }

    "Start with two workflows" in {
      for (agentRef <- directoryProvider.agentRefs) {
        live / (agentRef.path.string + ".agent.json") := agentRef
      }
      for (subagentItem <- directoryProvider.subagentItems) {
        live / (subagentItem.path.string + ".subagent.json") := subagentItem
      }

      writeWorkflowFile(AWorkflowPath)
      writeWorkflowFile(BWorkflowPath)
      v1Time

      // `initiallyUpdateControllerConfiguration` will send this diff to the Controller
      assert(provider.testControllerDiff.await(99.s).orThrow == InventoryItemDiff(
        addedOrChanged = Seq(
          agentRef,
          subagentItem,
          TestWorkflow.withId(AWorkflowPath),
          TestWorkflow.withId(BWorkflowPath)
        ).sortBy(_.key: InventoryItemKey)))

      provider.initiallyUpdateControllerConfiguration(V1.some).await(99.s).orThrow
      //assert(controller.controllerState.map(_.pathToAgentRefState.values).await(99.s) == Seq(agentRef))
      assert(controllerState.repo.pathToVersionToSignedItems == Map(
        AWorkflowPath -> List(
          Repo.Add(sign(TestWorkflow.withId(AWorkflowPath ~ V1)))),
        AWorkflowPath -> List(
          Repo.Add(sign(TestWorkflow.withId(AWorkflowPath ~ V1)))),
        BWorkflowPath -> List(
          Repo.Add(sign(TestWorkflow.withId(BWorkflowPath ~ V1))))))

      assert(provider.testControllerDiff.await(99.s).orThrow == InventoryItemDiff(
        Nil,
        Vector(
          WorkflowPathControlPath(AWorkflowPath),
          WorkflowPathControlPath(BWorkflowPath))))
    }

    "An unknown and some invalid files" in {
      sleep((v1Time + 1.1.seconds).timeLeftOrZero)  // File system timestamps may have only a one second precision
      writeWorkflowFile(AWorkflowPath)
      writeWorkflowFile(CWorkflowPath)
      (live / "UNKNOWN.tmp") := "?"
      (live / "NO-JSON.workflow.json") := "INVALID JSON"
      (live / "ERROR-1.workflow.json") := json"""{ "something": "different" }"""
      (live / "ERROR-2.workflow.json") := json"""{ "instructions": 0 }"""
      assert(provider.updateControllerConfiguration(V2.some).await(99.s) ==
        Left(Problem.Combined(Set(
          ItemPaths.AlienFileProblem(Paths.get("UNKNOWN.tmp"), Set(WorkflowPath, AgentPath, SubagentId)),
          VersionedItemReader.SourceProblem(WorkflowPath("NO-JSON"), SourceType.Json,
            Problem("JSON ParsingFailure: expected json value got 'INVALI...' (line 1, column 1)")),
          VersionedItemReader.SourceProblem(WorkflowPath("ERROR-1"), SourceType.Json,
            Problem("JSON DecodingFailure at .instructions: Missing required field")),
          VersionedItemReader.SourceProblem(WorkflowPath("ERROR-2"), SourceType.Json,
            Problem("JSON DecodingFailure at .instructions: Got value '0' with wrong type, expecting array"))))))
    }

    "Delete invalid files" in {
      delete(live / "UNKNOWN.tmp")
      delete(live / "NO-JSON.workflow.json")
      delete(live / "ERROR-1.workflow.json")
      delete(live / "ERROR-2.workflow.json")
      provider.updateControllerConfiguration(V2.some).await(99.s).orThrow
      // AWorkflow and BWorkflow from previous test are added
      assert(controllerState.repo.pathToVersionToSignedItems == Map(
        AWorkflowPath -> List(
          Repo.Add(sign(TestWorkflow.withId(AWorkflowPath ~ V2)))),
        BWorkflowPath -> List(
          Repo.Add(sign(TestWorkflow.withId(BWorkflowPath ~ V1)))),
        CWorkflowPath -> List(
          Repo.Add(sign(TestWorkflow.withId(CWorkflowPath ~ V2))))))
    }

    "Delete a Workflow" in {
      deleteFile(BWorkflowPath)
      provider.updateControllerConfiguration(V3.some).await(99.s).orThrow
      assert(controllerState.repo.versionIds == List(V2))
      assert(controllerState.repo.pathToVersionToSignedItems == Map(
        AWorkflowPath -> List(
          Repo.Add(sign(TestWorkflow.withId(AWorkflowPath ~ V2)))),
        CWorkflowPath -> List(
          Repo.Add(sign(TestWorkflow.withId(CWorkflowPath ~ V2))))))
    }

    "Workflow notation (including a try-instruction)" in {
      val notation =
        """define workflow {
             try fail;
             catch {}
           }"""
      val workflowPath = WorkflowPath("NOTATION")
      val workflow = WorkflowParser.parse(workflowPath, notation).orThrow
      live.resolve(workflowPath.toFile(SourceType.Txt)) := notation
      assert(provider.testControllerDiff.await(99.s).orThrow ==
        InventoryItemDiff(addedOrChanged = Seq(workflow)))

      provider.updateControllerConfiguration(V4.some).await(99.s).orThrow
      assert(provider.testControllerDiff.await(99.s).orThrow == InventoryItemDiff(
        Nil,
        Vector(
          WorkflowPathControlPath(AWorkflowPath),
          WorkflowPathControlPath(CWorkflowPath),
          WorkflowPathControlPath(workflowPath))))
    }

    "stop" in {
      provider.stop await 99.s
      provider.close()
    }
  }

  "observe" - {
    val stop = Promise[Unit]()
    lazy val whenObserved = Provider.observe(Task.fromFuture(stop.future), providerConfiguration)
      .orThrow
      .onCancelTriggerError
      .foreach { _ => }
    var lastEventId = EventId.BeforeFirst

    "Initial observation with a workflow and an agentRef added" in {
      lastEventId = eventWatch.lastAddedEventId
      writeWorkflowFile(BWorkflowPath)
      //live / (s"$agentPath.json") := AgentRef(agentPath, uri = agent.localUri)

      whenObserved
      val versionId = eventWatch.await[VersionAdded](after = lastEventId).head.value.event.versionId
      val events = eventWatch.await[VersionedItemEvent](after = lastEventId).map(_.value)
      assert(events == Vector(BWorkflowPath)
        .map(path => NoKey <-: VersionedItemAdded(sign(TestWorkflow withId path ~ versionId))))
    }

    "Delete a workflow" in {
      whenObserved
      lastEventId = eventWatch.lastAddedEventId
      deleteFile(CWorkflowPath)
      assert(eventWatch.await[VersionedItemEvent](after = lastEventId).map(_.value) ==
        Vector(NoKey <-: VersionedItemRemoved(CWorkflowPath)))
    }

    "Add a workflow" in {
      assert(!whenObserved.isCompleted)
      lastEventId = eventWatch.lastAddedEventId
      writeWorkflowFile(CWorkflowPath)
      val versionId = controller.eventWatch.await[VersionAdded](after = lastEventId).head.value.event.versionId
      assert(controller.eventWatch.await[VersionedItemEvent](after = lastEventId).map(_.value) ==
        Vector(NoKey <-: VersionedItemAdded(sign(TestWorkflow withId CWorkflowPath ~ versionId))))
    }

    "Change a workflow" in {
      logger.debug("Change a workflow")
      assert(!whenObserved.isCompleted)
      lastEventId = controller.eventWatch.lastAddedEventId
      live.resolve(CWorkflowPath.toFile(SourceType.Json)) := ChangedWorkflowJson
      val versionId = controller.eventWatch.await[VersionAdded](after = lastEventId).head.value.event.versionId
      assert(controller.eventWatch.await[VersionedItemEvent](after = lastEventId).map(_.value) ==
        Vector(NoKey <-: VersionedItemChanged(sign(ChangedWorkflow withId CWorkflowPath ~ versionId))))
    }

    "Add an order generator" in {
      lastEventId = controller.eventWatch.lastAddedEventId
      (orderGeneratorsDir / "test.order.xml") := """<?xml version="1.0"?>
        <order job_chain="A">
          <run_time>
            <period absolute_repeat="1"/>
          </run_time>
        </order>"""
      controller.eventWatch.await[OrderAdded](_.event.workflowId.path == AWorkflowPath, after = lastEventId)
      lastEventId = controller.eventWatch.lastAddedEventId
      controller.eventWatch.await[OrderAdded](_.event.workflowId.path == AWorkflowPath, after = lastEventId)
    }

    "Replace an order generator" in {
      (orderGeneratorsDir / "test.order.xml") := """<?xml version="1.0"?>
        <order job_chain="B">
          <run_time>
            <period absolute_repeat="1"/>
          </run_time>
        </order>"""
      controller.eventWatch.await[OrderAdded](_.event.workflowId.path == BWorkflowPath, after = lastEventId)
      lastEventId = controller.eventWatch.lastAddedEventId
      controller.eventWatch.await[OrderAdded](_.event.workflowId.path == BWorkflowPath, after = lastEventId)
    }

    "Delete an order generator" in {
      delete(orderGeneratorsDir / "test.order.xml")
      controller.eventWatch.await[OrderAdded](_.event.workflowId.path == BWorkflowPath, after = lastEventId)  // TIMEOUT
    }

    "stop" in {
      assert(!whenObserved.isCompleted)
      assert(!whenObserved.isCompleted)
      stop.success(())
      whenObserved await 99.s
    }
  }

  private def writeWorkflowFile(workflowPath: WorkflowPath): Unit =
    live.resolve(workflowPath.toFile(SourceType.Json)) := TestWorkflowJson

  private def deleteFile(path: InventoryItemPath): Unit =
    delete(live.resolve(path.toFile(SourceType.Json)))
}

object ProviderTest
{
  private val logger = Logger[this.type]
  private val loginName = "ProviderTest"
  private val loginPassword = "ProviderTest-PASSWORD"
  private val testConfig = config"""
    js7.provider.directory-watch.minimum-silence = 50ms
    js7.provider.directory-watch.poll-interval = "${if (isMac) "100ms" else "300s"}"
    """

  private val agentPath = AgentPath("AGENT")
  private val AWorkflowPath = WorkflowPath("A")
  private val BWorkflowPath = WorkflowPath("B")
  private val CWorkflowPath = WorkflowPath("C")

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
            "agentPath": "AGENT",
            "executable": {
              "TYPE": "PathExecutable",
              "path": "EXECUTABLE"
            }
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
            "agentPath": "AGENT",
            "executable": {
              "TYPE": "PathExecutable",
              "path": "OTHER-EXECUTABLE"
            }
          }
        }
      ]
    }"""
  private val ChangedWorkflow = ChangedWorkflowJson.as[Workflow].orThrow
}
