package js7.tests

import js7.agent.data.commands.AgentCommand
import js7.base.auth.{SimpleUser, UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.session.SessionApi
import js7.base.time.ScalaTime._
import js7.common.configutils.Configs._
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.time.WaitForCondition.waitForCondition
import js7.controller.RunningController
import js7.controller.client.{AkkaHttpControllerApi, HttpControllerApi}
import js7.controller.data.ControllerCommand.{ReleaseEvents, TakeSnapshot}
import js7.controller.data.events.ControllerEvent
import js7.core.command.CommandMeta
import js7.core.event.journal.files.JournalFiles.listJournalFiles
import js7.data.agent.AgentRefPath
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.problems.UserIsNotEnabledToReleaseEventsProblem
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.ReleaseEventsTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ReleaseEventsTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  protected val agentRefPaths = TestAgentRefPath :: Nil
  protected val inventoryItems = TestWorkflow :: Nil
  override protected val controllerConfig = config"""
    js7.journal.users-allowed-to-release-events = [ "A", "B" ]
    js7.auth.users {
      A = "plain:PASSWORD"
      B = "plain:PASSWORD"
      X = "plain:PASSWORD"
    }
    js7.journal.release-events-delay = 0s
    js7.controller.agent-driver.release-events-period = 0ms
    """

  "ReleaseEvents" in {
    for ((_, tree) <- directoryProvider.agentToTree) {
      tree.writeExecutable(TestExecutablePath, script(0.s))
    }

    directoryProvider.run { (controller, _) =>
      controller.eventWatch.await[ControllerEvent.ControllerReady]()
      controller.runOrder(aOrder)
    }

    def controllerJournalFiles = listJournalFiles(directoryProvider.controller.dataDir / "state" / "controller")
    def agentJournalFiles = listJournalFiles(directoryProvider.agents(0).dataDir / "state" / "controller-Controller")

    def assertControllerJournalFileCount(n: Int): Unit = {
      waitForCondition(9.s, 10.ms) { controllerJournalFiles.size == n }
      assert(controllerJournalFiles.size == n)
    }

    assertControllerJournalFileCount(2)
    assert(agentJournalFiles.size == 2)

    directoryProvider.run { case (controller, Seq(agent)) =>
      import controller.eventWatch.{lastFileTornEventId, tornEventId}

      val finished = controller.eventWatch.await[OrderFinished](predicate = _.key == aOrder.id)
      assert(finished.size == 1)
      assertControllerJournalFileCount(3)
      assert(agentJournalFiles.size <= 3)

      val a = newApi(controller, aUserAndPassword)
      val b = newApi(controller, bUserAndPassword)

      locally {
        val x = newApi(controller, xUserAndPassword)
        val result = x.httpClient.liftProblem(x.executeCommand(ReleaseEvents(finished.head.eventId))).await(99.s)
        assert(result.left.toOption.exists(_ is UserIsNotEnabledToReleaseEventsProblem))
      }

      a.executeCommand(ReleaseEvents(finished.head.eventId)).await(99.s)
      assertControllerJournalFileCount(3)

      b.executeCommand(ReleaseEvents(finished.head.eventId)).await(99.s)
      assertControllerJournalFileCount(2)

      // Controller sends ReleaseEvents after some events from Agent have arrived. So we start an order.
      controller.executeCommandAsSystemUser(TakeSnapshot).await(99.s).orThrow
      val bAdded = controller.runOrder(bOrder).head.eventId
      assertControllerJournalFileCount(3)

      controller.executeCommandAsSystemUser(TakeSnapshot).await(99.s).orThrow
      assertControllerJournalFileCount(4)

      controller.runOrder(cOrder)
      controller.executeCommandAsSystemUser(TakeSnapshot).await(99.s).orThrow
      assertControllerJournalFileCount(5)

      b.executeCommand(ReleaseEvents(bAdded)).await(99.s)
      assertControllerJournalFileCount(5)

      a.executeCommand(ReleaseEvents(lastFileTornEventId)).await(99.s)
      assertControllerJournalFileCount(3)
      assert(tornEventId <= bAdded)

      b.executeCommand(ReleaseEvents(lastFileTornEventId)).await(99.s)
      assertControllerJournalFileCount(1)

      // TakeSnapshot and KeepSnapshot on last event written should tear this event
      controller.executeCommandAsSystemUser(TakeSnapshot).await(99.s).orThrow
      assert(tornEventId < lastFileTornEventId)
      a.executeCommand(ReleaseEvents(lastFileTornEventId)).await(99.s)
      b.executeCommand(ReleaseEvents(lastFileTornEventId)).await(99.s)
      waitForCondition(5.s, 10.ms) { tornEventId == lastFileTornEventId }
      assert(tornEventId == lastFileTornEventId)

      // Agent's journal file count should be 1 after TakeSnapshot and after Controller has read all events
      agent.executeCommand(AgentCommand.TakeSnapshot, CommandMeta(SimpleUser(UserId("Controller"))))
        .await(99.s).orThrow
      waitForCondition(5.s, 10.ms) { agentJournalFiles.size == 2 }
      assert(agentJournalFiles.size == 2)
      controller.runOrder(dOrder)
      waitForCondition(5.s, 10.ms) { agentJournalFiles.size == 1 }
      assert(agentJournalFiles.size == 1)
    }
  }
}

private object ReleaseEventsTest
{
  private val aUserAndPassword = UserAndPassword(UserId("A"), SecretString("PASSWORD"))
  private val bUserAndPassword = UserAndPassword(UserId("B"), SecretString("PASSWORD"))
  private val xUserAndPassword = UserAndPassword(UserId("X"), SecretString("PASSWORD"))
  private val TestAgentRefPath = AgentRefPath("/agent-111")
  private val TestExecutablePath = ExecutablePath(s"/TEST$sh")
  private val TestWorkflow = Workflow.of(WorkflowPath("/test"),
    Execute(WorkflowJob(TestAgentRefPath, TestExecutablePath)))
  private val aOrder = FreshOrder(OrderId("🔵"), TestWorkflow.id.path)
  private val bOrder = FreshOrder(OrderId("🔶"), TestWorkflow.id.path)
  private val cOrder = FreshOrder(OrderId("⭕️"), TestWorkflow.id.path)
  private val dOrder = FreshOrder(OrderId("🔺"), TestWorkflow.id.path)

  private def newApi(controller: RunningController, credentials: UserAndPassword): HttpControllerApi = {
    val api = new TestApi(controller, credentials)
    api.loginUntilReachable() await 99.s
    api
  }

  private class TestApi(controller: RunningController, protected val credentials: UserAndPassword)
  extends AkkaHttpControllerApi(controller.localUri, Some(credentials), controller.actorSystem, name = "RunningController")
  with SessionApi.HasUserAndPassword
}
