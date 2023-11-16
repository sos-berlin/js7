package js7.tests

import js7.agent.data.commands.AgentCommand
import js7.base.auth.{SimpleUser, UserAndPassword, UserId}
import js7.base.configutils.Configs.*
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.process.Processes.ShellFileExtension as sh
import js7.base.problem.Checked.Ops
import js7.base.session.SessionApi
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.controller.client.{HttpControllerApi, PekkoHttpControllerApi}
import js7.core.command.CommandMeta
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.{ReleaseEvents, TakeSnapshot}
import js7.data.controller.ControllerEvent
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.problems.UserIsNotEnabledToReleaseEventsProblem
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.journal.files.JournalFiles.listJournalFiles
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.ReleaseEventsTest.*
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.{DirectoryProviderForScalaTest, TestController}
import monix.execution.Scheduler.Implicits.traced

/**
  * @author Joacim Zschimmer
  */
final class ReleaseEventsTest extends OurTestSuite with DirectoryProviderForScalaTest
{
  protected val agentPaths = TestAgentPath :: Nil
  protected val items = Seq(TestWorkflow)
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
    for ((_, env) <- directoryProvider.agentToEnv) {
      env.writeExecutable(TestPathExecutable, script(0.s))
    }

    directoryProvider.run { (controller, _) =>
      controller.eventWatch.await[ControllerEvent.ControllerReady]()
      controller.runOrder(aOrder)
    }

    def controllerJournalFiles = listJournalFiles(directoryProvider.controllerEnv.dataDir / "state" / "controller")
    def agentJournalFiles = listJournalFiles(directoryProvider.agentEnvs(0).dataDir / "state" / "agent")

    def assertControllerJournalFileCount(n: Int): Unit = {
      awaitAndAssert { controllerJournalFiles.size == n }
    }

    assertControllerJournalFileCount(2)
    assert(agentJournalFiles.size == 2)

    directoryProvider.run { case (controller, Seq(agent)) =>
      import controller.eventWatch.{lastFileEventId, tornEventId}

      val finished = controller.eventWatch.await[OrderFinished](predicate = _.key == aOrder.id)
      assert(finished.size == 1)
      assertControllerJournalFileCount(3)
      assert(agentJournalFiles.size <= 3)

      val a = newApi(controller, aUserAndPassword)
      val b = newApi(controller, bUserAndPassword)

      locally {
        val x = newApi(controller, xUserAndPassword)
        val result = x.httpClient.liftProblem(x.executeCommand(ReleaseEvents(controller.eventWatch.lastAddedEventId))).await(99.s)
        assert(result.left.toOption.exists(_ is UserIsNotEnabledToReleaseEventsProblem))
      }

      a.executeCommand(ReleaseEvents(controller.eventWatch.lastAddedEventId)).await(99.s)
      assertControllerJournalFileCount(3)

      b.executeCommand(ReleaseEvents(controller.eventWatch.lastAddedEventId)).await(99.s)
      assertControllerJournalFileCount(1)

      // Controller sends ReleaseEvents after some events from Agent have arrived. So we start an order.
      controller.api.executeCommand(TakeSnapshot).await(99.s).orThrow
      val bAdded = controller.runOrder(bOrder).head.eventId
      assertControllerJournalFileCount(2)

      controller.api.executeCommand(TakeSnapshot).await(99.s).orThrow
      assertControllerJournalFileCount(3)

      controller.runOrder(cOrder)
      controller.api.executeCommand(TakeSnapshot).await(99.s).orThrow
      assertControllerJournalFileCount(4)

      b.executeCommand(ReleaseEvents(bAdded)).await(99.s)
      assertControllerJournalFileCount(4)

      a.executeCommand(ReleaseEvents(lastFileEventId)).await(99.s)
      assertControllerJournalFileCount(3)
      assert(tornEventId <= bAdded)

      b.executeCommand(ReleaseEvents(lastFileEventId)).await(99.s)
      assertControllerJournalFileCount(1)

      // TakeSnapshot and KeepSnapshot on last event written should tear this event
      controller.api.executeCommand(TakeSnapshot).await(99.s).orThrow
      assert(tornEventId < lastFileEventId)
      a.executeCommand(ReleaseEvents(lastFileEventId)).await(99.s)
      b.executeCommand(ReleaseEvents(lastFileEventId)).await(99.s)
      awaitAndAssert { tornEventId == lastFileEventId }

      // Agent's journal file count should be 1 after TakeSnapshot and after Controller has read all events
      agent.executeCommand(AgentCommand.TakeSnapshot, CommandMeta(SimpleUser(UserId("Controller"))))
        .await(99.s).orThrow
      awaitAndAssert(5.s) { agentJournalFiles.size == 2 }
      controller.runOrder(dOrder)
      awaitAndAssert(5.s) { agentJournalFiles.size == 1 }
      assert(agentJournalFiles.size == 1)
    }
  }
}

private object ReleaseEventsTest
{
  private val aUserAndPassword = UserAndPassword(UserId("A"), SecretString("PASSWORD"))
  private val bUserAndPassword = UserAndPassword(UserId("B"), SecretString("PASSWORD"))
  private val xUserAndPassword = UserAndPassword(UserId("X"), SecretString("PASSWORD"))
  private val TestAgentPath = AgentPath("agent-111")
  private val TestPathExecutable = RelativePathExecutable(s"TEST$sh")
  private val TestWorkflow = Workflow.of(WorkflowPath("test"),
    Execute(WorkflowJob(TestAgentPath, TestPathExecutable)))
  private val aOrder = FreshOrder(OrderId("🟦"), TestWorkflow.id.path)
  private val bOrder = FreshOrder(OrderId("🔶"), TestWorkflow.id.path)
  private val cOrder = FreshOrder(OrderId("🔻"), TestWorkflow.id.path)
  private val dOrder = FreshOrder(OrderId("🔺"), TestWorkflow.id.path)

  private def newApi(controller: TestController, credentials: UserAndPassword): HttpControllerApi = {
    val api = new TestApi(controller, credentials)
    api.loginUntilReachable() await 99.s
    api
  }

  private class TestApi(controller: TestController, protected val credentials: UserAndPassword)
  extends PekkoHttpControllerApi(controller.localUri, Some(credentials), controller.actorSystem, name = "RunningController")
  with SessionApi.HasUserAndPassword
}
