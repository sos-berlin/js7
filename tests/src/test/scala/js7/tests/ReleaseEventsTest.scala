package js7.tests

import js7.agent.data.commands.AgentCommand
import js7.base.auth.{SimpleUser, UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.session.SessionApi
import js7.base.time.ScalaTime._
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.time.WaitForCondition.waitForCondition
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
import js7.master.RunningMaster
import js7.master.client.{AkkaHttpMasterApi, HttpMasterApi}
import js7.master.data.MasterCommand.{ReleaseEvents, TakeSnapshot}
import js7.master.data.events.MasterEvent
import js7.tests.ReleaseEventsTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.DirectoryProviderForScalaTest
import com.typesafe.config.ConfigFactory
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ReleaseEventsTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  protected val agentRefPaths = TestAgentRefPath :: Nil
  protected val fileBased = TestWorkflow :: Nil
  override protected val masterConfig = ConfigFactory.parseString(
     """jobscheduler {
       |  journal.users-allowed-to-release-events = [ "A", "B" ]
       |  auth.users {
       |    A = "plain:PASSWORD"
       |    B = "plain:PASSWORD"
       |    X = "plain:PASSWORD"
       |  }
       |  master.agent-driver.release-events-period = 0ms
       |}""".stripMargin)

  "ReleaseEvents" in {
    for ((_, tree) <- directoryProvider.agentToTree) {
      tree.writeExecutable(TestExecutablePath, script(0.s))
    }

    directoryProvider.run { (master, _) =>
      master.eventWatch.await[MasterEvent.MasterReady]()
      master.runOrder(aOrder)
    }

    def masterJournalFiles = listJournalFiles(directoryProvider.master.dataDir / "state" / "master")
    def agentJournalFiles = listJournalFiles(directoryProvider.agents(0).dataDir / "state" / "master-Master")

    def assertMasterJournalFileCount(n: Int): Unit = {
      waitForCondition(9.s, 10.ms) { masterJournalFiles.size == n }
      assert(masterJournalFiles.size == n)
    }

    assertMasterJournalFileCount(2)
    assert(agentJournalFiles.size == 2)

    directoryProvider.run { case (master, Seq(agent)) =>
      import master.eventWatch.{lastFileTornEventId, tornEventId}

      val finished = master.eventWatch.await[OrderFinished](predicate = _.key == aOrder.id)
      assert(finished.size == 1)
      assertMasterJournalFileCount(3)
      assert(agentJournalFiles.size <= 3)

      val a = newApi(master, aUserAndPassword)
      val b = newApi(master, bUserAndPassword)

      locally {
        val x = newApi(master, xUserAndPassword)
        val result = x.httpClient.liftProblem(x.executeCommand(ReleaseEvents(finished.head.eventId))).await(99.s)
        assert(result.left.toOption.exists(_ is UserIsNotEnabledToReleaseEventsProblem))
      }

      a.executeCommand(ReleaseEvents(finished.head.eventId)).await(99.s)
      assertMasterJournalFileCount(3)

      b.executeCommand(ReleaseEvents(finished.head.eventId)).await(99.s)
      assertMasterJournalFileCount(2)

      // Master sends ReleaseEvents after some events from Agent have arrived. So we start an order.
      master.executeCommandAsSystemUser(TakeSnapshot).await(99.s).orThrow
      val bAdded = master.runOrder(bOrder).head.eventId
      assertMasterJournalFileCount(3)

      master.executeCommandAsSystemUser(TakeSnapshot).await(99.s).orThrow
      assertMasterJournalFileCount(4)

      master.runOrder(cOrder)
      master.executeCommandAsSystemUser(TakeSnapshot).await(99.s).orThrow
      assertMasterJournalFileCount(5)

      b.executeCommand(ReleaseEvents(bAdded)).await(99.s)
      assertMasterJournalFileCount(5)

      a.executeCommand(ReleaseEvents(lastFileTornEventId)).await(99.s)
      assertMasterJournalFileCount(3)
      assert(tornEventId <= bAdded)

      b.executeCommand(ReleaseEvents(lastFileTornEventId)).await(99.s)
      assertMasterJournalFileCount(1)

      // TakeSnapshot and KeepSnapshot on last event written should tear this event
      master.executeCommandAsSystemUser(TakeSnapshot).await(99.s).orThrow
      assert(tornEventId < lastFileTornEventId)
      a.executeCommand(ReleaseEvents(lastFileTornEventId)).await(99.s)
      b.executeCommand(ReleaseEvents(lastFileTornEventId)).await(99.s)
      //waitForCondition(5.s, 10.ms) { tornEventId == last }
      assert(tornEventId == lastFileTornEventId)

      // Agent's journal file count should be 1 after TakeSnapshot and after Master has read all events
      agent.executeCommand(AgentCommand.TakeSnapshot, CommandMeta(SimpleUser(UserId("Master"))))
        .await(99.s).orThrow
      waitForCondition(5.s, 10.ms) { agentJournalFiles.size == 2 }
      assert(agentJournalFiles.size == 2)
      master.runOrder(dOrder)
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

  private def newApi(master: RunningMaster, credentials: UserAndPassword): HttpMasterApi = {
    val api = new TestApi(master, credentials)
    api.loginUntilReachable() await 99.s
    api
  }

  private class TestApi(master: RunningMaster, protected val credentials: UserAndPassword)
  extends AkkaHttpMasterApi(master.localUri, Some(credentials), master.actorSystem, name = "RunningMaster")
  with SessionApi.HasUserAndPassword
}
