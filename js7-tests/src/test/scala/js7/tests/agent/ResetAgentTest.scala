package js7.tests.agent

import java.nio.file.Files.exists
import js7.agent.RunningAgent
import js7.agent.data.Problems.AgentAlreadyDedicatedProblem
import js7.base.auth.Admission
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.touchFile
import js7.base.problem.Problem
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax._
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResources
import js7.data.Problems.AgentResetProblem
import js7.data.agent.AgentRefStateEvent.{AgentCoupled, AgentCouplingFailed, AgentDedicated, AgentReset}
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.controller.ControllerCommand.ResetAgent
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddOrChangeSimple, AddVersion}
import js7.data.item.VersionId
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCatched, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderLockAcquired, OrderLockReleased, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.workflow.instructions.{Fork, LockInstruction, TryInstruction}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import js7.proxy.ControllerApi
import js7.tests.agent.ResetAgentTest._
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.{ControllerAgentForScalaTest, DirectoryProvider}
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class ResetAgentTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """ withFallback super.agentConfig

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(simpleWorkflow, lockWorkflow, forkingWorkflow, lock, jobResource)

  private var myAgent: RunningAgent = null

  "ResetAgent while a locking order is executed" in {
    myAgent = agent
    val orderId = OrderId("RESET-AGENT-1")
    controllerApi.addOrder(FreshOrder(orderId, lockWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == orderId)

    controllerApi.executeCommand(ResetAgent(agentPath)).await(99.s).orThrow
    myAgent.terminated.await(99.s)
    eventWatch.await[OrderTerminated](_.key == orderId)
    assert(eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
      OrderAdded(lockWorkflow.id),
      OrderMoved(Position(0) / "try+0" % 0),
      OrderStarted,
      OrderLockAcquired(lock.path),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted,
      OrderProcessed(Outcome.Disrupted(AgentResetProblem(agentPath))),
      OrderDetached,
      OrderLockReleased(lock.path),
      OrderFailed(
        Position(0) / "try+0" % 0,
        Some(Outcome.Disrupted(AgentResetProblem(agentPath))))))

      barrier.flatMap(_.tryPut(())).runSyncUnsafe()
  }

  "Run another order" in {
    val orderId = OrderId("RESET-AGENT-2")
    controllerApi.addOrder(FreshOrder(orderId, lockWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderAttachable](_.key == orderId)

    barrier.flatMap(_.tryPut(())).runSyncUnsafe()
    myAgent = directoryProvider.startAgent(agentPath) await 99.s
    eventWatch.await[OrderTerminated](_.key == orderId)
    assert(eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
      OrderAdded(lockWorkflow.id),
      OrderMoved(Position(0) / "try+0" % 0),
      OrderStarted,
      OrderLockAcquired(lock.path),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "try+0" % 0 / "lock" % 1),
      OrderDetachable,
      OrderDetached,
      OrderLockReleased(lock.path),
      OrderMoved(Position(1)),
      OrderFinished))
  }

  "ReseAgent while a forking order is executed" in {
    val orderId = OrderId("FORKING")
    val childOrderId = orderId / "FORK"
    controllerApi.addOrder(FreshOrder(orderId, forkingWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == childOrderId)

    controllerApi.executeCommand(ResetAgent(agentPath)).await(99.s).orThrow
    myAgent.terminated.await(99.s)

    eventWatch.await[OrderFailedInFork](_.key == childOrderId)
    assert(eventWatch.keyedEvents[OrderEvent](childOrderId) == Seq(
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted,
      OrderProcessed(Outcome.Disrupted(AgentResetProblem(agentPath))),
      OrderDetached,
      OrderFailedInFork(
        Position(0) / "try+0" % 0 / "fork+FORK" % 0,
        Some(Outcome.Disrupted(AgentResetProblem(agentPath))))))

    eventWatch.await[OrderTerminated](_.key == orderId)
    assert(eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
      OrderAdded(forkingWorkflow.id),
      OrderMoved(Position(0) / "try+0" % 0),
      OrderStarted,
      OrderForked(Vector(OrderForked.Child("FORK", childOrderId))),
      OrderJoined(Outcome.Failed(None,Map())),
      OrderCatched(Position(0) / "catch+0" % 0),
      OrderMoved(Position(1)),
      OrderFinished))

    barrier.flatMap(_.tryPut(())).runSyncUnsafe()
  }

  "ResetAgent when Agent is reset already" in {
    val checked = controllerApi.executeCommand(ResetAgent(agentPath)).await(99.s)
    val possibleProblems = Set(
      Problem("AgentRef is already in state 'Resetting'"),
      Problem("AgentRef is already in state 'Reset'"))
    assert(checked.left.exists(possibleProblems.contains))
  }

  "Simulate journal deletion at restart" in {
    var eventId = eventWatch.lastAddedEventId
    myAgent = directoryProvider.startAgent(agentPath) await 99.s
    eventWatch.await[AgentDedicated](after = eventId)
    sleep(100.ms)

    eventId = eventWatch.lastAddedEventId
    controllerApi.executeCommand(ResetAgent(agentPath)).await(99.s).orThrow
    myAgent.terminated.await(99.s)

    // Create some file to let it look like the Agent could not delete the journal
    val stateDir = directoryProvider.agentToTree(agentPath).stateDir
    val markerFile = stateDir / "agent-DELETE!"
    touchFile(markerFile)
    val journalFile = stateDir / "agent--0.journal"
    val garbageFile = stateDir / "agent--0.journal~GARBAGE"
    journalFile := "Would crash but will be deleted"
    touchFile(garbageFile)

    myAgent = directoryProvider.startAgent(agentPath) await 99.s
    eventWatch.await[AgentReset](after = eventId)
    eventWatch.await[AgentDedicated](after = eventId)

    // The restarted Agent has deleted the files (due to markerFile)
    assert(!exists(markerFile) && !exists(garbageFile))
  }

  "One last order" in {
    controller.runOrder(FreshOrder(OrderId("🔷"), simpleWorkflow.path))
  }

  "Let a second Controller steal the Agent with ResetAgent force" in {
    // The second Controller has the same ControllerId("Controller"), just because this
    // is easy to code. It is expected to work for different ControllerId, too.

    val eventId = eventWatch.lastAddedEventId
    val agentTree = directoryProvider.agents(0)
    val secondProvider = new DirectoryProvider(
      testName = Some("ResetAgentTest-second"),
      agentPaths = Nil,
      controllerConfig = config"""
        js7.auth.agents.AGENT = "${agentTree.password.string}"
      """.withFallback(controllerConfig))
    autoClosing(secondProvider) { _ =>
      secondProvider.runController() { secondController =>
        val v1 = VersionId("1")
        val secondControllerApi = new ControllerApi(
          admissionsToApiResources(Seq(Admission(
            secondController.localUri,
            Some(directoryProvider.controller.userAndPassword))
          ))(secondController.actorSystem))
        secondControllerApi.updateItems(Observable(
          AddOrChangeSimple(AgentRef(agentPath, agents(0).localUri)),
          AddOrChangeSigned(secondProvider.toSignedString(jobResource)),
          AddVersion(v1),
          AddOrChangeSigned(secondProvider.toSignedString(simpleWorkflow.withVersion(v1))))
        ).await(99.s).orThrow

        // This is not our Agent
        val problem = secondController.eventWatch.await[AgentCouplingFailed]().head.value.event.problem
        assert(problem == AgentAlreadyDedicatedProblem)

        // Simple ResetAgent does not work
        val checked = secondControllerApi.executeCommand(ResetAgent(agentPath)).await(99.s)
        assert(checked == Left(Problem("AgentRef is already in state 'Reset'")))

        // Steal this Agent with ReseAgent(force)!
        secondControllerApi.executeCommand(ResetAgent(agentPath, force = true)).await(99.s)
        myAgent.terminated.await(99.s)
        myAgent = directoryProvider.startAgent(agentPath) await 99.s

        // Now, we the Agent is ours! It is dedicated to secondController
        secondController.eventWatch.await[AgentDedicated]()

        secondController.eventWatch.await[AgentCoupled]()
        val events = secondController.runOrder(
          FreshOrder(OrderId("SECOND-CONTROLLER"), simpleWorkflow.path))
        assert(events.last.value == OrderFinished)

        // The other Controller gets errors now, because we stole the Agent:
        eventWatch.await[AgentCouplingFailed](after = eventId)

        myAgent.terminate().await(99.s)
      }
    }
  }
}

object ResetAgentTest
{
  private val agentPath = AgentPath("AGENT")
  private val lock = Lock(LockPath("LOCK"))
  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"))

  private val simpleWorkflow = Workflow(WorkflowPath("SIMPLE-WORKFLOW") ~ "INITIAL",
    Seq(EmptyJob.execute(agentPath)))

  private val lockWorkflow = Workflow(WorkflowPath("LOCK-WORKFLOW") ~ "INITIAL",
    Vector(
      TryInstruction(
        Workflow.of(
          LockInstruction(lock.path, None, Workflow.of(
            TestJob.execute(
              agentPath,
              jobResourcePaths = Seq(jobResource.path))))),
        Workflow.empty)))

  private val forkingWorkflow = Workflow(WorkflowPath("FORKING-WORKFLOW") ~ "INITIAL",
    Vector(
      TryInstruction(
        Workflow.of(
          Fork(
            Vector(
              "FORK" -> Workflow.of(
                TestJob.execute(agentPath,
                  jobResourcePaths = Seq(jobResource.path)))),
            joinIfFailed = true)),
        Workflow.empty)))

  private val barrier = MVar.empty[Task, Unit]().memoize

  private final class TestJob extends InternalJob
  {
    def toOrderProcess(step: Step) =
      OrderProcess(
        barrier
          .tapEval(_ => Task(scribe.debug("TestJob start")))
          .flatMap(_.take)
          .guaranteeCase(exitCase => Task(scribe.debug(s"TestJob $exitCase")))
          .as(Outcome.succeeded))
  }
  private object TestJob extends InternalJob.Companion[TestJob]
}
