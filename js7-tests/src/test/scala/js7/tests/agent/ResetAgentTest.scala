package js7.tests.agent

import java.nio.file.Files.exists
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.touchFile
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.Problems.AgentResetProblem
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.AgentCreated
import js7.data.controller.ControllerCommand.ResetAgent
import js7.data.job.{InternalExecutable, JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCatched, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderLockAcquired, OrderLockReleased, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, Fork, LockInstruction, TryInstruction}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import js7.tests.agent.ResetAgentTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.catnap.Semaphore
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class ResetAgentTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 5ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """ withFallback super.agentConfig

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow, forkingWorkflow, lock, jobResource)

  "ResetAgent while an order is executed" in {
    val orderId = OrderId("RESET-AGENT-1")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == orderId)
    controllerApi.executeCommand(ResetAgent(agentPath)).await(99.s).orThrow
    agent.terminated.await(99.s)
    eventWatch.await[OrderTerminated](_.key == orderId)
    assert(eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id),
      OrderMoved(Position(0) / "try+0" % 0),
      OrderStarted,
      OrderLockAcquired(lock.path),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted,
      OrderProcessed(Outcome.Disrupted(AgentResetProblem(agentPath))),
      OrderDetached,
      OrderFailed(
        Position(0) / "try+0" % 0,
        Some(Outcome.Disrupted(AgentResetProblem(agentPath))),
        lockPaths = Seq(lock.path))))

      resetSemaphore()  // Semaphore may or may not be required
  }

  "Run another order" in {
    val orderId = OrderId("RESET-AGENT-2")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
    eventWatch.await[OrderAttachable](_.key == orderId)

    semaphore.flatMap(_.release).runSyncUnsafe()
    directoryProvider.startAgent(agentPath) await 99.s
    eventWatch.await[OrderTerminated](_.key == orderId)
    assert(eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id),
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

  "fork" in {
    val orderId = OrderId("FORKING")
    val childOrderId = orderId | "FORK"
    controllerApi.addOrder(FreshOrder(orderId, forkingWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == childOrderId)
    controllerApi.executeCommand(ResetAgent(agentPath)).await(99.s).orThrow
    agent.terminated.await(99.s)

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
      OrderForked(List(OrderForked.Child("FORK", childOrderId))),
      OrderJoined(Outcome.Failed(None,Map())),
      OrderCatched(Position(0) / "catch+0" % 0),
      OrderMoved(Position(1)),
      OrderFinished))

    resetSemaphore()
  }

  "Simulate journal deletion at restart" in {
    controllerApi.executeCommand(ResetAgent(agentPath)).await(99.s).orThrow
    agent.terminated.await(99.s)

    // Create some file to let it look like the Agent could not delete the journal
    val stateDir = directoryProvider.agentToTree(agentPath).stateDir
    val markerFile = stateDir / "agent-DELETE!"
    touchFile(markerFile)
    val journalFile = stateDir / "agent--0.journal"
    val garbageFile = stateDir / "agent--0.journal~GARBAGE"
    journalFile := "Would crash but will be deleted"
    touchFile(garbageFile)

    val eventId = controller.eventWatch.lastAddedEventId
    directoryProvider.startAgent(agentPath) await 99.s
    controller.eventWatch.await[AgentCreated](after = eventId)

    // The restarted Agent has deleted the files (due to markerFile)
    assert(!exists(markerFile) && !exists(garbageFile))
  }
}

object ResetAgentTest
{
  private val agentPath = AgentPath("AGENT")
  private val lock = Lock(LockPath("LOCK"))
  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"))
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      TryInstruction(
        Workflow.of(
          LockInstruction(lock.path, None, Workflow.of(
            Execute(WorkflowJob(
              agentPath,
              InternalExecutable(classOf[SemaphoreJob].getName),
              jobResourcePaths = Seq(jobResource.path)))))),
        Workflow.empty)))

  private val forkingWorkflow = Workflow(WorkflowPath("FORKING-WORKFLOW") ~ "INITIAL",
    Vector(
      TryInstruction(
        Workflow.of(
          Fork.of(
            "FORK" -> Workflow.of(
              Execute(WorkflowJob(
                agentPath,
                InternalExecutable(classOf[SemaphoreJob].getName),
                jobResourcePaths = Seq(jobResource.path)))))),
        Workflow.empty)))

  private val semaphore = Semaphore[Task](0).memoize

  private def resetSemaphore(): Unit =
    semaphore
      .flatMap(_.count)
      .flatMap {
        case 0 => Task.unit
        case n => semaphore.flatMap(_.releaseN(n))
      }
      .runSyncUnsafe()

  final class SemaphoreJob extends InternalJob
  {
    def toOrderProcess(step: Step) =
      OrderProcess(
        semaphore.flatMap(_.acquire)
          .as(Outcome.succeeded))
  }
}
