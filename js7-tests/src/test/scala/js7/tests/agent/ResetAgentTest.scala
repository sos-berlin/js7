package js7.tests.agent

import cats.effect.IO
import cats.effect.std.Queue
import cats.effect.unsafe.IORuntime
import fs2.Stream
import java.nio.file.Files.exists
import java.time.LocalTime
import js7.agent.TestAgent
import js7.base.auth.Admission
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.touchFile
import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.{AdmissionTimeScheme, DailyPeriod, Timestamp}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.controller.client.PekkoHttpControllerApi.admissionsToApiResource
import js7.data.Problems.AgentResetProblem
import js7.data.agent.AgentRefStateEvent.{AgentCoupled, AgentCouplingFailed, AgentDedicated, AgentReset}
import js7.data.agent.Problems.AgentAlreadyDedicatedProblem
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.controller.ControllerCommand.ResetAgent
import js7.data.delegate.DelegateCouplingState
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddOrChangeSimple, AddVersion}
import js7.data.item.VersionId
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath}
import js7.data.order.OrderEvent.{LockDemand, OrderAdded, OrderAttachable, OrderAttached, OrderCaught, OrderCyclingPrepared, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderLocksAcquired, OrderLocksReleased, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStateReset, OrderTerminated}
import js7.data.order.{CycleState, FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.workflow.instructions.{Cycle, Fork, LockInstruction, Schedule, TryInstruction}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.proxy.ControllerApi
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.agent.ResetAgentTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{ControllerAgentForScalaTest, DirectoryProvider}
import scala.language.unsafeNulls

final class ResetAgentTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.subagent-driver.reset-timeout = 10s # Allow slow testing
    """.withFallback(super.agentConfig)

  protected val agentPaths = Seq(agentPath)
  protected val bareSubagentId = SubagentId("BARE-SUBAGENT")
  override def bareSubagentItems: Seq[SubagentItem] = Seq(bareSubagentItem)

  protected lazy val bareSubagentItem = SubagentItem(
    bareSubagentId, agentPath, findFreeLocalUri(),
    disabled = true/*don't use for orders for expectable events*/)

  protected val items = Seq(
    bareSubagentItem,
    simpleWorkflow, lockWorkflow, cycleWorkflow, forkingWorkflow,
    lock, jobResource, calendar)

  private var myAgent: TestAgent = null

  "ResetAgent while a locking order is executed" in:
    myAgent = agent

    val lockingOrderId = OrderId("LOCKING")
    controller.api.addOrder(FreshOrder(lockingOrderId, lockWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == lockingOrderId)

    val now = Timestamp.now
    val today = now.toIsoString.take(10)
    val tomorrow = (now + 24.h).toIsoString.take(10)
    val cycleOrderId = OrderId(s"#$today#CYCLING")
    controller.api.addOrder(FreshOrder(cycleOrderId, cycleWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderCyclingPrepared](_.key == cycleOrderId)

    execCmd:
      ResetAgent(agentPath)
    myAgent.untilTerminated.await(99.s)

    eventWatch.await[OrderTerminated](_.key == lockingOrderId)
    assert(eventWatch.eventsByKey[OrderEvent](lockingOrderId) == Seq(
      OrderAdded(lockWorkflow.id),
      OrderMoved(Position(0) / "try+0" % 0),
      OrderStarted,
      OrderLocksAcquired(List(LockDemand(lock.path))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.Disrupted(AgentResetProblem(agentPath))),
      OrderDetached,
      OrderOutcomeAdded(OrderOutcome.Disrupted(AgentResetProblem(agentPath))),
      OrderLocksReleased(List(lock.path)),
      OrderFailed(Position(0) / "try+0" % 0)))

    eventWatch.await[OrderTerminated](_.key == cycleOrderId)
    assert(eventWatch.eventsByKey[OrderEvent](cycleOrderId) == Seq(
      OrderAdded(cycleWorkflow.id),
      OrderMoved(Position(0) / "try+0" % 0),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(0) / "try+0" % 1),
      OrderCyclingPrepared(CycleState(
        Timestamp(s"${tomorrow}T00:00:00Z"), schemeIndex = 0, index = 1,
        next = Timestamp(s"${today}T23:59:59Z"))),
      OrderDetached,
      OrderStateReset,
      OrderOutcomeAdded(OrderOutcome.Disrupted(AgentResetProblem(agentPath))),
      OrderFailed(Position(0) / "try+0" % 1)))

    // The Director has terminated the BareSubagent, too
    idToAllocatedSubagent(bareSubagentId).allocatedThing.untilTerminated.await(99.s)

    barrier.flatMap(_.tryOffer(())).await(99.s)

  "Run another order" in:
    val orderId = OrderId("RESET-AGENT-2")
    controller.api.addOrder(FreshOrder(orderId, lockWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderAttachable](_.key == orderId)

    barrier.flatMap(_.tryOffer(())).await(99.s)

    myAgent = directoryProvider.startAgent(agentPath).await(99.s)
    eventWatch.await[OrderTerminated](_.key == orderId)

    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(lockWorkflow.id),
      OrderMoved(Position(0) / "try+0" % 0),
      OrderStarted,
      OrderLocksAcquired(List(LockDemand(lock.path))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(0) / "try+0" % 0 / "lock" % 1),
      OrderDetachable,
      OrderDetached,
      OrderLocksReleased(List(lock.path)),
      OrderMoved(Position(1)),
      OrderFinished()))

  "ResetAgent while a forking order is executed" in:
    val orderId = OrderId("FORKING")
    val childOrderId = orderId / "FORK"
    controller.api.addOrder(FreshOrder(orderId, forkingWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == childOrderId)

    execCmd:
      ResetAgent(agentPath)
    myAgent.untilTerminated.await(99.s)

    eventWatch.await[OrderFailedInFork](_.key == childOrderId)
    assert(eventWatch.eventsByKey[OrderEvent](childOrderId) == Seq(
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.Disrupted(AgentResetProblem(agentPath))),
      OrderDetached,
      OrderOutcomeAdded(OrderOutcome.Disrupted(AgentResetProblem(agentPath))),
      OrderFailedInFork(Position(0) / "try+0" % 0 / "fork+FORK" % 0)))

    eventWatch.await[OrderTerminated](_.key == orderId)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(forkingWorkflow.id),
      OrderMoved(Position(0) / "try+0" % 0),
      OrderStarted,
      OrderForked(Vector("FORK" -> childOrderId)),
      OrderJoined(OrderOutcome.Failed(Some(
        "Order:FORKING|FORK Disrupted(Other(AgentReset: Agent:AGENT has been reset))"))),
      OrderCaught(Position(0) / "catch+0" % 0),
      OrderMoved(Position(1)),
      OrderFinished()))

    barrier.flatMap(_.tryOffer(())).await(99.s)

  "ResetAgent when Agent is reset already" in:
    val checked = controller.api.executeCommand(ResetAgent(agentPath)).await(99.s)
    val possibleProblems = Set(
      Problem("AgentRef is already in Resetting state"),
      Problem("AgentRef is already in Reset(ResetCommand) state"))
    assert(checked.left.exists(possibleProblems.contains))

    def isResettingOrReset(s: DelegateCouplingState) = s match
      case _: DelegateCouplingState.Resetting | _: DelegateCouplingState.Reset => true
      case _ => false

    awaitAndAssert:
      isResettingOrReset(controllerState.keyTo(AgentRefState)(agentPath).couplingState)

  "Simulate journal deletion at restart" in:
    var eventId = eventWatch.lastAddedEventId
    myAgent = directoryProvider.startAgent(agentPath).await(99.s)
    eventWatch.await[AgentDedicated](after = eventId)
    eventWatch.await[AgentCoupled](after = eventId)

    eventId = eventWatch.lastAddedEventId
    execCmd:
      ResetAgent(agentPath)
    myAgent.untilTerminated.await(99.s)

    // Create some file to let it look like the Agent could not delete the journal
    val stateDir = directoryProvider.agentToEnv(agentPath).stateDir
    val markerFile = stateDir / "agent-DELETE!"
    touchFile(markerFile)
    val journalFile = stateDir / "agent--0.journal"
    val garbageFile = stateDir / "agent--0.journal~GARBAGE"
    journalFile := "Would crash but will be deleted"
    touchFile(garbageFile)

    myAgent = directoryProvider.startAgent(agentPath).await(99.s)
    eventWatch.await[AgentReset](after = eventId)
    eventWatch.await[AgentDedicated](after = eventId)

    // The restarted Agent has deleted the files (due to markerFile)
    assert(!exists(markerFile) && !exists(garbageFile))

  "ResetAgent resets Subagents, too" is pending

  "One last order" in:
    controller.runOrder(FreshOrder(OrderId("🔷"), simpleWorkflow.path))

  "Let a second Controller steal the Agent with ResetAgent force" in:
    // The second Controller has the same ControllerId("Controller"), just because this
    // is easy to code. It is expected to work for different ControllerId, too.

    val eventId = eventWatch.lastAddedEventId
    val agentEnv = directoryProvider.agentEnvs(0)
    val secondProvider = new DirectoryProvider(
      testName = Some("ResetAgentTest-second"),
      agentPaths = Nil,
      controllerConfig = config"""
        js7.auth.agents.AGENT = "${agentEnv.controllerPassword.string}"
      """.withFallback(controllerConfig))
    autoClosing(secondProvider) { _ =>
      secondProvider.runController() { secondController =>
        val v1 = VersionId("1")
        val secondControllerApi = new ControllerApi(
          admissionsToApiResource(Nel.one(Admission(
            secondController.localUri,
            Some(directoryProvider.controllerEnv.userAndPassword))
          ))(using secondController.actorSystem))
        secondControllerApi.updateItems(Stream(
          AddOrChangeSimple(AgentRef(agentPath, Seq(agentEnv.localSubagentId))),
          AddOrChangeSimple(SubagentItem(agentEnv.localSubagentId, agentPath, agents(0).localUri)),
          AddOrChangeSigned(secondProvider.toSignedString(jobResource)),
          AddVersion(v1),
          AddOrChangeSigned(secondProvider.toSignedString(simpleWorkflow.withVersion(v1))))
        ).await(99.s).orThrow

        // This is not our Agent
        val problem = secondController.eventWatch.await[AgentCouplingFailed]().head.value.event.problem
        assert(problem == AgentAlreadyDedicatedProblem)

        // Simple ResetAgent does not work
        val checked = secondControllerApi.executeCommand(ResetAgent(agentPath)).await(99.s)
        // TODO Should reject Subagent due to non-matching SubagentRunId --> check SubagentRunId!
        assert(checked == Left(Problem("AgentRef is already in Reset(Fresh) state")))

        // Steal this Agent with ReseAgent(force)!
        secondControllerApi.executeCommand(ResetAgent(agentPath, force = true)).await(99.s)
        secondControllerApi.stop.await(99.s)  // ActorSystem still alive ???
        myAgent.untilTerminated.await(99.s)

        myAgent = directoryProvider.startAgent(agentPath).await(99.s)

        // Now, the Agent is our! It is dedicated to secondController
        secondController.eventWatch.await[AgentDedicated]()

        secondController.eventWatch.await[AgentCoupled]()
        val events = secondController.runOrder(
          FreshOrder(OrderId("SECOND-CONTROLLER"), simpleWorkflow.path))
        assert(events.last.value == OrderFinished())

        // The other Controller gets errors now, because we stole the Agent:
        eventWatch.await[AgentCouplingFailed](after = eventId)

        myAgent.terminate().await(99.s)
      }
    }


object ResetAgentTest:
  private val logger = Logger[this.type]

  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val lock = Lock(LockPath("LOCK"))
  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"))
  private val calendar = Calendar.jocStandard(CalendarPath("CALENDAR"))

  private val simpleWorkflow = Workflow(WorkflowPath("SIMPLE-WORKFLOW") ~ "INITIAL",
    Seq(EmptyJob.execute(agentPath)))

  private val lockWorkflow = Workflow(WorkflowPath("LOCK-WORKFLOW") ~ "INITIAL",
    Vector(
      TryInstruction(
        Workflow.of(
          LockInstruction.single(lock.path, None, Workflow.of(
            TestJob.execute(
              agentPath,
              jobResourcePaths = Seq(jobResource.path))))),
        Workflow.empty)))

  private val cycleWorkflow = Workflow(WorkflowPath("CYCLE-WORKFLOW") ~ "INITIAL",
    Vector(
      TryInstruction(
        Workflow.of(
          EmptyJob.execute(agentPath), // Move Order to Agent
          Cycle(Schedule(Seq(Schedule.Scheme(
              AdmissionTimeScheme(Seq(
                DailyPeriod(
                  LocalTime.parse("23:59:59")/*TODO test will fail at this UTC time!!!*/,
                  1.s))),
              Schedule.Ticking(1.s)
          )))):
            Workflow.of:
              EmptyJob.execute(agentPath)),
        Workflow.empty)),
    calendarPath = Some(calendar.path))

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

  private val barrier = memoize(Queue.bounded[IO, Unit](1))

  private final class TestJob extends InternalJob:
    def toOrderProcess(step: Step) =
      OrderProcess.cancelable:
        barrier
          .flatTap(_ => IO(logger.debug("TestJob start")))
          .flatMap(_.take)
          .guaranteeCase(exitCase => IO(logger.debug(s"TestJob $exitCase")))
          .as(OrderOutcome.succeeded)

  private object TestJob extends InternalJob.Companion[TestJob]
