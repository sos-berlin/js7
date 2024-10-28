package js7.agent.web

import cats.effect.unsafe.IORuntime
import js7.agent.client.AgentClient
import js7.agent.data.commands.AgentCommand.{CoupleController, DedicateAgentDirector, ReleaseEvents, TakeSnapshot}
import js7.agent.data.event.AgentEvent.AgentReady
import js7.agent.tests.AgentTester
import js7.agent.tests.TestAgentDirectoryProvider.*
import js7.agent.web.EventRouteTest.*
import js7.base.auth.Admission
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.monixlike.MonixLikeExtensions.{headL, lastL, toListL}
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Closer.syntax.*
import js7.data.agent.Problems.{AgentPathMismatchProblem, AgentRunIdMismatchProblem}
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.{ControllerId, ControllerRunId}
import js7.data.event.{Event, EventId, EventRequest, EventSeqTornProblem, JournalEvent, JournalId}
import js7.data.problems.UnknownEventIdProblem
import js7.data.subagent.SubagentId
import js7.journal.files.JournalFiles.listJournalFiles
import js7.tester.ScalaTestUtils.awaitAndAssert
import org.apache.pekko.actor.ActorSystem
import scala.compiletime.uninitialized

/**
  * @author Joacim Zschimmer
  */
final class EventRouteTest extends OurTestSuite, AgentTester:

  private given IORuntime = ioRuntime

  protected val pekkoAskTimeout = 99.s

  implicit private lazy val actorSystem: ActorSystem = agent.actorSystem
  private lazy val agentClient = AgentClient(Admission(agent.localUri, Some(TestUserAndPassword)))
    .closeWithCloser
  private lazy val controllerRunId = ControllerRunId(JournalId.random())
  private var agentRunId: AgentRunId = uninitialized
  private var eventId = EventId.BeforeFirst
  private var snapshotEventId = EventId.BeforeFirst

  "(Login)"  in:
    agentClient.login().await(99.s)

  "(DedicateAgentDirector)" in:
    val DedicateAgentDirector.Response(agentRunId, _) =
      agentClient.repeatUntilAvailable(99.s)(
        agentClient
          .commandExecute(
            DedicateAgentDirector(Seq(SubagentId("SUBAGENT")), controllerId, controllerRunId,
              agentPath)))
        .await(99.s).orThrow

    this.agentRunId = agentRunId

  "Request events after known EventId" in:
    val Right(stream) = agentClient
      .agentEventStream:
        EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(0.s))
      .await(99.s): @unchecked
    assert(stream.headL.await(99.s).eventId > EventId.BeforeFirst)

  "AgentReady" in:
    val Right(stream) = agentClient
      .agentEventStream:
        EventRequest[Event](Set(classOf[AgentReady]), after = EventId.BeforeFirst, timeout = Some(0.s))
      .await(99.s): @unchecked
    eventId = stream.lastL.await(99.s).eventId

  "Requesting events after unknown EventId returns Torn" in:
    // When Controller requests events, the requested EventId (after=) must be known
    val checked = agentClient.agentEventStream:
      EventRequest.singleClass[Event](after = 1L, timeout = Some(0.s))
    .await(99.s)
    assert(checked == Left(EventSeqTornProblem(requestedAfter = 1, tornEventId = 0)))

  "Login again"  in:
    agentClient.logout().await(99.s)
    agentClient.login().await(99.s)

  "Recoupling with changed AgentRunId or different AgentPath fails" in:
    assert(agentClient
      .commandExecute(CoupleController(
        agentPath,
        AgentRunId(JournalId.random()),
        eventId,
        controllerRunId))
      .await(99.s) == Left(AgentRunIdMismatchProblem(agentPath)))
    assert(agentClient
      .commandExecute(CoupleController(
        AgentPath("OTHER"), agentRunId, eventId, controllerRunId))
      .await(99.s) == Left(AgentPathMismatchProblem(AgentPath("OTHER"), agentPath)))
    assert(agentClient
      .agentEventStream(
        EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(99.s)))
      .await(99.s)
      .orThrow
      .headL
      .await(99.s)
      .eventId > EventId.BeforeFirst)

  "Recoupling fails if Agent's last events has been deleted" in:
    // Take snapshot, then delete old journal files
    snapshotEventId = eventId
    agentClient.commandExecute(TakeSnapshot).await(99.s).orThrow
    val stampedEvents = agentClient
      .agentEventStream(EventRequest.singleClass[Event](after = eventId, timeout = Some(0.s)))
      .await(99.s)
      .orThrow
      .toListL
      .await(99.s)
    assert(stampedEvents.size == 1)
    assert(stampedEvents.head.value.event == JournalEvent.SnapshotTaken)
    eventId = stampedEvents.head.eventId

    def journalFiles = listJournalFiles(agentConfiguration.stateDirectory / "agent")
    assert(journalFiles.head.fileEventId == EventId.BeforeFirst)

    // Remove obsolete journal files
    agentClient.commandExecute(ReleaseEvents(eventId)).await(99.s).orThrow
    // Await JournalEventsReleased
    eventId = agentClient
      .agentEventStream(EventRequest.singleClass[Event](after = eventId, timeout = Some(0.s)))
      .await(99.s)
      .orThrow
      .lastL
      .await(99.s)
      .eventId

    // Wait until ReleaseEvents takes effect (otherwise CoupleController may succeed or
    // fail with watch.ClosedException)
    awaitAndAssert { journalFiles.head.fileEventId > EventId.BeforeFirst }

    agentClient
      .commandExecute(CoupleController(
        agentPath, agentRunId, EventId.BeforeFirst, controllerRunId))
      .await(99.s)
      .match
        case Left(problem) if problem is UnknownEventIdProblem => succeed
        case o => fail(s"Unexpected response: $o")

  "Recoupling with Controller's last events deleted fails" in:
    val newerEventId = eventId + 1  // Assuming that no further Event has been emitted
    agentClient.commandExecute(CoupleController(agentPath, agentRunId, newerEventId, controllerRunId))
      .await(99.s)
      .match
        case Left(problem) if problem is UnknownEventIdProblem =>
        case o => fail(s"Unexpected response: $o")

    val unknownEventId = EventId(1)  // Assuming this is EventId has not been emitted
    agentClient.commandExecute(CoupleController(agentPath, agentRunId, unknownEventId, controllerRunId))
      .await(99.s)
      .match
        case Left(problem) if problem is UnknownEventIdProblem => succeed
        case o => fail(s"Unexpected response: $o")


  "Recouple with wrong ControllerRunId" in:
    assert(agentClient
      .commandExecute(CoupleController(agentPath, agentRunId, eventId, ControllerRunId.empty))
      .await(99.s).isLeft)

  "Recouple" in:
    agentClient.commandExecute(CoupleController(agentPath, agentRunId, eventId, controllerRunId)).await(99.s).orThrow

  "Continue fetching events" in:
    val events = agentClient
      .agentEventStream(EventRequest.singleClass[Event](after = eventId, timeout = Some(0.s)))
        .map(_.orThrow)
        .flatMap(_.toListL)
        .await(99.s)
    assert(events.isEmpty)

  "Torn EventSeq" in:
    assert(agentClient
      .agentEventStream(EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(0.s)))
      .await(99.s)
      == Left(EventSeqTornProblem(EventId.BeforeFirst, tornEventId = snapshotEventId)))

  "Future event (not used by Controller)" in:
    // Controller does not use this feature provided by GenericEventRoute. We test anyway.
    val futureEventId = eventId + 1
    val events = agentClient
      .agentEventStream(EventRequest.singleClass[Event](after = futureEventId, timeout = Some(0.s)))
      .map(_.orThrow)
      .flatMap(_.toListL)
      .await(99.s)
    assert(events.isEmpty)


object EventRouteTest:
  private val agentPath = AgentPath("AGENT")
  private val controllerId = ControllerId("CONTROLLER")
