package js7.agent.web

import js7.agent.client.AgentClient
import js7.agent.data.Problems.{AgentNotCreatedProblem, AgentPathMismatchProblem, AgentRunIdMismatchProblem}
import js7.agent.data.commands.AgentCommand.{CoupleController, CreateAgent, ReleaseEvents, TakeSnapshot}
import js7.agent.data.event.AgentEvent.AgentReady
import js7.agent.tests.AgentTester
import js7.agent.tests.TestAgentDirectoryProvider._
import js7.agent.web.EventRouteTest._
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.problem.Checked._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.Closer.syntax._
import js7.common.guice.GuiceImplicits._
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, JournalEvent, JournalId, TearableEventSeq}
import js7.data.problems.UnknownEventIdProblem
import js7.journal.files.JournalFiles.listJournalFiles
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class EventRouteTest extends AnyFreeSpec with AgentTester
{
  protected val akkaAskTimeout = 99.s

  implicit private lazy val scheduler = agent.injector.instance[Scheduler]
  implicit private lazy val actorSystem = agent.actorSystem
  private val agentClient = AgentClient(agent.localUri, Some(TestUserAndPassword)).closeWithCloser
  private var agentRunId: AgentRunId = _
  private var eventId = EventId.BeforeFirst
  private var snapshotEventId = EventId.BeforeFirst

  "(Login)"  in {
    agentClient.login().await(99.s)
  }

  "Requesting events of unregistered Controller" in {
    assert(agentClient.events(EventRequest.singleClass[Event](after = 1)).await(99.s) ==
      Left(AgentNotCreatedProblem))
  }

  "(CreateAgent)" in {
    val CreateAgent.Response(agentRunId) = agentClient.commandExecute(CreateAgent(agentPath, controllerId))
      .await(99.s).orThrow
    this.agentRunId = agentRunId
  }

  "Request events after known EventId" in {
    val Right(EventSeq.NonEmpty(events)) = agentClient.events(
      EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(99.s))).await(99.s)
    assert(events.head.eventId > EventId.BeforeFirst)
  }

  "AgentReady" in {
    val Right(EventSeq.NonEmpty(events)) = agentClient.events(
      EventRequest[Event](Set(classOf[AgentReady]), after = EventId.BeforeFirst, timeout = Some(99.s))).await(99.s)
    eventId = events.last.eventId
  }

  "Requesting events after unknown EventId returns Torn" in {
    // When Controller requests events, the requested EventId (after=) must be known
    val Right(TearableEventSeq.Torn(0)) = agentClient.events(EventRequest.singleClass[Event](after = 1L)).await(99.s)
  }

  "Login again"  in {
    agentClient.logout().await(99.s)
    agentClient.login().await(99.s)
  }

  "Recoupling with changed AgentRunId or different AgentPath fails" in {
    assert(agentClient.commandExecute(CoupleController(agentPath, AgentRunId(JournalId.random()), eventId)).await(99.s) ==
      Left(AgentRunIdMismatchProblem(agentPath)))
    assert(agentClient.commandExecute(CoupleController(AgentPath("OTHER"), agentRunId, eventId)).await(99.s) ==
      Left(AgentPathMismatchProblem(AgentPath("OTHER"), agentPath)))
    val Right(EventSeq.NonEmpty(events)) = agentClient.events(
      EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(99.s))).await(99.s)
    assert(events.head.eventId > EventId.BeforeFirst)
  }

  "Recoupling fails if Agent's last events has been deleted" in {
    // Take snapshot, then delete old journal files
    snapshotEventId = eventId
    agentClient.commandExecute(TakeSnapshot).await(99.s).orThrow
    val Right(EventSeq.NonEmpty(stampedEvents)) = agentClient.events(EventRequest.singleClass[Event](after = eventId)).await(99.s)
    assert(stampedEvents.size == 1)
    assert(stampedEvents.head.value.event == JournalEvent.SnapshotTaken)
    eventId = stampedEvents.head.eventId

    def journalFiles = listJournalFiles(agentConfiguration.stateDirectory / "agent")
    assert(journalFiles.head.afterEventId == EventId.BeforeFirst)

    // Remove obsolete journal files
    agentClient.commandExecute(ReleaseEvents(eventId)).await(99.s).orThrow
    // Await JournalEventsReleased
    eventId = agentClient.events(EventRequest.singleClass[Event](after = eventId)).await(99.s).orThrow
      .asInstanceOf[EventSeq.NonEmpty[Seq, AnyKeyedEvent]].stamped.last.eventId

    // Wait until ReleaseEvents takes effect (otherwise CoupleController may succeed or fail with watch.ClosedException)
    waitForCondition(9.s, 10.ms) { journalFiles.head.afterEventId > EventId.BeforeFirst }

    assert(agentClient.commandExecute(CoupleController(agentPath, agentRunId, EventId.BeforeFirst)).await(99.s) ==
      Left(UnknownEventIdProblem(EventId.BeforeFirst)))
  }

  "Recoupling with Controller's last events deleted fails" in {
    val newerEventId = eventId + 1  // Assuming that no further Event has been emitted
    assert(agentClient.commandExecute(CoupleController(agentPath, agentRunId, newerEventId)).await(99.s) ==
      Left(UnknownEventIdProblem(newerEventId)))

    val unknownEventId = EventId(1)  // Assuming this is EventId has not been emitted
    assert(agentClient.commandExecute(CoupleController(agentPath, agentRunId, unknownEventId)).await(99.s) ==
      Left(UnknownEventIdProblem(unknownEventId)))
  }

  "Recouple" in {
    agentClient.commandExecute(CoupleController(agentPath, agentRunId, eventId)).await(99.s).orThrow
  }

  "Continue fetching events" in {
    val Right(EventSeq.Empty(lastEventId)) = agentClient.events(EventRequest.singleClass[Event](after = eventId)).await(99.s)
    assert(lastEventId == eventId)
  }

  "Torn EventSeq" in {
    val Right(TearableEventSeq.Torn(tornEventId)) =
      agentClient.events(EventRequest.singleClass[Event](after = EventId.BeforeFirst)).await(99.s)
    assert(tornEventId == snapshotEventId)
  }

  "Future event (not used by Controller)" in {
    // Controller does not use this feature provided by GenericEventRoute. We test anyway.
    val futureEventId = eventId + 1
    val Right(EventSeq.Empty(lastEventId)) =
      agentClient.events(EventRequest.singleClass[Event](after = futureEventId)).await(99.s)
    assert(lastEventId == eventId)
  }
}

object EventRouteTest {
  private val agentPath = AgentPath("AGENT")
  private val controllerId = ControllerId("CONTROLLER")
}