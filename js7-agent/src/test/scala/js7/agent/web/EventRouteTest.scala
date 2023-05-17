package js7.agent.web

import akka.actor.ActorSystem
import js7.agent.client.AgentClient
import js7.agent.data.commands.AgentCommand.{CoupleController, DedicateAgentDirector, ReleaseEvents, TakeSnapshot}
import js7.agent.data.event.AgentEvent.AgentReady
import js7.agent.tests.AgentTester
import js7.agent.tests.TestAgentDirectoryProvider.*
import js7.agent.web.EventRouteTest.*
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.Closer.syntax.*
import js7.data.agent.Problems.{AgentPathMismatchProblem, AgentRunIdMismatchProblem}
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.data.event.{Event, EventId, EventRequest, EventSeqTornProblem, JournalEvent, JournalId}
import js7.data.problems.UnknownEventIdProblem
import js7.data.subagent.SubagentId
import js7.journal.files.JournalFiles.listJournalFiles
import monix.execution.Scheduler.Implicits.traced

/**
  * @author Joacim Zschimmer
  */
final class EventRouteTest extends OurTestSuite with AgentTester
{
  protected val akkaAskTimeout = 99.s

  implicit private lazy val actorSystem: ActorSystem = agent.actorSystem
  private val agentClient = AgentClient(agent.localUri, Some(TestUserAndPassword)).closeWithCloser
  private var agentRunId: AgentRunId = _
  private var eventId = EventId.BeforeFirst
  private var snapshotEventId = EventId.BeforeFirst

  "(Login)"  in {
    agentClient.login().await(99.s)
  }

  "(DedicateAgentDirector)" in {
    val DedicateAgentDirector.Response(agentRunId, _) = agentClient
      .commandExecute(
        DedicateAgentDirector(Seq(SubagentId("SUBAGENT")), controllerId, agentPath))
      .await(99.s).orThrow
    this.agentRunId = agentRunId
  }

  "Request events after known EventId" in {
    val Right(observable) = agentClient
      .eventObservable(
        EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(0.s)))
      .await(99.s)
    assert(observable.headL.await(99.s).eventId > EventId.BeforeFirst)
  }

  "AgentReady" in {
    val Right(observable) = agentClient
      .eventObservable(
        EventRequest[Event](Set(classOf[AgentReady]), after = EventId.BeforeFirst, timeout = Some(0.s)))
      .await(99.s)
    eventId = observable.lastL.await(99.s).eventId
  }

  "Requesting events after unknown EventId returns Torn" in {
    // When Controller requests events, the requested EventId (after=) must be known
    val checked = agentClient.eventObservable(EventRequest.singleClass[Event](after = 1L))
      .await(99.s)
    assert(checked == Left(EventSeqTornProblem(requestedAfter = 1, tornEventId = 0)))
  }

  "Login again"  in {
    agentClient.logout().await(99.s)
    agentClient.login().await(99.s)
  }

  "Recoupling with changed AgentRunId or different AgentPath fails" in {
    assert(agentClient.commandExecute(CoupleController(agentPath, AgentRunId(JournalId.random()), eventId))
      .await(99.s) == Left(AgentRunIdMismatchProblem(agentPath)))
    assert(agentClient.commandExecute(CoupleController(AgentPath("OTHER"), agentRunId, eventId))
      .await(99.s) == Left(AgentPathMismatchProblem(AgentPath("OTHER"), agentPath)))
    assert(agentClient
      .eventObservable(
        EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(99.s)))
      .await(99.s)
      .orThrow
      .headL
      .await(99.s)
      .eventId > EventId.BeforeFirst)
  }

  "Recoupling fails if Agent's last events has been deleted" in {
    // Take snapshot, then delete old journal files
    snapshotEventId = eventId
    agentClient.commandExecute(TakeSnapshot).await(99.s).orThrow
    val stampedEvents = agentClient
      .eventObservable(EventRequest.singleClass[Event](after = eventId))
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
      .eventObservable(EventRequest.singleClass[Event](after = eventId))
      .await(99.s)
      .orThrow
      .lastL
      .await(99.s)
      .eventId

    // Wait until ReleaseEvents takes effect (otherwise CoupleController may succeed or
    // fail with watch.ClosedException)
    waitForCondition(9.s, 10.ms) { journalFiles.head.fileEventId > EventId.BeforeFirst }

    assert(agentClient.commandExecute(CoupleController(agentPath, agentRunId, EventId.BeforeFirst))
      .await(99.s) == Left(UnknownEventIdProblem(EventId.BeforeFirst)))
  }

  "Recoupling with Controller's last events deleted fails" in {
    val newerEventId = eventId + 1  // Assuming that no further Event has been emitted
    assert(agentClient.commandExecute(CoupleController(agentPath, agentRunId, newerEventId))
      .await(99.s) == Left(UnknownEventIdProblem(newerEventId)))

    val unknownEventId = EventId(1)  // Assuming this is EventId has not been emitted
    assert(agentClient.commandExecute(CoupleController(agentPath, agentRunId, unknownEventId))
      .await(99.s) == Left(UnknownEventIdProblem(unknownEventId)))
  }

  "Recouple" in {
    agentClient.commandExecute(CoupleController(agentPath, agentRunId, eventId)).await(99.s).orThrow
  }

  "Continue fetching events" in {
    val events = agentClient
      .eventObservable(EventRequest.singleClass[Event](after = eventId))
        .map(_.orThrow)
        .flatMap(_.toListL)
        .await(99.s)
    assert(events.isEmpty)
  }

  "Torn EventSeq" in {
    assert(agentClient
      .eventObservable(EventRequest.singleClass[Event](after = EventId.BeforeFirst))
      .await(99.s)
      == Left(EventSeqTornProblem(EventId.BeforeFirst, tornEventId = snapshotEventId)))
  }

  "Future event (not used by Controller)" in {
    // Controller does not use this feature provided by GenericEventRoute. We test anyway.
    val futureEventId = eventId + 1
    val events = agentClient
      .eventObservable(EventRequest.singleClass[Event](after = futureEventId))
      .map(_.orThrow)
      .flatMap(_.toListL)
      .await(99.s)
    assert(events.isEmpty)
  }
}

object EventRouteTest {
  private val agentPath = AgentPath("AGENT")
  private val controllerId = ControllerId("CONTROLLER")
}
