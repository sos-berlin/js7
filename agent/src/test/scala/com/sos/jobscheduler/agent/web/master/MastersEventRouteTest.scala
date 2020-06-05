package js7.agent.web.master

import js7.agent.client.AgentClient
import js7.agent.data.Problems.{DuplicateAgentRef, MasterAgentMismatch, UnknownMaster}
import js7.agent.data.commands.AgentCommand.{CoupleMaster, RegisterAsMaster, ReleaseEvents, TakeSnapshot}
import js7.agent.data.event.AgentMasterEvent.AgentReadyForMaster
import js7.agent.tests.AgentTester
import js7.agent.tests.TestAgentDirectoryProvider._
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.utils.Closer.syntax._
import js7.common.guice.GuiceImplicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.data.agent.{AgentRefPath, AgentRunId}
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, JournalEvent, JournalId, TearableEventSeq}
import js7.data.master.MasterId
import js7.data.problems.UnknownEventIdProblem
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MastersEventRouteTest extends AnyFreeSpec with AgentTester
{
  protected val akkaAskTimeout = 99.s

  implicit private lazy val scheduler = agent.injector.instance[Scheduler]
  implicit private lazy val actorSystem = agent.actorSystem
  private val agentClient = AgentClient(agent.localUri, Some(TestUserAndPassword)).closeWithCloser
  private val agentRefPath = AgentRefPath("/AGENT")
  private var agentRunId: AgentRunId = _
  private var eventId = EventId.BeforeFirst
  private var snapshotEventId = EventId.BeforeFirst

  "(Login)"  in {
    agentClient.login().await(99.s)
  }

  "Requesting events of unregistered Master" in {
    assert(agentClient.mastersEvents(EventRequest.singleClass[Event](after = 1)).await(99.s) ==
      Left(UnknownMaster(MasterId.fromUserId(TestUserAndPassword.userId))))
  }

  "(RegisterAsMaster)" in {
    val RegisterAsMaster.Response(agentRunId_) = agentClient.commandExecute(RegisterAsMaster(agentRefPath)).await(99.s).orThrow
    this.agentRunId = agentRunId_
  }

  "Request events after known EventId" in {
    val Right(EventSeq.NonEmpty(events)) = agentClient.mastersEvents(
      EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(99.s))).await(99.s)
    assert(events.head.eventId > EventId.BeforeFirst)
  }

  "AgentReadyForMaster" in {
    val Right(EventSeq.NonEmpty(events)) = agentClient.mastersEvents(
      EventRequest[Event](Set(classOf[AgentReadyForMaster]), after = EventId.BeforeFirst, timeout = Some(99.s))).await(99.s)
    eventId = events.last.eventId
  }

  "Requesting events after unknown EventId returns Torn" in {
    // When Master requests events, the requested EventId (after=) must be known
    val Right(TearableEventSeq.Torn(0)) = agentClient.mastersEvents(EventRequest.singleClass[Event](after = 1L)).await(99.s)
  }

  "Login again"  in {
    agentClient.logout().await(99.s)
    agentClient.login().await(99.s)
  }

  "Recoupling with changed AgentRunId or different AgentRefPath fails" in {
    assert(agentClient.commandExecute(CoupleMaster(agentRefPath, AgentRunId(JournalId.random()), eventId)).await(99.s) ==
      Left(MasterAgentMismatch(agentRefPath)))
    assert(agentClient.commandExecute(CoupleMaster(AgentRefPath("/OTHER"), agentRunId, eventId)).await(99.s) ==
      Left(DuplicateAgentRef(first = agentRefPath, second = AgentRefPath("/OTHER"))))
    val Right(EventSeq.NonEmpty(events)) = agentClient.mastersEvents(
      EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(99.s))).await(99.s)
    assert(events.head.eventId > EventId.BeforeFirst)
  }

  "Recoupling with Agent's last events deleted fails" in {
    // Take snapshot, then delete old journal files
    snapshotEventId = eventId
    agentClient.commandExecute(TakeSnapshot).await(99.s).orThrow
    val Right(EventSeq.NonEmpty(stampedEvents)) = agentClient.mastersEvents(EventRequest.singleClass[Event](after = eventId)).await(99.s)
    assert(stampedEvents.size == 1)
    assert(stampedEvents.head.value.event == JournalEvent.SnapshotTaken)
    eventId = stampedEvents.head.eventId

    // Remove obsolete journal files
    agentClient.commandExecute(ReleaseEvents(eventId)).await(99.s).orThrow
    // Await JournalEventsReleased
    eventId = agentClient.mastersEvents(EventRequest.singleClass[Event](after = eventId)).await(99.s).orThrow
      .asInstanceOf[EventSeq.NonEmpty[Seq, AnyKeyedEvent]].stamped.last.eventId

    assert(agentClient.commandExecute(CoupleMaster(agentRefPath, agentRunId, EventId.BeforeFirst)).await(99.s) ==
      Left(UnknownEventIdProblem(EventId.BeforeFirst)))
  }

  "Recoupling with Master's last events deleted fails" in {
    val newerEventId = eventId + 1  // Assuming that no further Event has been emitted
    assert(agentClient.commandExecute(CoupleMaster(agentRefPath, agentRunId, newerEventId)).await(99.s) ==
      Left(UnknownEventIdProblem(newerEventId)))

    val unknownEventId = EventId(1)  // Assuming this is EventId has not been emitted
    assert(agentClient.commandExecute(CoupleMaster(agentRefPath, agentRunId, unknownEventId)).await(99.s) ==
      Left(UnknownEventIdProblem(unknownEventId)))
  }

  "Recouple" in {
    agentClient.commandExecute(CoupleMaster(agentRefPath, agentRunId, eventId)).await(99.s).orThrow
  }

  "Continue fetching events" in {
    val Right(EventSeq.Empty(lastEventId)) = agentClient.mastersEvents(EventRequest.singleClass[Event](after = eventId)).await(99.s)
    assert(lastEventId == eventId)
  }

  "Torn EventSeq" in {
    val Right(TearableEventSeq.Torn(tornEventId)) =
      agentClient.mastersEvents(EventRequest.singleClass[Event](after = EventId.BeforeFirst)).await(99.s)
    assert(tornEventId == snapshotEventId)
  }

  "Future event (not used by Master)" in {
    // Master does not use this feature provided by GenericEventRoute. We test anyway.
    val futureEventId = eventId + 1
    val Right(EventSeq.Empty(lastEventId)) =
      agentClient.mastersEvents(EventRequest.singleClass[Event](after = futureEventId)).await(99.s)
    assert(lastEventId == eventId)
  }
}
