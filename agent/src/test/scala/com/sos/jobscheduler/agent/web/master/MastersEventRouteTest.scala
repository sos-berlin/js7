package com.sos.jobscheduler.agent.web.master

import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{CoupleMaster, ReleaseEvents, RegisterAsMaster, TakeSnapshot}
import com.sos.jobscheduler.agent.data.event.AgentMasterEvent.AgentReadyForMaster
import com.sos.jobscheduler.agent.data.problems.MasterAgentMismatchProblem
import com.sos.jobscheduler.agent.tests.AgentTester
import com.sos.jobscheduler.agent.tests.TestAgentDirectoryProvider._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Closer.syntax._
import com.sos.jobscheduler.common.guice.GuiceImplicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.core.problems.NoSuchMasterProblem
import com.sos.jobscheduler.data.agent.AgentRunId
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, JournalEvent, JournalId, TearableEventSeq}
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.problems.UnknownEventIdProblem
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
  private val agentClient = AgentClient(agent.localUri).closeWithCloser
  private var agentRunId: AgentRunId = _
  private var eventId = EventId.BeforeFirst
  private var snapshotEventId = EventId.BeforeFirst

  "(Login)"  in {
    agentClient.login(Some(TestUserAndPassword)).await(99.s)
  }

  "Requesting events of unregistered Master" in {
    assert(agentClient.mastersEvents(EventRequest.singleClass[Event](after = 1)).await(99.s) ==
      Left(NoSuchMasterProblem(MasterId.fromUserId(TestUserAndPassword.userId))))
  }

  "(RegisterAsMaster)" in {
    val RegisterAsMaster.Response(agentRunId_) = agentClient.commandExecute(RegisterAsMaster).await(99.s).orThrow
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
    val Right(TearableEventSeq.Torn(0)) = agentClient.mastersEvents(EventRequest.singleClass[Event](after = 1)).await(99.s)
  }

  "Login again"  in {
    agentClient.logout().await(99.s)
    agentClient.login(Some(TestUserAndPassword)).await(99.s)
  }

  "Recoupling with changed AgentRunId fails" in {
    assert(agentClient.commandExecute(CoupleMaster(AgentRunId(JournalId.random()), eventId)).await(99.s) == Left(MasterAgentMismatchProblem))
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

    assert(agentClient.commandExecute(CoupleMaster(agentRunId, EventId.BeforeFirst)).await(99.s) ==
      Left(UnknownEventIdProblem(EventId.BeforeFirst)))
  }

  "Recoupling with Master's last events deleted fails" in {
    val newerEventId = eventId + 1  // Assuming that no further Event has been issued
    assert(agentClient.commandExecute(CoupleMaster(agentRunId, newerEventId)).await(99.s) ==
      Left(UnknownEventIdProblem(newerEventId)))

    val unknownEventId = EventId(1)  // Assuming this is EventId has not been issued
    assert(agentClient.commandExecute(CoupleMaster(agentRunId, unknownEventId)).await(99.s) ==
      Left(UnknownEventIdProblem(unknownEventId)))
  }

  "Recouple" in {
    agentClient.commandExecute(CoupleMaster(agentRunId, eventId)).await(99.s).orThrow
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
