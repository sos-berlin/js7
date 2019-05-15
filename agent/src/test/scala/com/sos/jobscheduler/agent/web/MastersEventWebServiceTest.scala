package com.sos.jobscheduler.agent.web

import akka.actor.ActorSystem
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.data.commands.AgentCommand.RegisterAsMaster
import com.sos.jobscheduler.agent.tests.AgentTester
import com.sos.jobscheduler.agent.tests.TestAgentDirectoryProvider._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.guice.GuiceImplicits._
import com.sos.jobscheduler.common.scalautil.Closer.ops._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, TearableEventSeq}
import monix.execution.Scheduler
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MastersEventWebServiceTest extends FreeSpec with AgentTester
{
  protected val akkaAskTimeout = 99.s

  implicit private lazy val scheduler = agent.injector.instance[Scheduler]
  implicit private lazy val actorSystem = agent.injector.instance[ActorSystem]
  private val agentClient = AgentClient(agent.localUri).closeWithCloser

  "(login)"  in {
      agentClient.login(Some(TestUserAndPassword)).await(99.s)
      agentClient.commandExecute(RegisterAsMaster).await(99.s).orThrow
  }

  "Request events after known EventId" in {
    val EventSeq.NonEmpty(events) = agentClient.mastersEvents(EventRequest.singleClass[Event](after = EventId.BeforeFirst)).await(99.s)
    assert(events.head.eventId > EventId.BeforeFirst)
  }

  "Requesting events after unknown EventId returns Torn" in {
    val TearableEventSeq.Torn(0) = agentClient.mastersEvents(EventRequest.singleClass[Event](after = 1)).await(99.s)
  }
}
