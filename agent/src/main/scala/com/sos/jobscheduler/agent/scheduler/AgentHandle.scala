package com.sos.jobscheduler.agent.scheduler

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.scheduler.AgentActor.Command
import com.sos.jobscheduler.agent.scheduler.order.AgentOrderKeeper
import com.sos.jobscheduler.agent.views.AgentOverview
import com.sos.jobscheduler.common.auth.UserId
import com.sos.jobscheduler.data.event.{EventRequest, EventSeq, KeyedEvent}
import com.sos.jobscheduler.data.order.OrderEvent
import scala.collection.immutable.Seq
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
final class AgentHandle(actor: ActorRef)(implicit askTimeout: Timeout) {

  def executeCommand(command: AgentCommand, userId: UserId, response: Promise[AgentCommand.Response])
    (implicit sender: ActorRef = ActorRef.noSender)
  : Unit =
    actor ! AgentActor.Input.ExternalCommand(userId, command, response)

  def fetchEvents(userId: UserId, request: EventRequest[OrderEvent]): Future[EventSeq[Seq, KeyedEvent[OrderEvent]]] = {
    val promise = Promise[EventSeq[Seq, KeyedEvent[OrderEvent]]]
    actor ! AgentActor.Input.RequestEvents(
      userId,
      AgentOrderKeeper.Input.RequestEvents(
        after = request.after,
        timeout = request.timeout,
        limit = request.limit,
        promise))
    promise.future
  }

  def overview: Future[AgentOverview] =
    (actor ? Command.GetOverview).mapTo[AgentOverview]
}
