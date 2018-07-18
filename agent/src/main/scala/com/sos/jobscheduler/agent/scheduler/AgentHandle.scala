package com.sos.jobscheduler.agent.scheduler

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.views.AgentOverview
import com.sos.jobscheduler.agent.scheduler.AgentActor.Command
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.common.event.EventReader
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.OrderEvent
import monix.eval.Task
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
final class AgentHandle(actor: ActorRef)(implicit askTimeout: Timeout) {

  def executeCommand(command: AgentCommand, userId: UserId, response: Promise[AgentCommand.Response])
    (implicit sender: ActorRef = ActorRef.noSender)
  : Unit =
    actor ! AgentActor.Input.ExternalCommand(userId, command, response)

  def eventReader(masterId: MasterId): Task[EventReader[OrderEvent]] =
    Task.deferFuture(
      (actor ? AgentActor.Input.GetEventReader(masterId)).mapTo[EventReader[OrderEvent]])

  def overview: Future[AgentOverview] =
    (actor ? Command.GetOverview).mapTo[AgentOverview]
}
