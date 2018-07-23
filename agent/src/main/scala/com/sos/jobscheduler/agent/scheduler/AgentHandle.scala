package com.sos.jobscheduler.agent.scheduler

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.views.AgentOverview
import com.sos.jobscheduler.agent.scheduler.AgentActor.Command
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.common.event.EventWatch
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.data.master.MasterId
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

  def eventWatch(masterId: MasterId): Task[EventWatch[Event]] =
    Task.deferFuture(
      (actor ? AgentActor.Input.GetEventWatch(masterId)).mapTo[EventWatch[Event]])

  def overview: Future[AgentOverview] =
    (actor ? Command.GetOverview).mapTo[AgentOverview]
}
