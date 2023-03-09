package js7.agent.scheduler

import akka.actor.ActorRef
import js7.agent.data.commands.AgentCommand
import js7.base.auth.UserId
import js7.base.log.CorrelId
import js7.base.problem.Checked
import scala.concurrent.Promise

/**
  * @author Joacim Zschimmer
  */
final class AgentHandle(actor: ActorRef) {

  def executeCommand(
    command: AgentCommand,
    userId: UserId,
    response: Promise[Checked[AgentCommand.Response]])
    (implicit sender: ActorRef = ActorRef.noSender)
  : Unit =
    actor ! AgentActor.Input.ExternalCommand(userId, command, CorrelId.current, response)
}
