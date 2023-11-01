package js7.agent.scheduler

import js7.agent.data.commands.AgentCommand
import js7.agent.data.views.AgentOverview
import js7.agent.scheduler.AgentActor.Command
import js7.base.auth.UserId
import js7.base.log.CorrelId
import js7.base.problem.Checked
import monix.eval.Task
import org.apache.pekko.actor.ActorRef
import org.apache.pekko.pattern.ask
import org.apache.pekko.util.Timeout
import scala.concurrent.Promise

/**
  * @author Joacim Zschimmer
  */
final class AgentHandle(actor: ActorRef)(implicit askTimeout: Timeout) {

  def executeCommand(
    command: AgentCommand,
    userId: UserId,
    response: Promise[Checked[AgentCommand.Response]])
    (implicit sender: ActorRef = ActorRef.noSender)
  : Unit =
    actor ! AgentActor.Input.ExternalCommand(userId, command, CorrelId.current, response)

  def overview: Task[AgentOverview] =
    Task.deferFuture(
      (actor ? Command.GetOverview).mapTo[AgentOverview])
}
