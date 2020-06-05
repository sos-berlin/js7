package js7.agent.task

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import js7.agent.data.AgentTaskId
import js7.agent.data.views.{TaskOverview, TaskRegisterOverview}
import js7.agent.task.TaskRegisterActor.Command
import js7.base.generic.Completed
import js7.base.process.ProcessSignal
import js7.base.process.ProcessSignal.SIGTERM
import js7.common.scalautil.Futures.promiseFuture
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class TaskRegister(actor: ActorRef)(implicit askTimeout: Timeout) {

  /**
    * TaskRegister removes task automatically when task terminates.
    */
  def add(task: BaseAgentTask): Future[Completed] =
    promiseFuture[Completed] { promise =>
      actor ! TaskRegisterActor.Input.Add(task, promise)
    }

  /**
    * TaskRegister removes task automatically when task terminates.
    */
  @TestOnly
  def remove(taskId: AgentTaskId): Unit =
    actor ! TaskRegisterActor.Input.Remove(taskId)

  def sendSignalToAllProcesses(signal: ProcessSignal): Future[Completed] =
    (actor ? TaskRegisterActor.Command.SendSignalToAllProcesses(SIGTERM)).mapTo[Completed]

  def overview: Future[TaskRegisterOverview] =
    (actor ? Command.GetOverview).mapTo[TaskRegisterOverview]

  def taskOverviews: Future[Seq[TaskOverview]] =
    (actor ? Command.GetTaskOverviews).mapTo[Seq[TaskOverview]]

  def taskOverview(taskId: AgentTaskId): Future[TaskOverview] =
    (actor ? Command.GetTaskOverview(taskId)).mapTo[TaskOverview]
}
