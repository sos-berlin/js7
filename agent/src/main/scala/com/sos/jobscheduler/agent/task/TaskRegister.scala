package com.sos.jobscheduler.agent.task

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.views.{TaskOverview, TaskRegisterOverview}
import com.sos.jobscheduler.agent.task.TaskRegisterActor.Command
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.process.ProcessSignal.SIGTERM
import com.sos.jobscheduler.common.scalautil.Futures.promiseFuture
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.Seq
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class TaskRegister(actor: ActorRef)(implicit askTimeout: Timeout) {

  /**
    * TaskRegister removes task automatically when task terminates.
    */
  def add(task: BaseAgentTask): Future[Completed] =
    promiseFuture[Completed] { promise ⇒
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
