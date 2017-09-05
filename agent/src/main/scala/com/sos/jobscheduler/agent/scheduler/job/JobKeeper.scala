package com.sos.jobscheduler.agent.scheduler.job

import akka.actor.{Actor, ActorRef, Stash, Terminated}
import com.sos.jobscheduler.agent.data.commandresponses.EmptyResponse
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.scheduler.job.JobKeeper._
import com.sos.jobscheduler.agent.scheduler.job.task.TaskRunner
import com.sos.jobscheduler.agent.task.TaskRegister
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.jobnet.JobPath
import com.sos.jobscheduler.shared.filebased.TypedPathDirectoryWalker.forEachTypedFile
import java.nio.file.Path
import scala.collection.{immutable, mutable}
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
final class JobKeeper(jobConfigurationDirectory: Path, newTaskRunner: TaskRunner.Factory, taskRegister: TaskRegister, timerService: TimerService)
  (implicit ec: ExecutionContext)
extends Actor with Stash {

  override val supervisorStrategy = SupervisorStrategies.escalate
  private val startedJobActors = mutable.Set[ActorRef]()
  private var terminating = false

  def receive = handleReadyForOrder orElse {
    case message ⇒
      message match {
        case Input.Start ⇒
          val pathToActor = mutable.Map[JobPath, ActorRef]()
          forEachTypedFile(jobConfigurationDirectory, Set(JobPath)) {
            case (file, jobPath: JobPath) ⇒
              val a = context.watch(JobActor.actorOf(jobPath, registeringNewTaskRunner, timerService))
              pathToActor += jobPath → a
              a ! JobActor.Command.StartWithConfigurationFile(file)
          }
          unstashAll()
          starting(pathToActor.toMap, sender())

        case _: AgentCommand.TerminateOrAbort ⇒
          stash()
      }
    }

  private val registeringNewTaskRunner = new TaskRunner.Factory {
    def apply(jobConfiguration: JobConfiguration) = {
      val taskRunner = newTaskRunner(jobConfiguration)
      taskRegister.add(taskRunner.asBaseAgentTask)  // TaskRegisterActor removes task automatically
      taskRunner
    }
  }

  private def starting(pathToActor: Map[JobPath, ActorRef], commander: ActorRef): Unit = {
    val expectedActors = mutable.Set[ActorRef]() ++ pathToActor.values
    startedJobActors.sizeHint(expectedActors.size)

    def ifAllJobsStartedThenBecomeStarted(): Boolean =
      startedJobActors == expectedActors && {
        logger.info(s"Ready, ${pathToActor.size} jobs")
        context.become(ready)
        commander ! Output.Ready(pathToActor.toVector)
        true
      }

    if (!ifAllJobsStartedThenBecomeStarted()) {
      context.become(handleReadyForOrder orElse {
        case JobActor.Response.Ready ⇒
          context.unwatch(sender())
          startedJobActors += sender()
          ifAllJobsStartedThenBecomeStarted()

        case Terminated(a) ⇒
          logger.error(s"$a stopped")  // Maybe XML parsing error, ignored ???
          expectedActors -= a
          startedJobActors -= a
          ifAllJobsStartedThenBecomeStarted()
      })
    }
  }

  private def ready: Receive =
    handleReadyForOrder orElse {
      case cmd: AgentCommand.Terminate ⇒
        terminating = true
        for (a ← startedJobActors) a ! cmd
        sender() ! EmptyResponse

      case Terminated(a) if startedJobActors contains a ⇒
        startedJobActors -= a
        if (terminating && startedJobActors.isEmpty) {
          context.stop(self)
        }
    }

  private def handleReadyForOrder: Receive = {
    case msg: JobActor.Output.ReadyForOrder.type ⇒
      context.parent.forward(msg)
  }
}

object JobKeeper {
  private val logger = Logger(getClass)

  object Input {
    case object Start
  }

  object Output {
    final case class Ready(jobs: immutable.Seq[(JobPath, ActorRef)])
  }
}
