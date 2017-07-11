package com.sos.jobscheduler.agent.scheduler.job

import akka.actor.{Actor, ActorRef, Stash, Terminated}
import com.sos.jobscheduler.agent.data.commandresponses.EmptyResponse
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.scheduler.job.JobKeeper._
import com.sos.jobscheduler.common.akkautils.Akkas.StoppingStrategies
import com.sos.jobscheduler.agent.scheduler.job.task.TaskRunner
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
final class JobKeeper(jobConfigurationDirectory: Path)(implicit newTaskRunner: TaskRunner.Factory, ts: TimerService, ec: ExecutionContext)
extends Actor with Stash {

  override val supervisorStrategy = StoppingStrategies.stopping(logger)
  private val startedJobActors = mutable.Set[ActorRef]()
  private var terminating = false

  def receive = handleReadyForOrder orElse {
    case message ⇒
      message match {
        case Input.Start ⇒
          val pathToActor = mutable.Map[JobPath, ActorRef]()
          forEachTypedFile(jobConfigurationDirectory, Set(JobPath)) {
            case (file, jobPath: JobPath) ⇒
              val a = context.watch(JobRunner.actorOf(jobPath))
              pathToActor += jobPath → a
              a ! JobRunner.Command.StartWithConfigurationFile(file)
          }
          unstashAll()
          starting(pathToActor.toMap, sender())

        case _: AgentCommand.TerminateOrAbort ⇒
          stash()
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
        case JobRunner.Response.Ready ⇒
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
    case msg: JobRunner.Output.ReadyForOrder.type ⇒
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
