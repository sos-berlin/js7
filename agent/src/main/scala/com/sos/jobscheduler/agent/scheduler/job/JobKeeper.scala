package com.sos.jobscheduler.agent.scheduler.job

import akka.Done
import akka.actor.{Actor, ActorRef, Stash, Terminated}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.scheduler.job.JobKeeper._
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

  def receive = handleReadyForOrder orElse {
    case message ⇒
      message match {
        case Input.Start ⇒
          val pathToActor = mutable.Map[JobPath, ActorRef]()
          forEachTypedFile(jobConfigurationDirectory, Set(JobPath)) {
            case (file, jobPath: JobPath) ⇒
              val a = JobRunner.actorOf(jobPath)
              pathToActor += jobPath → a
              context.watch(a)
              a ! JobRunner.Command.ReadConfigurationFile(file)
          }
          unstashAll()
          starting(pathToActor.toMap, sender())

        case _: AgentCommand.TerminateOrAbort ⇒
          stash()
      }
    }

  private def starting(pathToActor: Map[JobPath, ActorRef], commander: ActorRef): Unit = {
    val expectedActors = pathToActor.values.toSet
    val startedActors = mutable.Set[ActorRef]()
    startedActors.sizeHint(expectedActors.size)

    def onJobRunnerStarted(a: ActorRef): Unit = {
      startedActors += a
      ifAllJobsStartedThenBecomeStarted()
    }

    def ifAllJobsStartedThenBecomeStarted(): Boolean =
      startedActors == expectedActors && {
        logger.info(s"Ready, ${pathToActor.size} jobs")
        context.become(ready(pathToActor.values.toVector))
        commander ! Output.Ready(pathToActor.toVector)
        true
      }

    if (!ifAllJobsStartedThenBecomeStarted()) {
      context.become(handleReadyForOrder orElse {
        case JobRunner.Response.Ready ⇒
          context.unwatch(sender())
          onJobRunnerStarted(sender())

        case Terminated(a) ⇒
          logger.error(s"$a died")  // Maybe XML parsing error, ignored
          onJobRunnerStarted(a)
      })
    }
  }

  private def ready(jobActors: Vector[ActorRef]): Receive =
    handleReadyForOrder orElse handleTerminateOrAbort(jobActors)

  private def handleReadyForOrder: Receive = {
    case msg: JobRunner.Output.ReadyForOrder.type ⇒
      context.parent.forward(msg)
  }

  private def handleTerminateOrAbort(jobActors: Vector[ActorRef]): Receive = {
    case cmd: AgentCommand.TerminateOrAbort ⇒
      for (a ← jobActors) a ! cmd
      sender() ! Done
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
