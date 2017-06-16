package com.sos.jobscheduler.agent.scheduler.job

import akka.actor.{Actor, ActorRef, Props, Terminated}
import com.sos.jobscheduler.agent.scheduler.job.JobKeeper._
import com.sos.jobscheduler.agent.task.AgentTaskFactory
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.jobnet.JobPath
import com.sos.jobscheduler.shared.filebased.TypedPathDirectoryWalker.forEachTypedFile
import java.nio.file.Path
import scala.collection.{immutable, mutable}
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
final class JobKeeper private(jobConfigurationDirectory: Path)(implicit newTask: AgentTaskFactory, ec: ExecutionContext)
extends Actor {

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
          starting(pathToActor.toMap, sender())
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
        context.become(ready)
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

  private def ready: Receive =
    handleReadyForOrder

  private def handleReadyForOrder: Receive = {
    case msg: JobRunner.Output.ReadyForOrder.type ⇒
      context.parent.forward(msg)
  }
}

object JobKeeper {
  private val logger = Logger(getClass)

  def apply(jobConfigurationDirectory: Path)(implicit newTask: AgentTaskFactory, ec: ExecutionContext) =
    Props { new JobKeeper(jobConfigurationDirectory) }

  object Input {
    case object Start
  }

  object Output {
    final case class Ready(jobs: immutable.Seq[(JobPath, ActorRef)])
  }
}
