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

  def receive = {
    case message ⇒
      message match {
        case Start ⇒
          val pathToActor = mutable.Map[JobPath, ActorRef]()
          forEachTypedFile(jobConfigurationDirectory, Set(JobPath)) {
            case (file, jobPath: JobPath) ⇒
              logger.info(s"Adding $jobPath")
              val a = JobRunner.actorOf(jobPath)
              pathToActor += jobPath → a
              context.watch(a)
              a ! JobRunner.Command.ReadConfigurationFile(file)  // For many files, may this congest the Akka threads ???
          }
          starting(pathToActor.toMap, sender())

        case msg: JobRunner.Output.ReadyForOrder.type ⇒
          context.parent.forward(msg)
      }
  }

  private def starting(pathToActor: Map[JobPath, ActorRef], commander: ActorRef): Unit = {
    val expectedActors = pathToActor.values.toSet
    val startedActors = mutable.Set[ActorRef]()
    startedActors.sizeHint(expectedActors.size)

    def onJobRunnerStarted(a: ActorRef): Unit = {
      startedActors += a
      ifAllJobsStartedBecomeStarted()
    }

    def ifAllJobsStartedBecomeStarted(): Boolean =
      startedActors == expectedActors && {
        context.become(started)
        commander ! Started(pathToActor.toVector)
        true
      }

    if (!ifAllJobsStartedBecomeStarted()) {
      context.become {
        case JobRunner.Response.Started ⇒
          context.unwatch(sender())
          onJobRunnerStarted(sender())

        case Terminated(a) ⇒
          logger.error(s"$a died")  // Maybe XML parsing error, ignored
          onJobRunnerStarted(a)

        case msg: JobRunner.Output.ReadyForOrder.type ⇒
          context.parent.forward(msg)
      }
    }
  }

  private def started: Receive = {//Actor.emptyBehavior  // Nothing to do
    case msg: JobRunner.Output.ReadyForOrder.type ⇒
      context.parent.forward(msg)
  }
}

object JobKeeper {
  private val logger = Logger(getClass)

  def apply(jobConfigurationDirectory: Path)(implicit newTask: AgentTaskFactory, ec: ExecutionContext) =
    Props { new JobKeeper(jobConfigurationDirectory) }

  case object Start

  final case class Started(jobs: immutable.Seq[(JobPath, ActorRef)])
}
