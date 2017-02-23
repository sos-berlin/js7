package com.sos.jobscheduler.agent.orderprocessing

import akka.actor.{Actor, ActorRef, OneForOneStrategy, Props, Status, SupervisorStrategy}
import akka.util.Timeout
import com.sos.jobscheduler.agent.data.commandresponses.EmptyResponse
import com.sos.jobscheduler.agent.data.commands.{Command, OrderCommand, RegisterAsMaster}
import com.sos.jobscheduler.agent.orderprocessing.AgentActor._
import com.sos.jobscheduler.agent.orderprocessing.job.{JobKeeper, JobRunner}
import com.sos.jobscheduler.agent.task.AgentTaskFactory
import com.sos.jobscheduler.common.akkautils.Akkas
import com.sos.jobscheduler.common.auth.UserId
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.engine2.order.JobPath
import com.sos.jobscheduler.shared.event.SnapshotKeyedEventBus
import java.nio.file.Path
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
final class AgentActor(
  jobConfigurationDirectory: Path,
  stateDirectory: Path,
  implicit private val askTimeout: Timeout,
  syncOnCommit: Boolean)
  (implicit
    timerService: TimerService,
    keyedEventBus: SnapshotKeyedEventBus,
    eventIdGenerator: EventIdGenerator,
    newTask: AgentTaskFactory,
    executionContext: ExecutionContext)
extends Actor {

  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 0) {
    case _ ⇒ SupervisorStrategy.Stop
  }

  private val jobKeeper = context.actorOf(JobKeeper(jobConfigurationDirectory), "JobKeeper")
  private val masterToOrderKeeper = mutable.Map[UserId, ActorRef]() withDefault { userId ⇒
    throw new NoSuchElementException(s"No master registered for user '$userId'")
  }

  def receive = {
    case Input.Start ⇒
      context.become(startingJobKeeper(sender()))
      jobKeeper ! JobKeeper.Start
  }

  private def startingJobKeeper(commander: ActorRef): Receive = {
    case JobKeeper.Started(jobs) ⇒
      context.become(started(jobs))
      commander ! Output.Started
  }

  private def started(jobs: Seq[(JobPath, ActorRef)]): Receive = {
    case Input.CommandFromMaster(userId: UserId, command: Command) ⇒
      executeCommand(command, userId, jobs)

    case Input.RequestEvents(userId, input) ⇒
      masterToOrderKeeper.get(userId) match {
        case Some(actor) ⇒ actor ! (input: AgentOrderKeeper.Input)
        case None ⇒ sender() ! Status.Failure(new NoSuchElementException(s"No Master registered for User '$userId'"))
      }

    case msg: JobRunner.Output.ReadyForOrder.type ⇒
      for (actor ← masterToOrderKeeper.values) actor.forward(msg)   // Every MasterJunction ???
  }

  private def executeCommand(command: Command, userId: UserId, jobs: Seq[(JobPath, ActorRef)]): Unit =
    command match {
      case RegisterAsMaster ⇒
        if (!masterToOrderKeeper.contains(userId)) {
          //??? require(sessionToken.isDefined)
          val actor = newOrderKeeper(userId)
          masterToOrderKeeper += userId → actor
          actor ! AgentOrderKeeper.Input.Start(jobs)
        }
        sender() ! EmptyResponse

      case cmd: OrderCommand ⇒
        masterToOrderKeeper.get(userId) match {
          case Some(actor) ⇒
            actor.forward(cmd)
          case None ⇒
            sender() ! Status.Failure(new NoSuchElementException(s"Unknown Master for User '$userId'"))
        }
  }

  private def newOrderKeeper(userId: UserId) =
    context.actorOf(
      Props {
        new AgentOrderKeeper(
          journalFile = stateDirectory / s"master-$userId.journal",
          askTimeout = askTimeout,
          syncOnCommit = syncOnCommit,
          keyedEventBus,
          eventIdGenerator,
          timerService)
        },
      Akkas.encodeAsActorName(s"AgentOrderKeeper-for-$userId"))
}

object AgentActor {
  sealed trait Input
  object Input {
    final case object Start extends Output
    final case class CommandFromMaster(usedId: UserId, command: Command)
    final case class RequestEvents(usedId: UserId, input: AgentOrderKeeper.Input.RequestEvents)
  }

  sealed trait Output
  object Output {
    case object Started extends Output
  }
}
