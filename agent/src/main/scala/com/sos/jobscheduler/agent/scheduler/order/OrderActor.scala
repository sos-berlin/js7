package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{ActorRef, DeadLetterSuppression, Props, Status, Terminated}
import com.sos.jobscheduler.agent.scheduler.job.JobActor
import com.sos.jobscheduler.agent.scheduler.job.task.{TaskStepFailed, TaskStepSucceeded}
import com.sos.jobscheduler.agent.scheduler.order.OrderActor._
import com.sos.jobscheduler.agent.scheduler.order.StdouterrToEvent.Stdouterr
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.cast
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.KeyedJournalingActor
import com.sos.jobscheduler.data.job.JobKey
import com.sos.jobscheduler.data.order.OrderEvent._
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.system.{Stderr, Stdout, StdoutOrStderr}
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.taskserver.task.process.StdChannels
import com.typesafe.config.Config
import monix.execution.Scheduler
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class OrderActor private(orderId: OrderId, protected val journalActor: ActorRef, config: Config)
  (implicit scheduler: Scheduler)
extends KeyedJournalingActor[OrderEvent] {

  private val logger = Logger.withPrefix[OrderActor](orderId.toString)

  private val stdouterr = new StdouterrToEvent(context, config, writeStdouterr)
  private var order: Order[Order.State] = null
  private var terminating = false

  protected def key = orderId
  protected def snapshot = Option(order)

  override def postStop() = {
    stdouterr.close()
    super.postStop()
  }

  protected def recoverFromSnapshot(snapshot: Any) = {
    assert(order == null)
    order = cast[Order[Order.State]](snapshot)
  }

  protected def recoverFromEvent(event: OrderEvent) = throw new NotImplementedError

  override protected def finishRecovery() = {
    assert(order != null, "No Order")
    order.state match {
      case _: Order.Idle ⇒
        become("idle")(idle)

      case Order.InProcess ⇒
        become("processed")(processed)
        val event = OrderProcessed(MapDiff.empty, Outcome.RecoveryGeneratedOutcome)
        persist(event)(update)

      case _: Order.Processed ⇒
        become("processed")(processed)
        // Next event 'OrderMoved' is initiated by AgentOrderKeeper

      case _: Order.Forked ⇒
        become("forked")(forked)

      case _: Order.Stopped ⇒
        become("stopped")(stopped)

      case _: Order.Awaiting | _: Order.Stopped | _: Order.Offered | Order.Finished ⇒
        sys.error(s"Order is expected to be on Master, not on Agent: ${order.state}")   // A Finished order must be at Master
    }
    logger.debug(s"Recovered $order")
    sender() ! Output.RecoveryFinished(order)  // Sent via JournalRecoverer to AgentOrderKeeper
  }

  def receive = {
    case Input.Recover(o) ⇒
      assert(order == null)
      order = o

    case Input.AddChild(o) ⇒
      assert(order == null)
      order = o
      become("idle")(idle)

    case Input.AddPublished(o) ⇒
      assert(order == null)
      order = o
      become("offered")(offered)

    case command: Command ⇒
      command match {
        case Command.Attach(Order(`orderId`, workflowPosition, state: Order.Idle, Some(Order.AttachedTo.Agent(agentPath)), parent, payload)) ⇒
          become("idle")(idle)
          persist(OrderAttached(workflowPosition, state, parent, agentPath, payload)) { event ⇒
            sender() ! Completed
            update(event)
          }

        case _ ⇒
          executeOtherCommand(command)
      }
  }

  private val idle: Receive = {
    case Command.Detach ⇒
      detach()

    case command: Command ⇒
      executeOtherCommand(command)

    case Input.StartProcessing(jobKey, workflowJob, jobActor) ⇒
      val stdoutWriter = new StatisticalWriter(stdouterr.writers(Stdout))
      val stderrWriter = new StatisticalWriter(stdouterr.writers(Stderr))
      become("processing")(processing(jobKey, workflowJob, jobActor,
        () ⇒ (stdoutWriter.nonEmpty || stderrWriter.nonEmpty) option s"stdout: $stdoutWriter, stderr: $stderrWriter"))
      context.watch(jobActor)
      persist(OrderProcessingStarted) { event ⇒
        update(event)
        jobActor ! JobActor.Command.ProcessOrder(
          jobKey,
          order.castAfterEvent(event),
          new StdChannels(
            charBufferSize = stdouterr.charBufferSize,
            stdoutWriter = stdoutWriter,
            stderrWriter = stderrWriter))
      }

    case Input.HandleEvent(event: OrderForked) ⇒
      become("forked")(forked)
      persist(event)(update)

    case Input.HandleEvent(event: OrderOffered) ⇒
      persist(event)(update)

    case Input.HandleEvent(OrderDetachable) ⇒
      persist(OrderDetachable)(update)

    case Input.HandleEvent(event: OrderStopped) ⇒
      become("stopped")(stopped)
      persist(event)(update)

    case Input.Terminate ⇒
      context.stop(self)
  }

  private def stopped: Receive = {
    case Command.Detach ⇒
      detach()

    case command: Command ⇒
      executeOtherCommand(command)

    case Input.Terminate ⇒
      context.stop(self)
  }

  private def processing(jobKey: JobKey, job: WorkflowJob, jobActor: ActorRef, stdoutStderrStatistics: () ⇒ Option[String]): Receive = {
    case msg: Stdouterr ⇒  // Handle these events to continue the stdout and stderr threads or the threads will never terminate !!!
      stdouterr.handle(msg)

    case JobActor.Response.OrderProcessed(`orderId`, taskStepEnded) ⇒
      val event = taskStepEnded match {
        case TaskStepSucceeded(variablesDiff, returnCode) ⇒
          job.toOrderProcessed(variablesDiff, returnCode)

        case TaskStepFailed(disrupted) ⇒
          OrderProcessed(MapDiff.empty, disrupted)
      }
      finishProcessing(event, stdoutStderrStatistics)
      context.unwatch(jobActor)

    case Terminated(`jobActor`) ⇒
      val bad = Outcome.Disrupted(Outcome.Disrupted.Other(s"Job Actor for '$jobKey' terminated unexpectedly"))
      finishProcessing(OrderProcessed(MapDiff.empty, bad), stdoutStderrStatistics)

    case command: Command ⇒
      executeOtherCommand(command)

    case Input.Terminate ⇒
      terminating = true
  }

  private def finishProcessing(event: OrderProcessed, stdoutStderrStatistics: () ⇒ Option[String]): Unit = {
    stdouterr.close()
    for (o ← stdoutStderrStatistics()) logger.debug(o)
    become("processed")(processed)
    persist(event) { event ⇒
      update(event)
      if (terminating) {
        context.stop(self)
      }
    }
  }

  private def processed: Receive = {
    case Input.HandleEvent(event: OrderMoved) ⇒
      become("idle")(idle)
      persist(event)(update)

    case Input.HandleEvent(event: OrderStopped) ⇒
      become("stopped")(stopped)
      persist(event)(update)

    case Input.HandleEvent(OrderDetachable) ⇒
      persist(OrderDetachable)(update)

    case Input.Terminate ⇒
      context.stop(self)

    case Command.Detach ⇒
      detach()

    case command: Command ⇒
      executeOtherCommand(command)
  }

  private def forked: Receive = {
    case Input.HandleEvent(event: OrderJoined) ⇒
      become("idle")(idle)
      persist(event)(update)

    case Input.HandleEvent(OrderDetachable) ⇒
      persist(OrderDetachable)(update)

    case Command.Detach ⇒
      detach()

    case Input.Terminate ⇒
      context.stop(self)

    case command: Command ⇒
      executeOtherCommand(command)
  }

  private def offered: Receive = {
    case command: Command ⇒
      executeOtherCommand(command)
  }

  private def executeOtherCommand(command: Command): Unit = {
    val msg = s"Improper command $command while in state ${Option(order) map (_.state) getOrElse "(no order)"}"
    logger.error(msg)
    sender() ! Status.Failure(new IllegalStateException(msg))
  }

  private def detach(): Unit =
    persist(OrderDetached) { event ⇒
      sender() ! Completed
      update(event)
    }

  private def writeStdouterr(t: StdoutOrStderr, chunk: String): Future[Completed] =
    persist(OrderStdWritten(t)(chunk)) { _ ⇒
      Completed
    }

  private def update(event: OrderEvent) = {
    updateOrder(event)
    context.parent ! Output.OrderChanged(order, event)
    if (event == OrderDetached) {
      logger.trace("Stopping after OrderDetached")
      order = null
      context.stop(self)
    }
  }

  private def updateOrder(event: OrderEvent) = {
    order = event match {
      case event: OrderAttached ⇒
        Order.fromOrderAttached(orderId, event)
        // Order.state = Attached / MovedToAgent ???

      case _: OrderStdWritten ⇒
        // Not collected
        order

      case event: OrderCoreEvent if order != null ⇒
        order.update(event)

      case _ ⇒
        sys.error(s"Unexpected event for '$orderId': $event")
    }
  }

  override def unhandled(msg: Any) =
    msg match {
      case msg @ (_: Command | _: Input) ⇒
        logger.error(s"Unhandled message $msg in state ${order.state}")

      case _ ⇒
        super.unhandled(msg)
    }

  override def toString = s"OrderActor(${orderId.string})"
}

private[order] object OrderActor
{
  private[order] def props(orderId: OrderId, journalActor: ActorRef, config: Config)(implicit s: Scheduler) =
    Props { new OrderActor(orderId, journalActor = journalActor, config) }

  sealed trait Command
  object Command {
    final case class  Attach(order: Order[Order.Idle]) extends Command
    final case object Detach extends Command
  }

  sealed trait Input
  object Input {
    final case class Recover(order: Order[Order.State]) extends Input
    final case class AddChild(order: Order[Order.Ready]) extends Input
    final case class AddPublished(order: Order[Order.Offered]) extends Input
    final case class StartProcessing(jobKey: JobKey, workflowJob: WorkflowJob, jobActor: ActorRef) extends Input
    final case object Terminate extends Input with DeadLetterSuppression
    final case class HandleEvent(event: OrderActorEvent) extends Input
  }

  object Output {
    final case class RecoveryFinished(order: Order[Order.State])
    final case class OrderChanged(order: Order[Order.State], event: OrderEvent)
  }
}
