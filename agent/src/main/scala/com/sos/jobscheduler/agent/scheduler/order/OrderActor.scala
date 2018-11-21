package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{ActorRef, DeadLetterSuppression, Props, Status, Terminated}
import com.sos.jobscheduler.agent.scheduler.job.JobActor
import com.sos.jobscheduler.agent.scheduler.job.task.{TaskStepFailed, TaskStepSucceeded}
import com.sos.jobscheduler.agent.scheduler.order.OrderActor._
import com.sos.jobscheduler.agent.scheduler.order.StdouterrToEvent.Stdouterr
import com.sos.jobscheduler.base.generic.{Accepted, Completed}
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.cast
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
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
  private val stdoutDelay = config.getDuration("jobscheduler.order.stdout-stderr.sync-delay").toFiniteDuration
  private val charBufferSize = config.getInt  ("jobscheduler.order.stdout-stderr.char-buffer-size")

  private var stdouterr: StdouterrToEvent = null
  private var order: Order[Order.State] = null
  private var terminating = false

  protected def key = orderId
  protected def snapshot = Option(order)

  override def postStop() = {
    if (stdouterr != null) stdouterr.close()
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
      case _: Order.Fresh ⇒
        become("freshOrReady")(fresh)

      case Order.Ready ⇒
        become("ready")(ready)

      case Order.Processing ⇒
        become("processed")(processed)
        val event = OrderProcessed(MapDiff.empty, Outcome.RecoveryGeneratedOutcome)
        persist(event)(update)

      case _: Order.Processed ⇒
        become("processed")(processed)
        // Next event 'OrderMoved' is initiated by AgentOrderKeeper

      case _: Order.Forked ⇒
        become("forked")(forked)

      case _: Order.Stopped ⇒
        become("stopped")(stoppedOrDisrupted)

      case _: Order.Broken ⇒
        become("disrupted")(stoppedOrDisrupted)

      case _: Order.Awaiting | _: Order.Stopped | _: Order.Offering | Order.Finished ⇒
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
      become("ready")(ready)

    case Input.AddPublished(o) ⇒
      assert(order == null)
      order = o
      become("offered")(offered)

    case command: Command ⇒
      command match {
        case Command.Attach(Order(`orderId`, workflowPosition, state: Order.FreshOrReady, Some(Order.AttachedTo.Agent(agentPath)), parent, payload)) ⇒
          state match {
            case _: Order.Fresh ⇒ become("fresh")(fresh)
            case Order.Ready ⇒ become("ready")(ready)
          }
          persist(OrderAttached(workflowPosition, state, parent, agentPath, payload)) { event ⇒
            sender() ! Completed
            update(event)
          }

        case _ ⇒
          executeOtherCommand(command)
      }
  }

  private val fresh: Receive =
    freshOrReady orElse {
      case Input.StartProcessing(jobKey, workflowJob, jobActor, defaultArguments) ⇒

    }

  private val ready: Receive =
    freshOrReady orElse {
      case command: Command ⇒
        executeOtherCommand(command)

      case Input.HandleEvent(event: OrderForked) ⇒
        become("forked")(forked)
        persist(event)(update)

      case Input.HandleEvent(event: OrderOffered) ⇒
        persist(event)(update)

      //case Input.HandleEvent(event: OrderStopped) ⇒
      //  become("stopped")(stoppedOrDisrupted)
      //  persist(event)(update)
    }

  private def freshOrReady: Receive = {
    case Command.Detach ⇒
      detach()

    case Input.StartProcessing(jobKey, workflowJob, jobActor, defaultArguments) ⇒
      assert(stdouterr == null)
      stdouterr = new StdouterrToEvent(context, config, writeStdouterr)
      val stdoutWriter = new StatisticalWriter(stdouterr.writers(Stdout))
      val stderrWriter = new StatisticalWriter(stdouterr.writers(Stderr))
      become("processing")(processing(jobKey, workflowJob, jobActor,
        () ⇒ (stdoutWriter.nonEmpty || stderrWriter.nonEmpty) option s"stdout: $stdoutWriter, stderr: $stderrWriter"))
      context.watch(jobActor)
      val orderStarted = order.isState[Order.Fresh] list OrderStarted  // OrderStarted automatically with first OrderProcessingStarted
      persistTransaction(orderStarted :+ OrderProcessingStarted) { events ⇒
        events foreach update
        jobActor ! JobActor.Command.ProcessOrder(
          jobKey,
          order.castState[Order.Processing],
          defaultArguments,
          new StdChannels(
            charBufferSize = charBufferSize,
            stdoutWriter = stdoutWriter,
            stderrWriter = stderrWriter))
      }

    case Input.HandleEvent(OrderDetachable) ⇒
      persist(OrderDetachable)(update)

    case Input.HandleEvent(event: OrderBroken) ⇒
      become("disrupted")(stoppedOrDisrupted)
      persist(event)(update)

    case Input.Terminate ⇒
      context.stop(self)
  }

  private def stoppedOrDisrupted: Receive = {
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

        case TaskStepFailed(problem) ⇒
          OrderProcessed(MapDiff.empty, Outcome.Disrupted(problem))
      }
      finishProcessing(event, stdoutStderrStatistics)
      context.unwatch(jobActor)

    case Terminated(`jobActor`) ⇒
      // May occur when ActorSystem suddenly terminates (fatal Throwable or Java shutdown hook <-- ActorSystem registered itself)
      // JobActor has killed process. Job may be restarted after recovery.
      val problem = Problem.eager(s"Job Actor for '$jobKey' terminated unexpectedly")
      logger.error(problem.toString)
      val bad = Outcome.Disrupted(problem)
      finishProcessing(OrderProcessed(MapDiff.empty, bad), stdoutStderrStatistics)

    case command: Command ⇒
      executeOtherCommand(command)

    case Input.Terminate ⇒
      terminating = true
  }

  private def finishProcessing(event: OrderProcessed, stdoutStderrStatistics: () ⇒ Option[String]): Unit = {
    stdouterr.close()
    stdouterr = null
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
      become("read")(ready)
      persist(event)(update)

    case Input.HandleEvent(event: OrderStopped) ⇒
      become("stopped")(stoppedOrDisrupted)
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
      become("ready")(ready)
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

  private def writeStdouterr(t: StdoutOrStderr, chunk: String): Future[Accepted] =
    if (stdoutDelay.isZero)  // slow
      persist(OrderStdWritten(t)(chunk)) { _ ⇒
        Accepted
      }
    else
      persistAcceptEarly(OrderStdWritten(t)(chunk), delay = stdoutDelay)
      // Don't wait for disk-sync. OrderStdWritten is followed by a OrderProcessed, then waiting for disk-sync.

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

      case _: OrderStdWritten ⇒
        // Not collected
        order

      case event: OrderCoreEvent if order != null ⇒
        order.update(event).orThrow  // 🔥 ProblemException, snapshot will be lost!
        // Vielleicht anschließend: order.forceUpdate(OrderBroken(problem)) ?

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
    final case class  Attach(order: Order[Order.FreshOrReady]) extends Command
    final case object Detach extends Command
  }

  sealed trait Input
  object Input {
    final case class Recover(order: Order[Order.State]) extends Input
    final case class AddChild(order: Order[Order.Ready]) extends Input
    final case class AddPublished(order: Order[Order.Offering]) extends Input
    final case class StartProcessing(jobKey: JobKey, workflowJob: WorkflowJob, jobActor: ActorRef, defaultArguments: Map[String, String])
      extends Input
    final case object Terminate extends Input with DeadLetterSuppression
    final case class HandleEvent(event: OrderActorEvent) extends Input
  }

  object Output {
    final case class RecoveryFinished(order: Order[Order.State])
    final case class OrderChanged(order: Order[Order.State], event: OrderEvent)
  }
}
