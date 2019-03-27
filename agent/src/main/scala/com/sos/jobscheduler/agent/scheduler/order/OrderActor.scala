package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{ActorRef, DeadLetterSuppression, Props, Status, Terminated}
import akka.pattern.pipe
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.agent.scheduler.job.JobActor
import com.sos.jobscheduler.agent.scheduler.job.task.{TaskStepFailed, TaskStepSucceeded}
import com.sos.jobscheduler.agent.scheduler.order.OrderActor._
import com.sos.jobscheduler.agent.scheduler.order.StdouterrToEvent.Stdouterr
import com.sos.jobscheduler.base.generic.{Accepted, Completed}
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
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
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final class OrderActor private(orderId: OrderId, protected val journalActor: ActorRef, conf: Conf)
  (implicit scheduler: Scheduler)
extends KeyedJournalingActor[OrderEvent]
{
  private val logger = Logger.withPrefix[OrderActor](orderId.toString)
  import conf.{charBufferSize, stdoutCommitDelay}

  private var order: Order[Order.State] = null
  private var stdouterr: StdouterrToEvent = null
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
      case Order.Processing => handleEvent(OrderProcessed(Outcome.RecoveryGeneratedOutcome))
      case _ => becomeAsStateOf(order, force = true)
    }
    logger.debug(s"Recovered $order")
    sender() ! Output.RecoveryFinished(order)  // Sent via JournalRecoverer to AgentOrderKeeper
  }

  def receive = {
    case Input.Recover(o) =>
      assert(order == null)
      order = o

    case Input.AddChild(o) =>
      assert(order == null)
      order = o
      becomeAsStateOf(order, force = true)

    case Input.AddOffering(o) =>
      assert(order == null)
      order = o
      becomeAsStateOf(order, force = true)

    case command: Command =>
      command match {
        case Command.Attach(attached @ Order(`orderId`, workflowPosition, state: Order.FreshOrReady, arguments, historicOutcomes, Some(Order.Attached(agentRefPath)), parent, cancelationMarked/*???*/)) =>
          becomeAsStateOf(attached, force = true)
          persist(OrderAttached(arguments, workflowPosition, state, historicOutcomes, parent, agentRefPath)) { event =>
            update(event)
            sender() ! Completed
          }

        case _ =>
          executeOtherCommand(command)
      }
  }

  private def fresh = startable

  private def ready: Receive =
    startable orElse {
      case command: Command =>
        executeOtherCommand(command)
    }

  private def delayedAfterError: Receive =
    startable orElse {
      case command: Command => executeOtherCommand(command)
    }

  private def startable: Receive =
    receiveEvent orElse {
      case Input.StartProcessing(jobKey, workflowJob, jobActor, defaultArguments) =>
        assert(stdouterr == null)
        stdouterr = new StdouterrToEvent(context, conf.stdouterrToEventConf, writeStdouterr)
        val stdoutWriter = new StatisticalWriter(stdouterr.writers(Stdout))
        val stderrWriter = new StatisticalWriter(stdouterr.writers(Stderr))
        become("processing")(processing(jobKey, workflowJob, jobActor,
          () => (stdoutWriter.nonEmpty || stderrWriter.nonEmpty) option s"stdout: $stdoutWriter, stderr: $stderrWriter"))
        context.watch(jobActor)
        val orderStarted = order.isState[Order.Fresh] thenList OrderStarted  // OrderStarted automatically with first OrderProcessingStarted
        persistTransaction(orderStarted :+ OrderProcessingStarted) { events =>
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

      case Input.Terminate(_, _) =>
        context.stop(self)
    }

  private def stoppedOrBroken: Receive =
    receiveEvent orElse {
      case command: Command =>
        executeOtherCommand(command)

      case Input.Terminate(_, _) =>
        context.stop(self)
    }

  private def processing(jobKey: JobKey, job: WorkflowJob, jobActor: ActorRef, stdoutStderrStatistics: () => Option[String]): Receive =
    receiveEvent orElse {
      case msg: Stdouterr =>  // Handle these events to continue the stdout and stderr threads or the threads will never terminate !!!
        stdouterr.handle(msg)

      case JobActor.Response.OrderProcessed(`orderId`, taskStepEnded) =>
        val event = taskStepEnded match {
          case TaskStepSucceeded(keyValues, returnCode) =>
            job.toOrderProcessed(returnCode, keyValues)

          case TaskStepFailed(problem) =>
            OrderProcessed(Outcome.Disrupted(problem))
        }
        finishProcessing(event, stdoutStderrStatistics)
        context.unwatch(jobActor)

      case Terminated(`jobActor`) =>
        // May occur when JobActor is terminated while receiving JobActor.Command.ProcessOrder
        if (!terminating) {
          val problem = Problem.pure(s"Job Actor for '$jobKey' terminated unexpectedly")
          logger.error(problem.toString)
          finishProcessing(OrderProcessed(Outcome.Disrupted(problem)), stdoutStderrStatistics)
        } else {
          context.stop(self)
        }

      case command: Command =>
        executeOtherCommand(command)

      case Input.Terminate(sigtermProcesses, sigkillProcessesAfter) =>
        terminating = true
        jobActor ! JobActor.Input.KillProcess(orderId, sigtermProcesses, sigkillProcessesAfter)
    }

  private def finishProcessing(event: OrderProcessed, stdoutStderrStatistics: () => Option[String]): Unit = {
    stdouterr.close()
    stdouterr = null
    for (o <- stdoutStderrStatistics()) logger.debug(o)
    handleEvent(event)
  }

  private def processed: Receive =
    receiveEvent orElse {
      case Input.Terminate(_, _) =>
        context.stop(self)

      case command: Command =>
        executeOtherCommand(command)
    }

  private def forked: Receive =
    receiveEvent orElse {
      case Input.Terminate(_, _) =>
        context.stop(self)

      case command: Command =>
        executeOtherCommand(command)
    }

  private def offering: Receive =
    receiveEvent orElse {
      case Input.Terminate(_, _) =>
        context.stop(self)

      case command: Command =>
        executeOtherCommand(command)
    }

  private def receiveEvent: Receive = {
    case Command.HandleEvent(event) => handleEvent(event) pipeTo sender()
  }

  private def handleEvent(event: OrderCoreEvent): Future[Completed] =
    order.update(event) match {
      case Invalid(problem) =>
        logger.error(problem.toString)
        Future.successful(Completed)

      case Valid(updated) =>
        becomeAsStateOf(updated)
        if (event.isInstanceOf[OrderCancelationMarked] && updated == order)  // Duplicate, already canceling with same CancelMode?
          Future.successful(Completed)
        else
          persist(event) { event =>
            update(event)
            if (terminating) {
              context.stop(self)
            }
            Completed
          }
    }

  private def becomeAsStateOf(anOrder: Order[Order.State], force: Boolean = false): Unit = {
    if (anOrder.isDetaching)
      become("detaching")(detaching)
    else
    if (force || anOrder.state.getClass != order.state.getClass) {
      anOrder.state match {
        case _: Order.Fresh      => become("fresh")(fresh)
        case _: Order.Ready      => become("ready")(ready)
        case _: Order.Processing => sys.error("Unexpected Order.state 'Processing'")  // Not handled here
        case _: Order.Processed  => become("processed")(processed)
        case _: Order.DelayedAfterError => become("delayedAfterError")(delayedAfterError)
        case _: Order.Offering   => become("offering")(offering)
        case _: Order.Forked     => become("forked")(forked)
        case _: Order.Stopped    => become("stopped")(stoppedOrBroken)
        case _: Order.Broken     => become("broken")(stoppedOrBroken)
        case _: Order.Awaiting | _: Order.Stopped | _: Order.Offering | Order.Finished | Order.Canceled =>
          sys.error(s"Order is expected to be on Master, not on Agent: ${order.state}")   // A Finished order must be at Master
      }
    }
  }

  private def detaching: Receive =
    receiveEvent orElse {
      case Input.Terminate(_, _) =>
        context.stop(self)

      case cmd: Command =>
        executeOtherCommand(cmd)
    }

  private def executeOtherCommand(command: Command): Unit = {
    val msg = s"Improper command $command while in state ${Option(order) map (_.state) getOrElse "(no order)"}"
    logger.error(msg)
    sender() ! Status.Failure(new IllegalStateException(msg))
  }

  private def writeStdouterr(t: StdoutOrStderr, chunk: String): Future[Accepted] =
    if (stdoutCommitDelay.isZero)  // slow
      persist(OrderStdWritten(t)(chunk)) { _ =>
        Accepted
      }
    else
      persistAcceptEarly(OrderStdWritten(t)(chunk), delay = stdoutCommitDelay)
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
      case event: OrderAttached =>
        Order.fromOrderAttached(orderId, event)

      case _: OrderStdWritten =>
        // Not collected
        order

      case event: OrderCoreEvent if order != null =>
        order.update(event).orThrow  // 🔥 ProblemException, snapshot will be lost!
        // Vielleicht anschließend: order.forceUpdate(OrderBroken(problem)) ?

      case _ =>
        sys.error(s"Unexpected event for '$orderId': $event")
    }
  }

  override def unhandled(msg: Any) =
    msg match {
      case msg @ (_: Command | _: Input) =>
        logger.error(s"Unhandled message $msg in state '$actorStateName', Order ${order.state}")

      case _ =>
        super.unhandled(msg)
    }

  override def toString = s"OrderActor(${orderId.string})"
}

private[order] object OrderActor
{
  private[order] def props(orderId: OrderId, journalActor: ActorRef, conf: OrderActor.Conf)(implicit s: Scheduler) =
    Props { new OrderActor(orderId, journalActor = journalActor, conf) }

  sealed trait Command
  object Command {
    final case class Attach(order: Order[Order.FreshOrReady]) extends Command
    final case class HandleEvent(event: OrderCoreEvent) extends Input
  }

  sealed trait Input
  object Input {
    final case class Recover(order: Order[Order.State]) extends Input
    final case class AddChild(order: Order[Order.Ready]) extends Input
    final case class AddOffering(order: Order[Order.Offering]) extends Input
    final case class StartProcessing(jobKey: JobKey, workflowJob: WorkflowJob, jobActor: ActorRef, defaultArguments: Map[String, String])
      extends Input
    final case class Terminate(
      sigtermProcesses: Boolean = false,
      sigkillProcessesAfter: Option[FiniteDuration] = None)
    extends Input with DeadLetterSuppression
  }

  object Output {
    final case class RecoveryFinished(order: Order[Order.State])
    final case class OrderChanged(order: Order[Order.State], event: OrderEvent)
  }

  final case class Conf(stdoutCommitDelay: FiniteDuration, charBufferSize: Int, stdouterrToEventConf: StdouterrToEvent.Conf)
  object Conf {
    def apply(config: Config) = new Conf(
      stdoutCommitDelay = config.getDuration("jobscheduler.order.stdout-stderr.commit-delay").toFiniteDuration,
      charBufferSize    = config.getInt     ("jobscheduler.order.stdout-stderr.char-buffer-size"),
      stdouterrToEventConf = StdouterrToEvent.Conf(config))
  }
}
