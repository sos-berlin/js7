package js7.agent.scheduler.order

import akka.actor.ActorRef.noSender
import akka.actor.{ActorRef, DeadLetterSuppression, Props, Status, Terminated}
import akka.pattern.pipe
import com.typesafe.config.Config
import js7.agent.data.AgentState
import js7.agent.scheduler.job.JobActor
import js7.agent.scheduler.job.task.{TaskStepFailed, TaskStepSucceeded}
import js7.agent.scheduler.order.OrderActor._
import js7.agent.scheduler.order.StdouterrToEvent.Stdouterr
import js7.base.generic.{Accepted, Completed}
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.process.ProcessSignal
import js7.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import js7.common.scalautil.Logger
import js7.common.time.JavaTimeConverters._
import js7.core.event.journal.{JournalActor, JournalConf, KeyedJournalingActor}
import js7.data.command.CancelMode
import js7.data.job.{JobKey, ReturnCode}
import js7.data.order.OrderEvent._
import js7.data.order.{Order, OrderEvent, OrderId, Outcome}
import js7.data.system.{Stderr, Stdout, StdoutOrStderr}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.taskserver.task.process.StdChannels
import monix.execution.Scheduler
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, FiniteDuration}
import shapeless.tag.@@

/**
  * @author Joacim Zschimmer
  */
final class OrderActor private(orderId: OrderId, protected val journalActor: ActorRef @@ JournalActor.type, conf: Conf)
  (implicit protected val scheduler: Scheduler)
extends KeyedJournalingActor[AgentState, OrderEvent]
{
  private val logger = Logger.withPrefix[this.type](orderId.toString)
  import conf.{charBufferSize, stdoutCommitDelay}

  protected def journalConf = conf.journalConf
  private var order: Order[Order.State] = null
  private var stdouterr: StdouterrToEvent = null
  private var terminating = false

  protected def key = orderId

  override def postStop() = {
    if (stdouterr != null) stdouterr.close()
    super.postStop()
  }

  def receive = {
    case Input.Recover(o) =>
      assertThat(order == null)
      order = o
      order.state match {
        case Order.Processing => handleEvent(OrderProcessed(Outcome.RecoveryGeneratedOutcome))
        case _ => becomeAsStateOf(order, force = true)
      }
      logger.debug(s"Recovered $order")
      sender() ! Output.RecoveryFinished(order)

    case Input.AddChild(o) =>
      assertThat(order == null)
      order = o
      becomeAsStateOf(order, force = true)

    case Input.AddOffering(o) =>
      assertThat(order == null)
      order = o
      becomeAsStateOf(order, force = true)

    case command: Command =>
      command match {
        case Command.Attach(attached @ Order(`orderId`, wfPos, state: Order.IsFreshOrReady,
          arguments, historicOutcomes, Some(Order.Attached(agentName)), parent, mark, isSuspended, removeWhenTerminated)
        ) =>
          becomeAsStateOf(attached, force = true)
          persist(OrderAttachedToAgent(wfPos, state, arguments, historicOutcomes, agentName, parent, mark,
            isSuspended = isSuspended, removeWhenTerminated = removeWhenTerminated)) {
            (event, updatedState) =>
              update(event :: Nil, updatedState)
              Completed
          } pipeTo sender()

        case _ =>
          executeOtherCommand(command)
      }
  }

  private def fresh = startable

  private def ready: Receive =
    startable orElse receiveCommand

  private def processingCancelled: Receive =
    receiveEvent() orElse receiveCommand

  private def delayedAfterError: Receive =
    startable orElse receiveCommand

  private def startable: Receive =
    receiveEvent() orElse {
      case Input.StartProcessing(jobKey, workflowJob, jobActor, defaultArguments) =>
        assertThat(stdouterr == null)
        stdouterr = new StdouterrToEvent(context, conf.stdouterrToEventConf, writeStdouterr)
        val stdoutWriter = new StatisticalWriter(stdouterr.writers(Stdout))
        val stderrWriter = new StatisticalWriter(stdouterr.writers(Stderr))
        become("processing")(processing(jobKey, workflowJob, jobActor,
          () => (stdoutWriter.isRelevant || stderrWriter.isRelevant) option s"stdout: $stdoutWriter, stderr: $stderrWriter"))
        context.watch(jobActor)
        val orderStarted = order.isState[Order.Fresh] thenList OrderStarted  // OrderStarted automatically with first OrderProcessingStarted
        persistTransaction(orderStarted :+ OrderProcessingStarted) { (events, updatedState) =>
          update(events, updatedState)
          jobActor ! JobActor.Command.ProcessOrder(
            jobKey,
            order.castState[Order.Processing],
            defaultArguments,
            new StdChannels(
              charBufferSize = charBufferSize,
              stdoutWriter = stdoutWriter,
              stderrWriter = stderrWriter))
        }

      case _: Input.Terminate =>
        context.stop(self)
    }

  private def failedOrBroken: Receive =
    receiveEvent() orElse receiveCommand orElse receiveTerminate

  private def processing(jobKey: JobKey, job: WorkflowJob, jobActor: ActorRef, stdoutStderrStatistics: () => Option[String]): Receive =
    receiveCommand orElse receiveEvent(jobActor) orElse {
      case msg: Stdouterr =>  // Handle these events to continue the stdout and stderr threads or the threads will never terminate !!!
        stdouterr.handle(msg)

      case JobActor.Response.OrderProcessed(`orderId`, taskStepEnded, isKilled) =>
        val outcome = taskStepEnded match {
          case TaskStepSucceeded(keyValues, returnCode) =>
            val o = job.toOutcome(returnCode, keyValues)
            if (isKilled) Outcome.Cancelled(o) else o

          case TaskStepFailed(problem) =>
            if (isKilled) Outcome.Cancelled(Outcome.Failed(Some(problem.toString/*???*/), ReturnCode(0)/*TODO*/, Map.empty))
            else Outcome.Disrupted(problem)
        }
        finishProcessing(OrderProcessed(outcome), stdoutStderrStatistics)
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

      case Input.Terminate(signal) =>
        terminating = true
        jobActor ! JobActor.Input.KillProcess(orderId, signal)
    }

  private def finishProcessing(event: OrderProcessed, stdoutStderrStatistics: () => Option[String]): Unit = {
    stdouterr.close()
    stdouterr = null
    for (o <- stdoutStderrStatistics()) logger.debug(o)
    handleEvent(event)
  }

  private def processed: Receive =
    receiveEvent() orElse receiveCommand orElse receiveTerminate

  private def forked: Receive =
    receiveEvent() orElse receiveCommand orElse receiveTerminate

  private def offering: Receive =
    receiveEvent() orElse receiveCommand orElse receiveTerminate

  private def receiveEvent(jobActor: ActorRef = noSender): Receive = {
    case Command.HandleEvent(event) => handleEvent(event, jobActor) pipeTo sender()
  }

  private def handleEvent(event: OrderCoreEvent, jobActor: ActorRef = noSender): Future[Completed] =
    order.update(event) match {
      case Left(problem) =>
        logger.error(problem.toString)
        Future.successful(Completed)

      case Right(updated) =>
        becomeAsStateOf(updated)
        if (event.isInstanceOf[OrderCancelMarked] && updated == order)  // Duplicate, already cancelling with same CancelMode?
          Future.successful(Completed)
        else
          persist(event) { (event, updatedState) =>
            update(event :: Nil, updatedState)
            if (terminating) {
              context.stop(self)
            } else
              event match {
                case OrderCancelMarked(CancelMode.FreshOrStarted(Some(CancelMode.Kill(immediately, maybeWorkflowPos))))
                  if maybeWorkflowPos.forall(_ == order.workflowPosition) && jobActor != noSender =>
                  jobActor ! JobActor.Input.KillProcess(order.id, Some(if (immediately) SIGKILL else SIGTERM))
                case _ =>
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
        case _: Order.ProcessingCancelled  => become("processingCancelled")(processingCancelled)
        case _: Order.DelayedAfterError => become("delayedAfterError")(delayedAfterError)
        case _: Order.Offering   => become("offering")(offering)
        case _: Order.Forked     => become("forked")(forked)
        case _: Order.Failed     => become("failed")(failedOrBroken)
        case _: Order.FailedWhileFresh => become("stoppedWhileFresh")(failedOrBroken)
        case _: Order.FailedInFork => become("failedInFork")(failedOrBroken)
        case _: Order.Broken     => become("broken")(failedOrBroken)
        case _: Order.Awaiting | _: Order.Finished | Order.Cancelled | Order.Removed =>
          sys.error(s"Order is expected to be on Controller, not on Agent: ${order.state}")   // A Finished order must be at Controller
      }
    }
  }

  private def detaching: Receive =
    receiveCommand orElse receiveEvent() orElse receiveTerminate

  private def receiveTerminate: Receive = {
    case _: Input.Terminate =>
      context.stop(self)
  }

  private def receiveCommand: Receive = {
    case command: Command => executeOtherCommand(command)
  }

  private def executeOtherCommand(command: Command): Unit = {
    val msg = s"Improper command $command while in state ${Option(order) map (_.state) getOrElse "(no order)"}"
    logger.error(msg)
    sender() ! Status.Failure(new IllegalStateException(msg))
  }

  private def writeStdouterr(t: StdoutOrStderr, chunk: String): Future[Accepted] =
    if (stdoutCommitDelay == Duration.Zero)  // slow
      persist(OrderStdWritten(t)(chunk)) { (_, _) =>
        Accepted
      }
    else
      persistAcceptEarly(OrderStdWritten(t)(chunk), delay = stdoutCommitDelay)
        .map(_.orThrow)
      // Don't wait for disk-sync. OrderStdWritten is followed by a OrderProcessed, then waiting for disk-sync.

  private def update(events: Seq[OrderEvent], updatedState: AgentState) = {
    events foreach updateOrder
    context.parent ! Output.OrderChanged(order, events)
    if (events.last == OrderDetached) {
      logger.trace("Stopping after OrderDetached")
      order = null
      context.stop(self)
    }
  }

  private def updateOrder(event: OrderEvent) = {
    order = event match {
      case event: OrderAttachedToAgent =>
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
        logger.error(s"Unhandled message $msg in Actor state '$actorStateName', Order state is ${order.state}")

      case _ =>
        super.unhandled(msg)
    }

  override def toString = s"OrderActor(${orderId.string})"
}

private[order] object OrderActor
{
  private[order] def props(orderId: OrderId, journalActor: ActorRef @@ JournalActor.type, conf: OrderActor.Conf)(implicit s: Scheduler) =
    Props { new OrderActor(orderId, journalActor = journalActor, conf) }

  sealed trait Command
  object Command {
    final case class Attach(order: Order[Order.IsFreshOrReady]) extends Command
    final case class HandleEvent(event: OrderCoreEvent) extends Input
  }

  sealed trait Input
  object Input {
    final case class Recover(order: Order[Order.State]) extends Input
    final case class AddChild(order: Order[Order.Ready]) extends Input
    final case class AddOffering(order: Order[Order.Offering]) extends Input
    final case class StartProcessing(jobKey: JobKey, workflowJob: WorkflowJob, jobActor: ActorRef, defaultArguments: Map[String, String])
      extends Input
    final case class Terminate(processSignal: Option[ProcessSignal] = None)
    extends Input with DeadLetterSuppression
  }

  object Output {
    final case class RecoveryFinished(order: Order[Order.State])
    final case class OrderChanged(order: Order[Order.State], events: Seq[OrderEvent])
  }

  final case class Conf(stdoutCommitDelay: FiniteDuration, charBufferSize: Int, stdouterrToEventConf: StdouterrToEvent.Conf,
    journalConf: JournalConf)
  object Conf {
    def apply(config: Config, journalConf: JournalConf) = new Conf(
      stdoutCommitDelay = config.getDuration("js7.order.stdout-stderr.commit-delay").toFiniteDuration,
      charBufferSize    = config.getInt     ("js7.order.stdout-stderr.char-buffer-size"),
      stdouterrToEventConf = StdouterrToEvent.Conf(config),
      journalConf)
  }
}
