package js7.agent.scheduler.order

import akka.actor.{ActorRef, DeadLetterSuppression, Props, Status}
import akka.pattern.pipe
import com.typesafe.config.Config
import js7.agent.data.AgentState
import js7.agent.scheduler.job.JobDriver
import js7.agent.scheduler.order.OrderActor._
import js7.base.configutils.Configs.RichConfig
import js7.base.generic.{Accepted, Completed}
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.io.process.{ProcessSignal, Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Checked.Ops
import js7.base.time.JavaTimeConverters._
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.chunkStrings
import js7.base.utils.ScalaUtils.syntax._
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.controller.ControllerId
import js7.data.order.OrderEvent._
import js7.data.order.{Order, OrderEvent, OrderId, OrderMark, Outcome}
import js7.data.value.expression.Expression
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.executor.StdObservers
import js7.journal.configuration.JournalConf
import js7.journal.{JournalActor, KeyedJournalingActor}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import scala.concurrent.Future
import scala.concurrent.duration._
import shapeless.tag.@@

/**
  * @author Joacim Zschimmer
  */
final class OrderActor private(
  orderId: OrderId,
  protected val journalActor: ActorRef @@ JournalActor.type,
  conf: Conf)
  (implicit protected val scheduler: Scheduler)
extends KeyedJournalingActor[AgentState, OrderEvent]
{
  private val logger = Logger.withPrefix[this.type](orderId.toString)
  import conf.{charBufferSize, stdoutCommitDelay}

  protected def journalConf = conf.journalConf
  private var order: Order[Order.State] = null
  private var terminating = false

  protected def key = orderId

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

    case command: Command =>
      command match {
        case Command.Attach(attached @ Order(`orderId`, wfPos, state: Order.IsFreshOrReady,
          arguments, scheduledFor, externalOrderKey, historicOutcomes,
          Some(Order.Attached(agentPath)), parent, mark, isSuspended, removeWhenTerminated)
        ) =>
          becomeAsStateOf(attached, force = true)
          persist(OrderAttachedToAgent(wfPos, state, arguments, scheduledFor, externalOrderKey,
            historicOutcomes, agentPath, parent, mark,
            isSuspended = isSuspended, deleteWhenTerminated = removeWhenTerminated)) {
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
    startable orElse receiveCommand orElse receiveTerminate

  private def processingKilled: Receive =
    receiveEvent() orElse receiveCommand orElse receiveTerminate

  private def delayedAfterError: Receive =
    startable orElse receiveCommand orElse receiveTerminate

  private def startable: Receive =
    receiveEvent() orElse {
      case Input.StartProcessing(jobDriver, workflowJob, defaultArguments) =>
        if (order.isProcessable) {
          val out, err = PublishSubject[String]()
          val outErrStatistics = Map(Stdout -> new OutErrStatistics, Stderr -> new OutErrStatistics)

          def writeObservableAsEvents(outerr: StdoutOrStderr, observable: Observable[String]) =
            observable
              .buffer(Some(conf.stdouterr.delay), conf.stdouterr.chunkSize, toWeight = _.length)
              .flatMap(strings => Observable.fromIterable(chunkStrings(strings, conf.stdouterr.chunkSize)))
              .flatMap(chunk => Observable.fromTask(
                outErrStatistics(outerr).count(
                  chunk.length,
                  persistStdouterr(outerr, chunk))))
              .completedL

          val outErrCompleted = Task
            .parZip2(
              writeObservableAsEvents(Stdout, out),
              writeObservableAsEvents(Stderr, err))
            .void.runToFuture

          val stdObservers = new StdObservers(out, err, charBufferSize = charBufferSize,
            keepLastErrLine = workflowJob.failOnErrWritten)

          become("processing")(
            processing(jobDriver, stdObservers, outErrCompleted,
              () => (outErrStatistics(Stdout).isRelevant || outErrStatistics(Stderr).isRelevant) ?
                s"stdout: ${outErrStatistics(Stdout)}, stderr: ${outErrStatistics(Stderr)}"))

          // OrderStarted automatically with first OrderProcessingStarted
          val orderStarted = order.isState[Order.Fresh] thenList OrderStarted
          persistTransaction(orderStarted :+ OrderProcessingStarted) { (events, updatedState) =>
            update(events, updatedState)
            jobDriver
              .processOrder(
                order.castState[Order.Processing],
                defaultArguments,
                stdObservers)
              .tapEval(outcome => Task {
                self ! Internal.OrderProcessed(orderId, outcome)
              })
              .runAsyncAndForget
            maybeKillOrder(Some(jobDriver))
          }
        }

      case _: Input.Terminate =>
        context.stop(self)
    }

  private def failedOrBroken: Receive =
    receiveEvent() orElse receiveCommand orElse receiveTerminate

  private def processing(jobDriver: JobDriver, stdObservers: StdObservers, outerrCompleted: Future[Unit],
    stdoutStderrStatistics: () => Option[String])
  : Receive =
    receiveCommand orElse receiveEvent(Some(jobDriver)) orElse {
      case Internal.OrderProcessed(`orderId`, outcome) =>
        stdObservers.stop/*may already be stopped by OrderProcess/JobDriver*/
          .flatMap(_ => Task.fromFuture(outerrCompleted))
          .runToFuture
          .onComplete { _ =>
            self ! Internal.OutErrCompleted(outcome)
          }

      case Internal.OutErrCompleted(outcome) =>
        finishProcessing(OrderProcessed(outcome), stdoutStderrStatistics)

      case Input.Terminate(signal) =>
        terminating = true
        for (signal <- signal) {
          jobDriver.killOrder(orderId, signal)
            .runAsyncAndForget
        }
    }

  private def finishProcessing(event: OrderProcessed, stdoutStderrStatistics: () => Option[String]): Unit = {
    for (o <- stdoutStderrStatistics()) logger.debug(o)
    handleEvent(event)
  }

  private def processed: Receive =
    receiveEvent() orElse receiveCommand orElse receiveTerminate

  private def forked: Receive =
    receiveEvent() orElse receiveCommand orElse receiveTerminate

  private def receiveEvent(jobDriver: Option[JobDriver] = None): Receive = {
    case Command.HandleEvents(events) => handleEvents(events, jobDriver) pipeTo sender()
  }

  private def handleEvent(event: OrderCoreEvent, jobDriver: Option[JobDriver] = None)
  : Future[Completed] =
    handleEvents(event :: Nil, jobDriver)

  private def handleEvents(events: Seq[OrderCoreEvent], jobDriver: Option[JobDriver] = None)
  : Future[Completed] =
    order.applyEvents(events) match {
      case Left(problem) =>
        logger.error(problem.toString)
        Future.successful(Completed)

      case Right(updated) =>
        becomeAsStateOf(updated)
        if (events.size == 1 && events.head.isInstanceOf[OrderCancellationMarked] && updated == order)  // Duplicate, already cancelling with same CancellationMode?
          Future.successful(Completed)
        else
          persistTransaction(events) { (event, updatedState) =>
            update(events, updatedState)
            if (terminating) {
              context.stop(self)
            } else
              event foreach {
                case OrderKillingMarked(Some(kill)) => maybeKillOrder(kill, jobDriver)
                case _ =>
              }
            Completed
          }
    }

  private def maybeKillOrder(jobDriver: Option[JobDriver]): Unit =
    order.mark match {
      case Some(OrderMark.Cancelling(CancellationMode.FreshOrStarted(Some(kill)))) =>
        maybeKillOrder(kill, jobDriver)

      case Some(OrderMark.Suspending(SuspensionMode(Some(kill)))) =>
        maybeKillOrder(kill, jobDriver)

      case _ =>
    }

  private def maybeKillOrder(kill: CancellationMode.Kill, jobDriver: Option[JobDriver]): Unit =
    for (jobDriver <- jobDriver) {
      if (kill.workflowPosition.forall(_ == order.workflowPosition)) {
        jobDriver
          .killOrder(
            order.id,
            if (kill.immediately) SIGKILL else SIGTERM)
          .runAsyncAndForget
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
        case _: Order.ProcessingKilled  => become("processingKilled")(processingKilled)
        case _: Order.DelayedAfterError => become("delayedAfterError")(delayedAfterError)
        case _: Order.Forked     => become("forked")(forked)
        case _: Order.Failed     => become("failed")(failedOrBroken)
        case _: Order.FailedWhileFresh => become("stoppedWhileFresh")(failedOrBroken)
        case _: Order.FailedInFork => become("failedInFork")(failedOrBroken)
        case _: Order.Broken     => become("broken")(failedOrBroken)
        case Order.WaitingForLock | _: Order.ExpectingNotice | _: Order.Prompting |
             Order.Finished | Order.Cancelled | Order.Deleted =>
          sys.error(s"Order is expected to be at the Controller, not at Agent: ${order.state}")   // A Finished order must be at Controller
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
    val msg = s"Improper command $command while in state ${Option(order).map(_.state) getOrElse "(no order)"}"
    logger.error(msg)
    sender() ! Status.Failure(new IllegalStateException(msg))
  }

  private def persistStdouterr(t: StdoutOrStderr, chunk: String): Task[Accepted] =
    if (stdoutCommitDelay.isZero)  // slow
      persistTask(OrderStdWritten(t)(chunk)) { (_, _) =>
        Accepted
      }.map(_.orThrow)
    else
      persistAcceptEarlyTask(OrderStdWritten(t)(chunk), delay = stdoutCommitDelay)
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
        order.applyEvent(event).orThrow/*!!!*/

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
  private[order] def props(
    orderId: OrderId,
    journalActor: ActorRef @@ JournalActor.type,
    conf: OrderActor.Conf)
    (implicit s: Scheduler) =
    Props { new OrderActor(orderId, journalActor = journalActor, conf) }

  private object Internal {
    final case class OrderProcessed(orderId: OrderId, outcome: Outcome)
    final case class OutErrCompleted(outcome: Outcome)
  }

  sealed trait Command
  object Command {
    final case class Attach(order: Order[Order.IsFreshOrReady]) extends Command
    final case class HandleEvents(event: Seq[OrderCoreEvent]) extends Input
  }

  sealed trait Input
  object Input {
    final case class Recover(order: Order[Order.State]) extends Input
    final case class AddChild(order: Order[Order.Ready]) extends Input

    final case class StartProcessing(
      jobDriver: JobDriver,
      workflowJob: WorkflowJob,
      defaultArguments: Map[String, Expression])
    extends Input

    final case class Terminate(processSignal: Option[ProcessSignal] = None)
    extends Input with DeadLetterSuppression
  }

  object Output {
    final case class RecoveryFinished(order: Order[Order.State])
    final case class OrderChanged(order: Order[Order.State], events: Seq[OrderEvent])
  }

  final case class Conf(
    stdoutCommitDelay: FiniteDuration,
    charBufferSize: Int,
    journalConf: JournalConf,
    stdouterr: StdouterrConf)
  object Conf {
    def apply(config: Config, journalConf: JournalConf) = {
      val outErrConf = StdouterrConf(config)
      new Conf(
        stdoutCommitDelay = config.getDuration("js7.order.stdout-stderr.commit-delay").toFiniteDuration,
        charBufferSize    = config.memorySizeAsInt("js7.order.stdout-stderr.char-buffer-size").orThrow
                              .min(outErrConf.chunkSize),
        journalConf,
        outErrConf)
    }
  }

  final case class StdouterrConf(chunkSize: Int, delay: FiniteDuration/*, noDelayAfter: FiniteDuration*/)
  object StdouterrConf {
    def apply(config: Config): StdouterrConf = new StdouterrConf(
      chunkSize    = config.memorySizeAsInt("js7.order.stdout-stderr.chunk-size").orThrow,
      delay        = config.getDuration("js7.order.stdout-stderr.delay").toFiniteDuration)
      //noDelayAfter = config.getDuration("js7.order.stdout-stderr.no-delay-after").toFiniteDuration)
  }
}
