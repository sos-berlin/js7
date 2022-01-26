package js7.agent.scheduler.order

import akka.actor.{ActorRef, DeadLetterSuppression, Props, Status}
import akka.pattern.pipe
import js7.agent.data.AgentState
import js7.agent.scheduler.order.OrderActor._
import js7.agent.subagent.SubagentKeeper
import js7.base.generic.Completed
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichCheckedTask
import js7.base.problem.Checked.Ops
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.order.OrderEvent._
import js7.data.order.{Order, OrderEvent, OrderId, OrderMark}
import js7.data.value.expression.Expression
import js7.journal.configuration.JournalConf
import js7.journal.{JournalActor, KeyedJournalingActor}
import monix.execution.Scheduler
import scala.concurrent.Future
import scala.util.{Failure, Success}
import shapeless.tag.@@

/**
  * @author Joacim Zschimmer
  */
final class OrderActor private(
  orderId: OrderId,
  subagentKeeper: SubagentKeeper,
  protected val journalActor: ActorRef @@ JournalActor.type,
  protected val journalConf: JournalConf)
  (implicit protected val scheduler: Scheduler)
extends KeyedJournalingActor[AgentState, OrderEvent]
{
  private val logger = Logger.withPrefix[this.type](orderId.toString)

  private var order: Order[Order.State] = null
  private var terminating = false

  protected def key = orderId

  def receive = {
    case Input.Recover(o) =>
      assertThat(order == null)
      order = o
      for (o <- order.ifState[Order.Processing]) {
        subagentKeeper
          .continueProcessingOrder(
            o,
            events => self ! Internal.UpdateEvents(events))
          .materializeIntoChecked
          .map(_.onProblemHandle(problem => logger.error(s"continueProcessingOrder: $problem")))
          .runAsyncAndForget
      }
      becomeAsStateOf(order, force = true)
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
              update(event :: Nil)
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
    receiveEvent orElse receiveCommand orElse receiveTerminate

  private def delayedAfterError: Receive =
    startable orElse receiveCommand orElse receiveTerminate

  private def startable: Receive =
    receiveEvent orElse {
      case Input.StartProcessing(defaultArguments) =>
        if (order.isProcessable) {
          become("processing")(processing)
          subagentKeeper
            .processOrder(
              order.checkedState[Order.IsFreshOrReady].orThrow,
              defaultArguments,
              events => self ! Internal.UpdateEvents(events))
            .materialize
            .foreach {
              case Failure(t) => logger.error(t.toStringWithCauses)  // OrderFailed ???
              case Success(Left(problem)) => logger.error(problem.toString)  // ???
              case Success(Right(())) =>
            }
        }

      case _: Input.Terminate =>
        context.stop(self)
    }

  private def processing: Receive =
    receiveCommand orElse receiveEvent orElse {
      case Internal.UpdateEvents(events) =>
        update(events)

      case Input.Terminate(signal) =>
        terminating = true
        if (subagentKeeper.orderIsLocal(orderId)) {
          for (signal <- signal) {
            subagentKeeper
              .killProcess(orderId, signal)
              .runAsyncAndForget
          }
        } else {
          context.stop(self)
        }
    }

  private def processed: Receive =
    receiveEvent orElse receiveCommand orElse receiveTerminate

  private def standard: Receive =
    receiveEvent orElse receiveCommand orElse receiveTerminate

  private def receiveEvent: Receive = {
    case Command.HandleEvents(events) => handleEvents(events) pipeTo sender()
  }

  private def handleEvent(event: OrderCoreEvent)
  : Future[Completed] =
    handleEvents(event :: Nil)

  private def handleEvents(events: Seq[OrderCoreEvent]): Future[Completed] =
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
            update(events)
            if (terminating) {
              context.stop(self)
            } else
              event foreach {
                case OrderKillingMarked(Some(kill)) => maybeKillOrder(kill)
                case _ =>
              }
            Completed
          }
    }

  private def maybeKillOrder(): Unit =
    order.mark match {
      case Some(OrderMark.Cancelling(CancellationMode.FreshOrStarted(Some(kill)))) =>
        maybeKillOrder(kill)

      case Some(OrderMark.Suspending(SuspensionMode(Some(kill)))) =>
        maybeKillOrder(kill)

      case _ =>
    }

  private def maybeKillOrder(kill: CancellationMode.Kill): Unit =
    if (kill.workflowPosition.forall(_ == order.workflowPosition)) {
      subagentKeeper
        .killProcess(
          order.id,
          if (kill.immediately) SIGKILL else SIGTERM)
        .runAsyncAndForget
    }

  private def becomeAsStateOf(anOrder: Order[Order.State], force: Boolean = false): Unit = {
    if (anOrder.isDetaching)
      become("detaching")(detaching)
    else
    if (force || anOrder.state.getClass != order.state.getClass) {
      anOrder.state match {
        case _: Order.Fresh             => become("fresh")(fresh)
        case _: Order.Ready             => become("ready")(ready)
        case _: Order.Processing        => become("processing")(processing)
        case _: Order.Processed         => become("processed")(processed)
        case _: Order.ProcessingKilled  => become("processingKilled")(processingKilled)
        case _: Order.DelayedAfterError => become("delayedAfterError")(delayedAfterError)
        case _: Order.Forked            => become("forked")(standard)
        case _: Order.BetweenCycles     => become("forked")(standard)
        case _: Order.Failed            => become("failed")(standard)
        case _: Order.FailedWhileFresh  => become("stoppedWhileFresh")(standard)
        case _: Order.FailedInFork      => become("failedInFork")(standard)
        case _: Order.Broken            => become("broken")(standard)
        case Order.WaitingForLock | _: Order.ExpectingNotice | _: Order.Prompting |
             Order.Finished | Order.Cancelled | Order.Deleted =>
          sys.error(s"Order is expected to be at the Controller, not at Agent: ${order.state}")   // A Finished order must be at Controller
      }
    }
  }

  private def detaching: Receive =
    receiveCommand orElse receiveEvent orElse receiveTerminate

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

  private def update(events: Seq[OrderEvent]) = {
    events foreach updateOrder
    context.parent ! Output.OrderChanged(order, events)
    events.last match {
      case OrderDetached =>
        logger.trace("Stopping after OrderDetached")
        order = null
        context.stop(self)

      case _: OrderProcessed =>
        maybeKillOrder()
        if (terminating) {
          context.stop(self)
        } else {
          become("processed")(processed)
        }
      case _ =>
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
    subagentKeeper: SubagentKeeper,
    journalActor: ActorRef @@ JournalActor.type,
    journalConf: JournalConf)
    (implicit s: Scheduler) =
    Props { new OrderActor(orderId, subagentKeeper, journalActor = journalActor, journalConf) }

  private object Internal {
    final case class UpdateEvents(events: Seq[OrderEvent])
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

    final case class StartProcessing(defaultArguments: Map[String, Expression])
    extends Input

    final case class Terminate(processSignal: Option[ProcessSignal] = None)
    extends Input with DeadLetterSuppression
  }

  object Output {
    final case class RecoveryFinished(order: Order[Order.State])
    final case class OrderChanged(order: Order[Order.State], events: Seq[OrderEvent])
  }
}
