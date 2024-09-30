package js7.agent.scheduler.order

import cats.effect.unsafe.IORuntime
import cats.effect.{FiberIO, IO}
import com.softwaremill.tagging.@@
import js7.agent.data.AgentState
import js7.agent.scheduler.order.OrderActor.*
import js7.base.catsutils.CatsEffectExtensions.materializeIntoChecked
import js7.base.generic.Completed
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.log.{CorrelId, Logger}
import js7.base.monixlike.MonixLikeExtensions.{foreach, materialize}
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.order.OrderEvent.*
import js7.data.order.{Order, OrderEvent, OrderId, OrderMark}
import js7.journal.configuration.JournalConf
import js7.journal.{JournalActor, KeyedJournalingActor}
import js7.subagent.director.SubagentKeeper
import org.apache.pekko.actor.{ActorRef, DeadLetterSuppression, Props, Status}
import org.apache.pekko.pattern.pipe
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class OrderActor private(
  orderId: OrderId,
  orderCorrelId: CorrelId,
  subagentKeeper: SubagentKeeper[AgentState],
  protected val journalActor: ActorRef @@ JournalActor.type,
  protected val journalConf: JournalConf)
  (implicit protected val ioRuntime: IORuntime)
extends KeyedJournalingActor[AgentState, OrderEvent]:

  private given ExecutionContext = ioRuntime.compute

  private val logger = Logger.withPrefix[this.type](orderId.toString)

  private var order: Order[Order.State] = null
  private var terminating = false

  protected def key = orderId.asInstanceOf[E.Key]/*???*/

  def receive =
    case Input.Recover(o) =>
      assertThat(order == null)
      order = o
      becomeAsStateOf(order, force = true)
      val sender = this.sender()
      order.ifState[Order.Processing] match
        case None =>
          sender ! Output.RecoveryFinished

        case Some(o) =>
          // TODO Process all recovered Orders in a batch!
          subagentKeeper
            .recoverOrderProcessing(
              o,
              events => self ! Internal.UpdateEvents(events, orderCorrelId))
            .materializeIntoChecked
            .flatTap:
              case Left(problem) => IO(logger.error(s"recoverOrderProcessing(${o.id}): $problem"))
              case Right(_) => IO.unit
            .foreach { (_: Checked[FiberIO[OrderProcessed]]) =>
              sender ! Output.RecoveryFinished
            }

    case Input.AddChild(o) =>
      assertThat(order == null)
      order = o
      becomeAsStateOf(order, force = true)

    case command: Command =>
      command match
        case Command.Attach(attachedOrder, correlId) if attachedOrder.id == orderId =>
          correlId.bind[Unit]:
            attachedOrder.toOrderAttachedToAgent match
              case Left(problem) =>
                logger.error(problem.toString)
                sender() ! Status.Failure(problem.throwable)

              case Right(orderAttachedToAgent) =>
                persist(orderAttachedToAgent) {
                  (event, updatedState) =>
                    becomeAsStateOf(attachedOrder, force = true)
                    update(event :: Nil)
                    Completed
                } pipeTo sender()

        case _ =>
          executeOtherCommand(command)

  private def fresh = startable

  private def ready: Receive =
    startable orElse receiveCommand orElse receiveTerminate

  private def processingKilled: Receive =
    receiveEvent orElse receiveCommand orElse receiveTerminate

  private def delayedAfterError: Receive =
    startable orElse receiveCommand orElse receiveTerminate

  private def startable: Receive =
    receiveEvent orElse:
      case Input.StartProcessing =>
        orderCorrelId.bind[Unit]:
          if !order.isProcessable then
            logger.warn("Input.StartProcessing but !order.isProcessable")
          else
            // Separate CorrelId for each order process
            CorrelId.bindNew:
              IO.defer:
                subagentKeeper
                  .processOrder(
                    order.checkedState[Order.IsFreshOrReady].orThrow,
                    events => self ! Internal.UpdateEvents(events, orderCorrelId))
                  .materialize
                  .map:
                    case Failure(t) =>
                      logger.error(s"startOrderProcessing => ${t.toStringWithCauses}") // OrderFailed ???

                    //case Success(Left(problem: SubagentDriverStoppedProblem)) =>
                    //  logger.debug(s"startOrderProcessing => $problem")

                    case Success(Left(problem)) =>
                      logger.error(s"startOrderProcessing => $problem")  // ???

                    case Success(Right(())) =>
              .unsafeToFuture()

      case _: Input.Terminate =>
        context.stop(self)

  private def processing: Receive =
    receiveEvent orElse receiveCommand orElse:
      case Input.Terminate(signal) =>
        terminating = true
        if subagentKeeper.orderIsLocal(orderId) then
          for signal <- signal do
            subagentKeeper
              .killProcess(orderId, signal)
              .unsafeRunAndForget()
        else
          context.stop(self)

  private def processed: Receive =
    receiveEvent orElse receiveCommand orElse receiveTerminate

  private def standard: Receive =
    receiveEvent orElse receiveCommand orElse receiveTerminate

  private def receiveEvent: Receive =
    case Internal.UpdateEvents(events, correlId) =>
      correlId.bind:
        update(events)

    case Command.HandleEvents(events, correlId) =>
      correlId.bind[Unit]:
        handleEvents(events) pipeTo sender()

  private def handleEvents(events: Seq[OrderCoreEvent]): Future[Checked[Completed]] =
    order.applyEvents(events) match
      case Left(problem) =>
        Future.successful(Left(problem))

      case Right(updated) =>
        if events.size == 1 && events.head.isInstanceOf[OrderCancellationMarked] && updated == order then  // Duplicate, already cancelling with same CancellationMode?
          Future.successful(Right(Completed))
        else
          persistTransactionReturnChecked(events) { (event, updatedState) =>
            update(events)
            if terminating then
              context.stop(self)
            else
              event foreach:
                case OrderKillingMarked(Some(kill)) => maybeKillOrder(kill)
                case _ =>
            Completed
          }

  private def maybeKillOrder(): Unit =
    if order.state.isInstanceOf[Order.Processing] then
      order.mark match
        case Some(OrderMark.Cancelling(CancellationMode.FreshOrStarted(Some(kill)))) =>
          maybeKillOrder(kill)

        case Some(OrderMark.Suspending(SuspensionMode(_, Some(mode)))) =>
          maybeKillOrder(mode)

        case _ =>

  private def maybeKillOrder(kill: CancellationMode.Kill): Unit =
    if kill.workflowPosition.forall(_ == order.workflowPosition) then
      subagentKeeper
        .killProcess(
          order.id,
          if kill.immediately then SIGKILL else SIGTERM)
        .unsafeRunAndForget()

  private def becomeAsStateOf(anOrder: Order[Order.State], force: Boolean = false): Unit =
    if anOrder.isDetaching then
      become("detaching")(detaching)
    else
    if true || force || anOrder.state.getClass != order.state.getClass then
      anOrder.state match
        case _: Order.Fresh             => become("fresh")(wrap(fresh))
        case _: Order.Ready             => become("ready")(wrap(ready))
        case _: Order.Processing        => become("processing")(wrap(processing))
        case _: Order.Processed         => become("processed")(wrap(processed))
        case _: Order.ProcessingKilled  => become("processingKilled")(wrap(processingKilled))
        case _: Order.DelayingRetry     => become("delayingRetry")(wrap(delayedAfterError))
        case _: Order.DelayedAfterError => become("delayedAfterError")(wrap(delayedAfterError))
        case _: Order.Forked            => become("forked")(wrap(standard))
        case _: Order.BetweenCycles     => become("forked")(wrap(standard))
        case _: Order.Failed            => become("failed")(wrap(standard))
        case _: Order.FailedWhileFresh  => become("failedWhileFresh")(wrap(standard))
        case _: Order.Stopped           => become("stopped")(wrap(standard))
        case _: Order.StoppedWhileFresh => become("stoppedWhileFresh")(wrap(standard))
        case _: Order.FailedInFork      => become("failedInFork")(wrap(standard))
        case _: Order.Broken            => become("broken")(wrap(standard))
        case Order.WaitingForLock | _: Order.ExpectingNotice | _: Order.ExpectingNotices |
             _: Order.Prompting | Order.Finished | Order.Cancelled | Order.Deleted =>
          sys.error(s"Order is expected to be at the Controller, not at Agent: ${order.state}")   // A Finished order must be at Controller

  private def wrap(receive: Receive): Receive =
    case msg if receive.isDefinedAt(msg) =>
      orderCorrelId.bind:
        receive(msg)

  private def detaching: Receive =
    receiveEvent orElse receiveCommand orElse receiveTerminate

  private def receiveTerminate: Receive =
    case _: Input.Terminate =>
      context.stop(self)

  private def receiveCommand: Receive =
    case command: Command => executeOtherCommand(command)

  private def executeOtherCommand(command: Command): Unit =
    val msg = s"Improper command $command while in state ${Option(order).map(_.state) getOrElse "(no order)"}"
    logger.error(msg)
    sender() ! Status.Failure(new IllegalStateException(msg))

  private def update(events: Seq[OrderEvent]): Unit =
    val previousOrderOrNull = order
    events foreach updateOrder
    becomeAsStateOf(order)
    context.parent ! Output.OrderChanged(orderId, CorrelId.current, previousOrderOrNull, events)
    events.last match
      case OrderDetached =>
        logger.trace("Stopping after OrderDetached")
        order = null
        context.stop(self)

      case _: OrderProcessingStarted =>
        maybeKillOrder()

      case _: OrderProcessed =>
        if terminating then
          context.stop(self)
      case _ =>

  private def updateOrder(event: OrderEvent): Unit =
    order = event match
      case event: OrderAttachedToAgent =>
        Order.fromOrderAttached(orderId, event)

      case _: OrderStdWritten =>
        // Not collected
        order

      case event: OrderCoreEvent if order != null =>
        order.applyEvent(event).orThrow/*!!!*/

      case _ =>
        sys.error(s"Unexpected event for '$orderId': $event")

  override def unhandled(msg: Any): Unit =
    msg match
      case msg @ (_: Command | _: Input | _: Internal.UpdateEvents) =>
        logger.error(s"Unhandled message $msg in Actor state '$actorStateName', Order state is ${order.state}")

      case _ =>
        super.unhandled(msg)

  override def toString = s"OrderActor(${orderId.string})"

private[order] object OrderActor:
  private[order] def props(
    orderId: OrderId,
    correlId: CorrelId,
    subagentKeeper: SubagentKeeper[AgentState],
    journalActor: ActorRef @@ JournalActor.type,
    journalConf: JournalConf)
    (using IORuntime) =
    Props { new OrderActor(
      orderId, correlId, subagentKeeper, journalActor = journalActor, journalConf)
    }

  private object Internal:
    final case class UpdateEvents(events: Seq[OrderEvent], correlId: CorrelId)

  sealed trait Command
  object Command:
    final case class Attach(order: Order[Order.IsFreshOrReady], correlId: CorrelId)
    extends Command

    final case class HandleEvents(event: Seq[OrderCoreEvent], correlId: CorrelId)
    extends Input

  sealed trait Input
  object Input:
    final case class Recover(order: Order[Order.State]) extends Input
    final case class AddChild(order: Order[Order.Ready]) extends Input

    case object StartProcessing
    extends Input

    final case class Terminate(processSignal: Option[ProcessSignal] = None)
    extends Input, DeadLetterSuppression

  object Output:
    case object RecoveryFinished
    final case class OrderChanged(
      orderId: OrderId,
      correlId: CorrelId,
      previousOrderOrNull: Order[Order.State],
      events: Seq[OrderEvent])
