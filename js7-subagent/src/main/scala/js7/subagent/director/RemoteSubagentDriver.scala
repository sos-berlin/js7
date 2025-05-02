package js7.subagent.director

import cats.effect.{Deferred, FiberIO, IO, ResourceIO}
import cats.syntax.flatMap.*
import com.typesafe.config.Config
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.catsutils.CatsExtensions.traverseCombine
import js7.base.configutils.Configs.RichConfig
import js7.base.crypt.Signed
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, CorrelIdWrapped, Logger}
import js7.base.monixlike.MonixLikeExtensions.{materialize, onErrorRestartLoop}
import js7.base.monixutils.{AsyncVariable, Switch}
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.service.Service
import js7.base.stream.Numbered
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, DelayConf, SetOnce}
import js7.base.web.HttpClient
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.common.system.PlatformInfos.currentPlatformInfo
import js7.data.controller.ControllerId
import js7.data.event.{EventId, KeyedEvent}
import js7.data.item.{InventoryItemKey, ItemRevision, SignableItem}
import js7.data.job.JobKey
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.other.HeartbeatTiming
import js7.data.subagent.Problems.{ProcessLostDueToResetProblem, ProcessLostDueToRestartProblem, ProcessLostProblem, SubagentIsShuttingDownProblem, SubagentNotDedicatedProblem, SubagentShutDownBeforeProcessStartProblem}
import js7.data.subagent.SubagentCommand.{AttachSignedItem, CoupleDirector, DedicateSubagent, KillProcess, StartOrderProcess}
import js7.data.subagent.SubagentItemStateEvent.{SubagentCouplingFailed, SubagentDedicated, SubagentDied, SubagentReset, SubagentRestarted}
import js7.data.subagent.{SubagentCommand, SubagentDirectorState, SubagentItem, SubagentItemState, SubagentRunId}
import js7.journal.Journal
import js7.subagent.director.RemoteSubagentDriver.*
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

// TODO Some bookkeeping desired?
// - Which operations (SubagentCommand) on SubagentItem are ongoing?
// - dedicate, couple, reset, delete, ...
// - Which operations may overlap with, wait for or cancel the older?

private final class RemoteSubagentDriver private(
  // Change of disabled does not change this subagentItem.
  // Then, it differs from the original SubagentItem
  val subagentItem: SubagentItem,
  protected val api: HttpSubagentApi,
  protected val journal: Journal[? <: SubagentDirectorState[?]],
  controllerId: ControllerId,
  protected val conf: RemoteSubagentDriver.Conf,
  protected val recouplingStreamReaderConf: RecouplingStreamReaderConf)
extends SubagentDriver, Service.StoppableByRequest, SubagentEventListener:

  private val logger = Logger.withPrefix[this.type](subagentItem.pathRev.toString)
  private val resetLock = AsyncLock()
  private val dispatcher = new SubagentDispatcher(subagentId, postQueuedCommand)
  private val attachedItemKeys = AsyncVariable(Map.empty[InventoryItemKey, Option[ItemRevision]])
  private val initiallyCoupled = SetOnce[SubagentRunId]
  @volatile private var lastSubagentRunId: Option[SubagentRunId] = None
  @volatile private var shuttingDown = false

  def isLocal: Boolean =
    api.isLocal

  protected def isShuttingDown = shuttingDown

  protected def start =
    startService(
      untilStopRequested *> onStop)

  private def onStop =
    IO.both(dispatcher.shutdown, stopEventListener)
      .*>(api.tryLogout.void)
      .logWhenItTakesLonger(s"RemoteSubagentDriver($subagentId).stop")

  def startObserving =
    startEventListener

  def startMovedSubagent(previous: RemoteSubagentDriver): IO[Unit] =
    logger.debugIO:
      //previous.stopDispatcherAndEmitProcessLostEvents(None) *>
      IO.race(
        untilStopRequested,
        initiallyCoupled.io)
      .flatMap:
        case Left(())/*stopped*/ => IO.unit
        case Right(subagentRunId) =>
          logger.debug(
            s"startMovedSubagent(${previous.lastSubagentRunId} $previous ${previous.hashCode}): this=$subagentRunId")
          // Does not work, so we kill all processes. FIXME Do we kill them?
          //if (previous.lastSubagentRunId contains subagentRunId)
          //  // Same SubagentRunId continues. So we transfer the command queue.
          //  dispatcher.enqueueExecutes(previous.dispatcher)
          //else
            /*previous.stopDispatcherAndEmitProcessLostEvents(None)
          .*>*/(IO.unit/*dispatcher.start(subagentRunId)*/)

  def terminate: IO[Unit] =
    logger.traceIO:
      stop

  def stopJobs(jobKeys: Iterable[JobKey], signal: ProcessSignal) =
    // TODO stop RemoteSubagentDriver jobs (and detach Workflows and JobResources!)
    IO.unit

  def reset(force: Boolean, dontContinue: Boolean = false): IO[Unit] =
    logger.debugIO:
      resetLock.lock(IO.defer:
        val wasHeartbeating = isHeartbeating
        // Stop listening events before we mark order processes as lost.
        // Eventual event from Subagent must not interfere without
        // OrderProcessed .processLost.
        // Unfortunately, this will inhibit SubagentShutdown event, too.
        stopEventListener
          .*>(IO.whenA((wasHeartbeating || force) && !suppressResetShutdown)(
            // One shot. Processes are killed only if Subagent is reachable.
            // May delay ProcessLost and SubagentReset for connection timeout.
            tryShutdownSubagent(processSignal = Some(SIGKILL), dontWaitForDirector = true)))
          .*>(onSubagentDied(ProcessLostDueToResetProblem, SubagentReset))
          .*>(IO.unlessA(dontContinue)(startEventListener)))

  private def suppressResetShutdown =
    conf.config.hasPath("js7.tests.RemoteSubagentDriver.suppressResetShutdown")

  def tryShutdown: IO[Unit] =
    logger.debugIO:
      IO.defer:
        shuttingDown = true
        // Wait until no Order is being processed
        orderToDeferred.stop
        // Emit event and change state ???
      .*>(tryShutdownSubagent())

  //def suspend: IO[Unit] =
  //  dispatcher.suspend *> stopEventListener

  private def tryShutdownSubagent(
    processSignal: Option[ProcessSignal] = None,
    dontWaitForDirector: Boolean = false)
  : IO[Unit] =
    IO.defer:
      shuttingDown = true
      api
        .executeSubagentCommand(Numbered(0,
          SubagentCommand.ShutDown(processSignal, dontWaitForDirector = dontWaitForDirector,
            restart = true)))
        .rightAs(())
        .timeoutTo(conf.subagentResetTimeout, IO:
          logger.error:
            s"$subagentId did not respond to Shutdown command for ${conf.subagentResetTimeout}"
          Checked.unit)
        .orThrow
        .handleError: t =>  // Ignore when Subagent is unreachable
          logger.error(s"SubagentCommand.ShutDown => ${t.toStringWithCauses}")

  protected def dedicateOrCouple: IO[Checked[(SubagentRunId, EventId)]] =
    logger.debugIO:
      currentSubagentItemState
        .flatMapT: subagentItemState =>
          dedicateOrCouple2(subagentItemState).map(Right(_))

  private def dedicateOrCouple2(subagentItemState: SubagentItemState): IO[(SubagentRunId, EventId)] =
    subagentItemState.subagentRunId match
      case None =>
        for
          response <- dedicate
          eventId <- couple(response.subagentRunId, response.subagentEventId)
        yield (response.subagentRunId, eventId)

      case Some(subagentRunId) =>
        couple(subagentRunId, subagentItemState.eventId)
          .map(subagentRunId -> _)

  private def dedicate: IO[DedicateSubagent.Response] =
    val agentRunId = journal.unsafeAggregate().agentRunId
    val cmd = DedicateSubagent(subagentId, subagentItem.agentPath, agentRunId, controllerId)
    logger.debugIO:
      postCommandUntilSucceeded(cmd)
        .flatMap(response => journal
          .persistKeyedEvent:
            subagentId <-: SubagentDedicated(response.subagentRunId, Some(currentPlatformInfo()))
          .flatTap(checked => IO.whenA(checked.isRight)(IO:
            lastSubagentRunId = Some(response.subagentRunId)
            shuttingDown = false))
          .rightAs(response)
          .orThrow)

  private def couple(subagentRunId: SubagentRunId, eventId: EventId): IO[EventId] =
    logger.traceIO(cancelAndFailWhenStopping:
      val cmd = CoupleDirector(subagentId, subagentRunId, eventId, conf.heartbeatTiming)
      api.login(onlyIfNotLoggedIn = true)
        .*>(api.executeSubagentCommand(Numbered(0, cmd)).orThrow)
        .as(subagentRunId -> eventId)
        .onErrorRestartLoop(()):
          case (ProblemException(SubagentNotDedicatedProblem), _, _) =>
            orderToDeferred.toMap.size match
              case 0 => logger.info("Subagent restarted")
              case n => logger.warn(s"Subagent restarted, $n Order processes are lost")
            onSubagentDied(ProcessLostDueToRestartProblem, SubagentRestarted)
              .*>(dedicate)
              .map(response => response.subagentRunId -> response.subagentEventId)

          case (throwable, _, retry) =>
            logger.warn(s"CoupleDirector failed: ${throwable.toStringWithCauses}")
            emitSubagentCouplingFailed(Some(HttpClient.throwableToProblem(throwable)))
              .*>(IO.sleep(reconnectErrorDelay))
              .*>(retry(()))
        .flatTap: (subagentRunId, _) =>
          IO:
            shuttingDown = false
            initiallyCoupled.trySet(subagentRunId)
          *>
            dispatcher.start(subagentRunId)  // Dispatcher may have been stopped after SubagentReset
        .map(_._2/*EventId*/))

  // May run concurrently with onStartOrderProcessFailed !!!
  // We make sure that only one OrderProcessed event is emitted.
  /** Emit OrderProcessed(ProcessLost) and `subagentDiedEvent`. */
  protected def onSubagentDied(orderProblem: ProcessLostProblem, subagentDiedEvent: SubagentDied)
  : IO[Unit] =
    stopDispatcherAndEmitProcessLostEvents(orderProblem, Some(subagentDiedEvent))

  /** Emit OrderProcessed(ProcessLost) and optionally a `subagentDiedEvent` event. */
  def stopDispatcherAndEmitProcessLostEvents(
    orderProblem: ProcessLostProblem,
    subagentDiedEvent: Option[SubagentDied])
  : IO[Unit] =
    // Subagent died and lost its state
    // Emit OrderProcessed(Disrupted(ProcessLost)) for each processing order.
    // Then optionally subagentDiedEvent
    val processing = Order.Processing(subagentId)
    logger.debugIO(orderToDeferred
      .removeAll
      .flatMap: oToD =>
        val orderIds = oToD.keys
        IO
          .whenA(subagentDiedEvent.isDefined):
            if isLocal then dispatcher.shutdown else dispatcher.stopAndFailCommands
          .*>(attachedItemKeys.update(_ => IO.pure(Map.empty)))
          .*>(journal
            .persist(state => Right(orderIds.view
              .flatMap:
                state.idToOrder.get
              .filter: order =>
                order.state == processing // Just to be sure, condition should always be true
              // Filter Orders which have been sent to Subagent ???
              .map: order =>
                order.id <-: state.orderProcessLostIfRestartable(order, orderProblem)
              .concat(subagentDiedEvent.map(subagentId <-: _))
              .toVector)))
          .map(_.orThrow)
          .flatMap: (stampedEvents, _) =>
            stampedEvents.map(_.value).traverseCombine:
              case KeyedEvent(orderId: OrderId, orderProcessed: OrderProcessed) =>
                oToD(orderId).complete(orderProcessed).void
              case _ => IO.unit)

  // May run concurrently with onSubagentDied !!!
  // Be sure that only on OrderProcessed event is emitted!
  private def onStartOrderProcessFailed(
    startOrderProcess: StartOrderProcess,
    orderProcessed: OrderProcessed)
  : IO[Checked[Unit]] =
    journal
      .persist: state =>
        Right:
          state.idToOrder.get(startOrderProcess.orderId)
            .filter(o => // Just to be sure, condition should always be true:
              o.state == Order.Processing(subagentId) &&
                o.workflowPosition == startOrderProcess.order.workflowPosition)
            .map(_.id <-: orderProcessed)
            .toList
      .rightAs(())

  /** Continue a recovered processing Order. */
  def recoverOrderProcessing(order: Order[Order.Processing]) =
    logger.traceIO("recoverOrderProcessing", order.id):
      if isLocal then
        emitOrderProcessLostAfterRestart(order)
          .map(_.orThrow)
          .start
          .map(Right(_))
      else
        requireNotStopping.flatMapT(_ =>
          startOrderProcessing(order))

  def startOrderProcessing(order: Order[Order.Processing]): IO[Checked[FiberIO[OrderProcessed]]] =
    logger.traceIO("startOrderProcessing", order.id):
      requireNotStopping.flatMapT: _ =>
        startProcessingOrder2(order)

  private def startProcessingOrder2(order: Order[Order.Processing])
  : IO[Checked[FiberIO[OrderProcessed]]] =
    orderToDeferred
      .insert(order.id, Deferred.unsafe)
      // OrderProcessed event will fulfill and remove the Deferred
      .flatMap:
        case Left(problem) => IO.left(problem)
        case Right(deferred) =>
          orderToExecuteDefaultArguments(order)
            .map(_.map(StartOrderProcess(order, _)))
            .flatMapT(dispatcher.executeCommand)
            .materializeIntoChecked
            .flatMap:
              case Left(problem) =>
                // StartOrderProcess failed
                orderToDeferred
                  .remove(order.id)
                  .flatMap:
                    case None =>
                      // Deferred has been removed
                      if problem != CommandDispatcher.StoppedProblem then
                        // onSubagentDied has stopped all queued StartOrderProcess commands
                        logger.warn(s"${order.id} got OrderProcessed, so we ignore $problem")
                      IO.right(())

                    case Some(deferred_) =>
                      assert(deferred_ eq deferred)

                      val orderProcessed =
                        problem match
                          case SubagentIsShuttingDownProblem =>
                            OrderProcessed.processLost(SubagentShutDownBeforeProcessStartProblem)
                          case _ =>
                            OrderProcessed(OrderOutcome.Disrupted(problem))

                      journal
                        .persistKeyedEvent(order.id <-: orderProcessed)
                        .orThrow
                        .*>(deferred.complete(orderProcessed))
                        .as(orderProcessed)

              case Right(()) =>
                // Command succeeded, wait for Deferred
                IO.right(())
            .*>(deferred.get)
            .start
            .map(Right(_))

  //private def killAll(signal: ProcessSignal): IO[Unit] =
  //  IO.defer {
  //    val cmds = orderToDeferred.toMap.keys.map(KillProcess(_, signal))
  //    dispatcher
  //      .executeCommands(cmds)
  //      .map(cmds.zip(_).map {
  //        case (cmd, Left(problem)) => logger.error(s"$cmd => $problem")
  //        case _ =>
  //      })
  //  }

  def killProcess(orderId: OrderId, signal: ProcessSignal): IO[Unit] =
    dispatcher.executeCommand(KillProcess(orderId, signal))
      .map:
        // TODO Stop postQueuedCommand loop for this OrderId
        // Subagent may have been restarted
        case Left(problem) => logger.error(s"killProcess $orderId => $problem")
        case Right(_) =>

  private def postCommandUntilSucceeded(command: SubagentCommand): IO[command.Response] =
    logger.traceIO("postCommandUntilSucceeded", command.toShortString):
      DelayConf.default.runIO: delayer =>
        cancelAndFailWhenStopping:
          api.login(onlyIfNotLoggedIn = true)
            .*>(api.executeSubagentCommand(Numbered(0, command)).orThrow)
            .map(_.asInstanceOf[command.Response])
            .onErrorRestartLoop(()): (throwable, _, retry) =>
              logger.warn:
                s"${command.getClass.simpleScalaName} command failed: ${throwable.toStringWithCauses}"
              emitSubagentCouplingFailed(Some(Problem.reverseThrowable(throwable)))
                *> delayer.sleep
                *> retry(())

  private def cancelAndFailWhenStopping[A](io: IO[A]): IO[A] =
    IO
      .race(untilStopRequested, io)
      .flatMap:
        case Left(()) =>
          logger.debug("◼️  cancelAndFailWhenStopping!")
          IO.raiseError(Problem(s"$toString is being stopped").throwable)

        case Right(a) =>
          IO.pure(a)

  protected def emitSubagentCouplingFailed(maybeProblem: Option[Problem]): IO[Unit] =
    logger.debugIO("emitSubagentCouplingFailed", maybeProblem):
      // TODO Suppress duplicate errors
      journal.persist:
        _.idToSubagentItemState.checked(subagentId)
          .map: subagentItemState =>
            val problem = maybeProblem
              .orElse(subagentItemState.problem)
              .getOrElse(Problem.pure("decoupled"))
            (!subagentItemState.problem.contains(problem))
              .thenList(subagentId <-: SubagentCouplingFailed(problem))
      .map(_.orThrow)
      .void
      .onError: t =>
        // Error isn't logged until stopEventListener has been called
        IO(logger.error("emitSubagentCouplingFailed => " + t.toStringWithCauses))

  protected def detachProcessedOrder(orderId: OrderId): IO[Unit] =
    enqueueCommandAndForget:
      SubagentCommand.DetachProcessedOrder(orderId)

  protected def releaseEvents(eventId: EventId): IO[Unit] =
    enqueueCommandAndForget:
      SubagentCommand.ReleaseEvents(eventId)

  private def enqueueCommandAndForget(cmd: SubagentCommand.Queueable): IO[Unit] =
    dispatcher
      .enqueueCommand(cmd)
      .map(_
        .map(_.orThrow/*???*/)
        .handleError(t =>
          logger.error(s"${cmd.toShortString} => ${t.toStringWithCauses}",
            t.nullIfNoStackTrace))
        .startAndForget/* Don't await response */)

  private def postQueuedCommand(
    numberedCommand: Numbered[SubagentCommand.Queueable],
    subagentRunId: SubagentRunId,
    processingAllowed: Switch.ReadOnly)
  : IO[Checked[Unit]] =
    IO.defer:
      //val heartbeatTimeoutElapsed = scheduler.now + SubagentEventListener.heartbeatTiming.longHeartbeatTimeout
      val retryAfterError = new RetryAfterError(processingAllowed.whenOff)
      val command = numberedCommand.value
      lazy val commandString = numberedCommand.copy(value = command.toShortString).toString
      logger.traceIO("postQueuedCommand", commandString):
        ().tailRecM: _ =>
          // TODO Use processingAllowed when Order is being canceled
          currentSubagentItemState
            .flatMapT: subagentItemState =>
              processingAllowed.isOff.flatMap: isStopped =>
                // Double-check subagentRunId to be sure.
                if isStopped || !subagentItemState.subagentRunId.contains(subagentRunId) then
                  logger.debug(s"postQueuedCommand($commandString) stopped")
                  IO.right(())
                else
                  postQueuedCommand2(numberedCommand)
            .materialize
            .flatMap:
              case Failure(throwable) =>
                retryAfterError(Problem.reverseThrowable(throwable))

              //case Success(checked @ Left(_: SubagentDriverStoppedProblem)) =>
              //  logger.debug(s"postQueuedCommand($commandString) stopped")
              //  IO.right(checked)

              case Success(checked @ Left(problem)) =>
                if HttpClient.isTemporaryUnreachableStatus(problem.httpStatusCode) then
                  // We don't check the error type,
                  // because it seems impossible to properly detect recoverable errors.
                  // Instead, we repeat the command until we are being stopped due
                  // to heartbeat timeout detection or other reason.
                  retryAfterError(problem)
                else
                  IO.right(checked)

              case Success(checked @ Right(_)) =>
                IO.right(checked)
        .flatMap:
          case Left(SubagentNotDedicatedProblem) =>
            logger.debug(s"⚠️ $commandString => SubagentNotDedicatedProblem => ProcessLost")
            command match
              case command: StartOrderProcess =>
                orderToDeferred
                  .remove(command.orderId)
                  // Delay to let onSubagentDied go ahead and let it handle all orders at once
                  //?.delayBy(100.ms)
                  .flatMap:
                    case None =>
                      // OrderProcessed has already been emitted by onSubagentDied)
                      // The command does not fail:
                      IO.right(())

                    case Some(deferred) =>
                      // The SubagentEventListener should do the same for all lost processes
                      // at once, but just in case the SubagentEventListener does run, we issue
                      // an OrderProcess event here.
                      val orderProcessed = OrderProcessed.processLost(SubagentNotDedicatedProblem)
                      onStartOrderProcessFailed(command, orderProcessed)
                        .flatTapT: _ =>
                          deferred.complete(orderProcessed)
                            .as(Checked.unit)
              case _ =>
                IO.right(SubagentNotDedicatedProblem)

          case Left(problem) =>
            processingAllowed.isOff.flatMap(if _ then
              logger.debug(s"⚠️ postQueuedCommand($commandString) error after stop ignored: $problem")
              IO.right(())
            else
              IO.left(problem))

          case Right(()) => IO.right(())
  end postQueuedCommand

  private final class RetryAfterError(whenStopped: IO[Unit]):
    private val startedAt = now
    private var lastWarning: Option[String] = None
    private var warningCount = 0

    def apply(problem: Problem): IO[Either[Unit, Right[Nothing, Unit]]] =
      IO
        .race(
          whenStopped.as(Right(Right(()))),
          retry(problem))
        .map(_.merge)

    private def retry(problem: Problem): IO[Left[Unit, Nothing]] =
      IO.defer:
        warningCount += 1
        val warning = problem.throwableOption match
          case None => problem.toString
          case Some(t) => t.toStringWithCauses
        if lastWarning.contains(warning) then
          logger.debug(s"⚠️ Retry warning #$warningCount (${startedAt.elapsed.pretty}): $warning")
        else
          lastWarning = Some(warning)
          logger.warn(s"Retry warning #$warningCount: $warning")
        IO.sleep(tryPostErrorDelay) // Retry
          .as(Left(()))

  private def postQueuedCommand2(numberedCommand: Numbered[SubagentCommand.Queueable])
  : IO[Checked[Unit]] =
    IO.defer:
      val command = numberedCommand.value
      dependentSignedItems(command)
        .map(_.orThrow)
        .flatMap: signedItems =>
          val cmd = signedItems match
            case Nil => numberedCommand
            case signedSeq =>
              val correlId = CorrelId.current
              numberedCommand.copy(
                value = SubagentCommand.Batch(signedSeq
                  .map(AttachSignedItem(_): SubagentCommand)
                  .appended(command)
                  .map(CorrelIdWrapped(correlId, _))))
          api
            .executeSubagentCommand(cmd)
            .flatMapT(_ => attachedItemKeys
              .update(o => IO.pure:
                o ++ signedItems.view.map(_.value.keyAndRevision))
              .as(Checked.unit))

  private def dependentSignedItems(command: SubagentCommand)
  : IO[Checked[Seq[Signed[SignableItem]]]] =
    command match
      case startOrderProcess: StartOrderProcess =>
        val alreadyAttached = attachedItemKeys.get
        signableItemsForOrderProcessing(startOrderProcess.order.workflowPosition)
          .map(_.map(_.filterNot: signed =>
            alreadyAttached.get(signed.value.key) contains signed.value.itemRevision))
      case _ =>
        IO.right(Nil)

  private def currentSubagentItemState: IO[Checked[SubagentItemState]] =
    journal.aggregate.map(_.idToSubagentItemState.checked(subagentId))

  override def toString =
    s"RemoteSubagentDriver(${subagentItem.pathRev})"


object RemoteSubagentDriver:
  private val reconnectErrorDelay = 5.s/*TODO*/
  private val tryPostErrorDelay = 5.s/*TODO*/

  private[director] def resource[S <: SubagentDirectorState[S]](
    subagentItem: SubagentItem,
    api: HttpSubagentApi,
    journal: Journal[S],
    controllerId: ControllerId,
    conf: RemoteSubagentDriver.Conf,
    recouplingStreamReaderConf: RecouplingStreamReaderConf)
  : ResourceIO[RemoteSubagentDriver] =
    Service.resource(IO:
      new RemoteSubagentDriver(
        subagentItem, api, journal, controllerId, conf, recouplingStreamReaderConf))

  final case class Conf(
    eventBufferDelay: FiniteDuration,
    eventBufferSize: Int,
    commitDelay: FiniteDuration,
    heartbeatTiming: HeartbeatTiming,
    subagentResetTimeout: FiniteDuration,
    config: Config)
  object Conf:
    def fromConfig(config: Config, commitDelay: FiniteDuration) =
      new Conf(
        eventBufferDelay = config.finiteDuration("js7.subagent-driver.event-buffer-delay").orThrow,
        eventBufferSize = config.getInt("js7.subagent-driver.event-buffer-size"),
        commitDelay = commitDelay,
        HeartbeatTiming(
          heartbeat =
            config.finiteDuration("js7.subagent-driver.heartbeat").orThrow
              .min(config.finiteDuration("js7.web.client.keep-alive").orThrow),
          heartbeatTimeout = config.finiteDuration("js7.subagent-driver.heartbeat-timeout").orThrow),
        subagentResetTimeout = config.finiteDuration("js7.subagent-driver.reset-timeout").orThrow,
        config)

  //final case class SubagentDriverStoppedProblem(subagentId: SubagentId) extends Problem.Coded {
  //  def arguments = Map("subagentId" -> subagentId.string)
  //}
