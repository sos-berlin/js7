package js7.subagent.director

import cats.effect.Resource
import cats.effect.concurrent.Deferred
import cats.syntax.all.*
import js7.base.io.process.ProcessSignal
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.stream.Numbered
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.system.PlatformInfos.currentPlatformInfo
import js7.core.command.CommandMeta
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, KeyedEvent, Stamped}
import js7.data.job.JobKey
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId, Outcome}
import js7.data.subagent.Problems.{SubagentIsShuttingDownProblem, SubagentShutDownBeforeProcessStartProblem}
import js7.data.subagent.SubagentCommand.{AttachSignedItem, DedicateSubagent}
import js7.data.subagent.SubagentItemStateEvent.{SubagentCouplingFailed, SubagentDedicated, SubagentEventsObserved, SubagentRestarted}
import js7.data.subagent.{SubagentCommand, SubagentDirectorState, SubagentEvent, SubagentItem, SubagentRunId}
import js7.journal.state.Journal
import js7.subagent.configuration.SubagentConf
import js7.subagent.{LocalSubagentApi, Subagent}
import monix.eval.{Fiber, Task}

private final class LocalSubagentDriver private(
  val subagentItem: SubagentItem,
  subagent: Subagent,
  protected val journal: Journal[? <: SubagentDirectorState[?]],
  controllerId: ControllerId,
  protected val subagentConf: SubagentConf)
extends SubagentDriver, Service.StoppableByRequest:
  
  private val logger = Logger.withPrefix[this.type](subagentItem.pathRev.toString)
  // isDedicated when this Director gets activated after fail-over.
  private val wasRemote = subagent.isDedicated
  protected val api = new LocalSubagentApi(subagent)
  @volatile private var shuttingDown = false
  @volatile private var _testFailover = false

  subagent.supressJournalLogging(true) // Events are logged by the Director's Journal

  protected def isHeartbeating = true

  protected def isShuttingDown = shuttingDown

  protected def start =
    dedicate.map(_.orThrow) *>
      startService(
        untilStopRequested)

  private def dedicate: Task[Checked[Unit]] =
    logger.debugTask(Task.defer(
      if wasRemote then
        Task.right(())
      else
        subagent
          .executeDedicateSubagent(
            DedicateSubagent(subagentId, subagentItem.agentPath, controllerId))
          .flatMapT(response =>
            persistDedicated(response.subagentRunId))))

  private def persistDedicated(subagentRunId: SubagentRunId): Task[Checked[Unit]] =
    journal
      .persist(state => Right(
        state.idToSubagentItemState.get(subagentId).exists(_.subagentRunId.nonEmpty)
          .thenList(subagentId <-: SubagentRestarted)
          .appended(subagentId <-: SubagentDedicated(subagentRunId, Some(currentPlatformInfo())))))
      .rightAs(())

  def startObserving: Task[Unit] =
    observe.startAndForget

  // TODO Similar to SubagentEventListener
  private def observe: Task[Unit] =
    logger.debugTask(
      journal.state
        .map(_.idToSubagentItemState(subagentId).eventId)
        .flatMap(observeAfter))

  // TODO Similar to SubagentEventListener
  private def observeAfter(eventId: EventId): Task[Unit] =
    subagent.journal.eventWatch
      .observe(EventRequest.singleClass[Event](after = eventId, timeout = None))
      .mapEval(handleEvent)
      .mapEval:
        case (maybeStampedEvent, followUp) =>
          maybeStampedEvent
            .fold(Task.unit)(stampedEvent => journal
              // TODO Save Stamped timestamp
              .persistKeyedEvent(stampedEvent.value)
              .map(_.orThrow._1 /*???*/)
              // After an OrderProcessed event an DetachProcessedOrder must be sent,
              // to terminate StartOrderProcess command idempotency detection and
              // allow a new StartOrderProcess command for a next process.
              .tapEval {
                case Stamped(_, _, KeyedEvent(orderId: OrderId, _: OrderProcessed)) =>
                  subagent.commandExecutor
                    .executeCommand(
                      Numbered(0, SubagentCommand.DetachProcessedOrder(orderId)),
                      CommandMeta.System)
                    .orThrow
                case _ => Task.unit
              }
              // TODO Emit SubagentEventsObserved for a chunk of events (use fs2)
              .*>(journal.persistKeyedEvent(
                subagentId <-: SubagentEventsObserved(stampedEvent.eventId)))
              .*>(releaseEvents(stampedEvent.eventId)))
            .*>(followUp)
      .takeUntilEval(untilStopRequested)
      .completedL
      .onErrorHandle(t => logger.error(s"observeEvents => ${t.toStringWithCauses}"))

  /** Returns optionally the event and a follow-up task. */
  private def handleEvent(stamped: Stamped[AnyKeyedEvent])
  : Task[(Option[Stamped[AnyKeyedEvent]], Task[Unit])] =
    stamped.value match
      case keyedEvent @ KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        event match
          case _: OrderStdWritten =>
            // TODO Save Timestamp
            // FIXME Persist asynchronous!
            Task.pure(Some(stamped) -> Task.unit)

          case orderProcessed: OrderProcessed =>
            // TODO Save Timestamp
            onOrderProcessed(orderId, orderProcessed).map:
              case None => None -> Task.unit // OrderProcessed already handled
              case Some(followUp) =>
                // The followUp Task notifies OrderActor about OrderProcessed by calling `onEvents`
                Some(stamped) -> followUp

          case _ =>
            logger.error(s"Unexpected event: $keyedEvent")
            Task.pure(None -> Task.unit)

      case KeyedEvent(_: NoKey, SubagentEvent.SubagentShutdown) =>
        // Ignore this, should not happen
        Task.pure(None -> Task.unit)

      case KeyedEvent(_: NoKey, event: SubagentEvent.SubagentItemAttached) =>
        logger.debug(event.toShortString)
        Task.pure(None -> Task.unit)

      case keyedEvent =>
        logger.error(s"Unexpected event: $keyedEvent")
        Task.pure(None -> Task.unit)

  private def onOrderProcessed(orderId: OrderId, orderProcessed: OrderProcessed)
  : Task[Option[Task[Unit]]] =
    orderToDeferred.remove(orderId).map:
      case None =>
        logger.error(s"Unknown Order for event: ${orderId <-: orderProcessed}")
        None

      case Some(processing) =>
        Some(processing.complete(orderProcessed))

  def stopJobs(jobKeys: Iterable[JobKey], signal: ProcessSignal) =
    Task(subagent.checkedDedicatedSubagent.toOption)
      .flatMap(_
        .fold(Task.unit)(_
          .stopJobs(jobKeys, signal)))

  def terminate: Task[Unit] =
    logger.traceTask(
      stop)

  def tryShutdown: Task[Unit] =
    Task.raiseError(new RuntimeException("The local Subagent cannot be shut down"))

  /** Continue a recovered processing Order. */
  def recoverOrderProcessing(order: Order[Order.Processing]) =
    logger.traceTask("recoverOrderProcessing", order.id)(
      if wasRemote then
        requireNotStopping.flatMapT(_ =>
          startOrderProcessing(order)) // TODO startOrderProcessing again ?
      else
        emitOrderProcessLost(order)
          .map(_.orThrow)
          .start
          .map(Right(_)))

  def startOrderProcessing(order: Order[Order.Processing]): Task[Checked[Fiber[OrderProcessed]]] =
    logger.traceTask("startOrderProcessing", order.id)(
      requireNotStopping
        .flatMapT(_ => attachItemsForOrder(order))
        .flatMapT(_ => startProcessingOrder2(order)))

  private def attachItemsForOrder(order: Order[Order.Processing]): Task[Checked[Unit]] =
    signableItemsForOrderProcessing(order.workflowPosition)
      //.map(_.map(_.filterNot(signed =>
      //  alreadyAttached.get(signed.value.key) contains signed.value.itemRevision)))
      .flatMapT(_
        .traverse(signedItem =>
          subagent.commandExecutor.executeCommand(
            Numbered(0, AttachSignedItem(signedItem)),
            CommandMeta.System))
        .map(_.map(_.rightAs(())).combineAll))

  private def startProcessingOrder2(order: Order[Order.Processing])
  : Task[Checked[Fiber[OrderProcessed]]] =
    orderToDeferred
      .insert(order.id, Deferred.unsafe)
      .flatMap:
        case Left(problem) => Task.left(problem)
        case Right(deferred) =>
          orderToExecuteDefaultArguments(order)
            .flatMapT(subagent.startOrderProcess(order, _))
            .materializeIntoChecked
            .flatMap:
              case Left(problem) =>
                orderToDeferred
                  .remove(order.id)
                  .flatMap:
                    case None =>
                      // Deferred has been removed
                      if problem != CommandDispatcher.StoppedProblem then
                        // onSubagentDied has stopped all queued StartOrderProcess commands
                        logger.warn(s"${order.id} got OrderProcessed, so we ignore $problem")
                      Task.right(())

                    case Some(deferred) =>
                      assert(deferred eq deferred)

                      val orderProcessed = OrderProcessed(
                        problem match {
                          case SubagentIsShuttingDownProblem =>
                            Outcome.processLost(SubagentShutDownBeforeProcessStartProblem)
                          case _ => Outcome.Disrupted(problem)
                        })

                      if _testFailover && orderProcessed.outcome.isInstanceOf[Outcome.Killed] then
                        Task(logger.warn(
                          s"Suppressed due to failover by command: ${order.id} <-: $orderProcessed")
                        ).start
                      else
                        journal
                          .persistKeyedEvent(order.id <-: orderProcessed)
                          .orThrow.start

              case Right(_: Fiber[OrderProcessed]) =>
                Task.unit
            // Now wait for OrderProcessed event fulfilling Deferred
            .*>(deferred.get)
            .start
            .map(Right(_))

  def killProcess(orderId: OrderId, signal: ProcessSignal): Task[Unit] =
    subagent.killProcess(orderId, signal)
      // TODO Stop postQueuedCommand loop for this OrderId
      .map(_.onProblemHandle(problem => logger.error(s"killProcess $orderId => $problem")))

  protected def emitSubagentCouplingFailed(maybeProblem: Option[Problem]): Task[Unit] =
    logger.debugTask("emitSubagentCouplingFailed", maybeProblem)(
      // TODO Suppress duplicate errors
      journal
        .lock(subagentId)(
          journal.persist(_
            .idToSubagentItemState.checked(subagentId)
            .map { subagentItemState =>
              val problem = maybeProblem
                .orElse(subagentItemState.problem)
                .getOrElse(Problem.pure("decoupled"))
              (!subagentItemState.problem.contains(problem))
                .thenList(subagentId <-: SubagentCouplingFailed(problem))
            }))
        .map(_.orThrow)
        .void
        .onErrorHandleWith(t => Task.defer {
          // Error isn't logged until stopEventListener is called
          logger.error("emitSubagentCouplingFailed => " + t.toStringWithCauses)
          Task.raiseError(t)
        }))

  protected def detachProcessedOrder(orderId: OrderId): Task[Unit] =
    enqueueCommandAndForget(
      SubagentCommand.DetachProcessedOrder(orderId))

  protected def releaseEvents(eventId: EventId): Task[Unit] =
    enqueueCommandAndForget(
      SubagentCommand.ReleaseEvents(eventId))

  private def enqueueCommandAndForget(cmd: SubagentCommand.Queueable): Task[Unit] =
    subagent.commandExecutor.executeCommand(Numbered(0, cmd), CommandMeta.System)
      .start
      .map(_
        .join
        .map(_.orThrow)
        .void
        .onErrorHandle(t =>
          logger.error(s"${cmd.toShortString} => ${t.toStringWithCauses}",
            t.nullIfNoStackTrace))
        .startAndForget/* Don't await response */)

  def testFailover(): Unit =
    _testFailover = true

  override def toString =
    s"LocalSubagentDriver(${subagentItem.pathRev})"


object LocalSubagentDriver:
  private[director] def resource[S <: SubagentDirectorState[S]](
    subagentItem: SubagentItem,
    subagent: Subagent,
    journal: Journal[S],
    controllerId: ControllerId,
    subagentConf: SubagentConf)
  : Resource[Task, LocalSubagentDriver] =
    Service.resource(Task {
      new LocalSubagentDriver(subagentItem, subagent, journal, controllerId, subagentConf)
    })
