package js7.controller.agent

import akka.actor.{DeadLetterSuppression, Props}
import cats.data.EitherT
import com.typesafe.config.ConfigUtil
import js7.agent.client.AgentClient
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{CoupleController, DedicateAgentDirector}
import js7.agent.data.event.AgentEvent
import js7.base.auth.UserAndPassword
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.crypt.Signed
import js7.base.generic.{Completed, SecretString}
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.promiseTask
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked.*
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.problem.{Checked, Problem}
import js7.base.thread.Futures.promiseFuture
import js7.base.thread.Futures.syntax.RichFuture
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SetOnce
import js7.base.web.Uri
import js7.common.akkautils.ReceiveLoggingActor
import js7.common.http.RecouplingStreamReader
import js7.controller.agent.AgentDriver.*
import js7.controller.agent.CommandQueue.QueuedInputResponse
import js7.controller.configuration.ControllerConfiguration
import js7.data.agent.AgentRefStateEvent.{AgentCoupled, AgentCouplingFailed, AgentDedicated, AgentReset}
import js7.data.agent.Problems.AgentNotDedicatedProblem
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRunId}
import js7.data.controller.ControllerState
import js7.data.delegate.DelegateCouplingState.{Coupled, Resetting}
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, KeyedEvent, Stamped}
import js7.data.item.ItemAttachedState.{Attachable, Attached}
import js7.data.item.{InventoryItemEvent, InventoryItemKey, SignableItem, UnsignedItem}
import js7.data.order.OrderEvent.{OrderAttachedToAgent, OrderDetached}
import js7.data.order.{Order, OrderEvent, OrderId, OrderMark}
import js7.data.orderwatch.OrderWatchEvent
import js7.data.subagent.{SubagentId, SubagentItemStateEvent}
import js7.journal.state.StatePersistence
import monix.eval.Task
import monix.execution.atomic.AtomicInt
import monix.execution.{Cancelable, CancelableFuture, Scheduler}
import monix.reactive.Observable
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now
import scala.util.chaining.scalaUtilChainingOps
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success, Try}

/**
  * Couples to an Agent, sends orders, and fetches events.
  *
  * @author Joacim Zschimmer
  */
final class AgentDriver private(
  initialAgentRef: AgentRef,
  initialAgentRunId: Option[AgentRunId],
  initialEventId: EventId,
  persistence: StatePersistence[ControllerState],
  conf: AgentDriverConfiguration,
  controllerConfiguration: ControllerConfiguration)
  (implicit protected val scheduler: Scheduler)
extends ReceiveLoggingActor.WithStash
{
  import controllerConfiguration.controllerId

  private val agentPath = initialAgentRef.path
  private var agentRef = initialAgentRef
  private val logger = Logger.withPrefix[this.type](agentPath.string)
  private val agentUserAndPassword: Option[UserAndPassword] =
    controllerConfiguration.config.optionAs[SecretString]("js7.auth.agents." + ConfigUtil.joinPath(agentPath.string))
      .map(password => UserAndPassword(controllerConfiguration.controllerId.toUserId, password))

  private val agentRunIdOnce = SetOnce.fromOption(initialAgentRunId)
  private var client = newAgentClient(persistence.currentState.agentToUri(agentPath)
    .getOrElse(Uri(s"unknown-uri://$agentPath"/*should not happen ???*/)))
  /** Only filled when coupled */
  private var lastFetchedEventId = initialEventId
  private var lastCommittedEventId = initialEventId
  @volatile
  private var lastCouplingFailed: Option[AgentCouplingFailed] = None
  private var currentFetchedFuture: Option[CancelableFuture[Completed]] = None
  private var releaseEventsCancelable: Option[Cancelable] = None
  private var delayNextReleaseEvents = false
  private var delayCommandExecutionAfterErrorUntil = now
  private var isTerminating = false
  private var changingUri: Option[Uri] = None
  private val sessionNumber = AtomicInt(0)
  private val eventFetcherTerminated = Promise[Completed]()
  private var noJournal = false
  private var isReset = false

  private val eventFetcher = new RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], AgentClient](
    _.eventId, conf.recouplingStreamReader)
  {
    private var attachedOrderIds: Set[OrderId] = null

    override protected def couple(eventId: EventId) =
      Task(persistence.currentState.keyTo(AgentRefState).checked(agentPath))
        .flatMapT(agentRefState =>
          ((agentRefState.couplingState, agentRefState.agentRunId) match {
            case (Resetting(false), None) =>
              Task.pure(Left(Problem.pure("Resetting, but no AgentRunId?")))  // Invalid state

            case (Resetting(force), maybeAgentRunId) if force || maybeAgentRunId.isDefined =>
              client.commandExecute(AgentCommand.Reset(maybeAgentRunId))
                .map {
                  case Left(AgentNotDedicatedProblem) => Checked.unit  // Already reset
                  case o => o
                }
                .flatMapT(_ =>
                  persistence.persistKeyedEvent(agentPath <-: AgentReset))

            case _ =>
              Task.pure(Checked.unit)
          })
        .flatMapT(_ => dedicateAgentIfNeeded)
        .flatMapT { case (agentRunId, agentEventId) =>
          client.commandExecute(CoupleController(agentPath, agentRunId, eventId = agentEventId))
            .flatMapT { case CoupleController.Response(orderIds) =>
              logger.trace(s"CoupleController returned attached OrderIds={${orderIds.toSeq.sorted.mkString(" ")}}")
              attachedOrderIds = orderIds
              persistence
                .lock(agentPath)(
                  persistence.persist(controllerState =>
                    for (a <- controllerState.keyTo(AgentRefState).checked(agentPath)) yield
                      (a.couplingState != Coupled || a.problem.nonEmpty)
                        .thenList(agentPath <-: AgentCoupled)))
                .rightAs(agentEventId)
            }
        })

    protected def getObservable(api: AgentClient, after: EventId) =
      Task { logger.debug(s"getObservable(after=$after)") } *>
        api.eventObservable(EventRequest[Event](EventClasses, after = after,
          timeout = Some(requestTimeout)))

    override protected def onCouplingFailed(api: AgentClient, problem: Problem) =
      Task.defer {
        val agentCouplingFailed = AgentCouplingFailed(problem)
        if (lastCouplingFailed contains agentCouplingFailed) {
          logger.debug(s"Coupling failed: $problem")
          Task.pure(true)
        } else {
          lastCouplingFailed = Some(agentCouplingFailed)
          logger.warn(s"Coupling failed: $problem")
          for (t <- problem.throwableOption if t.getStackTrace.nonEmpty) logger.debug(s"Coupling failed: $problem", t)
          if (noJournal)
            Task.pure(true)
          else
            persistence.persistKeyedEvent(agentPath <-: agentCouplingFailed)
              .map(_.map(_ => true))  // recouple and continue after onCouplingFailed
              .map(_.orThrow)
        }
      }

    override protected def onCoupled(api: AgentClient, after: EventId) =
      promiseTask[Completed] { promise =>
        logger.info(s"Coupled with $api after=${EventId.toString(after)}")
        sessionNumber += 1
        assertThat(attachedOrderIds != null)
        self ! Internal.OnCoupled(promise, attachedOrderIds)
        attachedOrderIds = null
      }

    override protected def onDecoupled = Task {
      logger.debug("onDecoupled")
      sessionNumber += 1
      self ! Internal.OnDecoupled
      Completed
    }

    protected def stopRequested = false
  }

  private val commandQueue = new CommandQueue(logger, batchSize = conf.commandBatchSize) {
    protected def commandParallelism = conf.commandParallelism

    protected def executeCommand(command: AgentCommand.Batch) = {
      val expectedSessionNumber: Int = sessionNumber.get()
      for {
        _ <- Task.defer {
          val delay = delayCommandExecutionAfterErrorUntil.timeLeft
          if (delay.isPositive) logger.debug(s"${AgentDriver.this.toString}: Delay command after error until ${Timestamp.now + delay}")
          Task.sleep(delay)
        }
        checkedApi <- eventFetcher.coupledApi.map(_.toRight(DecoupledProblem))
        response <-
          (for {
            // Fail on recoupling, later read restarted Agent's attached OrderIds before issuing again AttachOrder
            api <- EitherT(Task.pure(checkedApi))
            response <- EitherT(
              if (sessionNumber.get() != expectedSessionNumber)
                Task.pure(Left(DecoupledProblem))
              else
                // TODO Still a small possibility for race-condition? May log a AgentDuplicateOrder
                api.commandExecute(command))
          } yield response).value
      } yield response.map(_.asInstanceOf[AgentCommand.Batch.Response]/*???*/)
    }

    protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) =
      self ! Internal.BatchSucceeded(queuedInputResponses)

    protected def asyncOnBatchFailed(inputs: Vector[Queueable], problem: Problem) =
      if (problem == DecoupledProblem/*avoid loop*/ || isTerminating) {
        self ! Internal.BatchFailed(inputs, problem)
      } else
        eventFetcher.invalidateCoupledApi
          .*>(Task { currentFetchedFuture.foreach(_.cancel()) })
          .*>(cancelObservationAndAwaitTermination)
          .*>(eventFetcher.decouple)
          .*>(Task {
            self ! Internal.BatchFailed(inputs, problem)
          })
          .runToFuture
          .onFailure { case t =>
            logger.error("asyncOnBatchFailed: " + t.toStringWithCauses, t.nullIfNoStackTrace)
          }
  }

  override def preStart() = {
    super.preStart()
    logger.debug(s"preStart $agentRunIdOnce")
  }

  override def postStop() = {
    logger.debug("postStop")
    eventFetcher.markAsStopped()
    eventFetcher.terminateAndLogout.runAsyncAndForget
    currentFetchedFuture.foreach(_.cancel())
    super.postStop()
  }

  protected def key = agentPath  // Only one version is active at any time

  private def newAgentClient(uri: Uri): AgentClient =
    AgentClient(uri, agentUserAndPassword, label = agentPath.toString,
      controllerConfiguration.httpsConfig)(context.system)

  def receive = {
    case input: Input with Queueable if !isTerminating =>
      val ok = commandQueue.enqueue(input)
      if (ok) scheduler.scheduleOnce(conf.commandBatchDelay) {
        self ! Internal.CommandQueueReady  // (Even with commandBatchDelay == 0) delay maySend() such that Queueable pending in actor's mailbox can be queued
      }

    case Input.ChangeUri(agentRef, uri) =>
      this.agentRef = agentRef
      if (uri != client.baseUri || isReset) {
        logger.debug(s"ChangeUri $uri")
        // TODO Changing URI in quick succession is not properly solved
        isReset = false
        for (u <- changingUri) logger.warn(s"Already changing URI to $u ?")
        changingUri = Some(uri)
        cancelObservationAndAwaitTermination
          .*>(eventFetcher.decouple)
          .runToFuture.onFailure { case t =>
            logger.error("ChangeUri: " + t.toStringWithCauses, t.nullIfNoStackTrace)
          }
      }

    case Internal.UriChanged =>
      for (uri <- changingUri) {
        client.close()
        logger.debug(s"new AgentClient($uri)")
        client = newAgentClient(uri)
        self ! Internal.FetchEvents
        changingUri = None
      }

    case Input.Terminate(noJournal, reset) =>
      this.noJournal = noJournal
      // Wait until all pending Agent commands are responded, and do not accept further commands
      if (!isTerminating) {
        logger.debug(s"Terminate" + (noJournal ?? " noJournal") + (reset ?? " reset"))
        isTerminating = true
        commandQueue.terminate()
        currentFetchedFuture.foreach(_.cancel())
        eventFetcherTerminated.completeWith(
          eventFetcher.terminateAndLogout.runToFuture)  // Rejects current commands waiting for coupling
        agentRunIdOnce.toOption match {
          case Some(agentRunId) if reset =>
            // Required only for ItemDeleted, redundant for ResetAgent
            // Because of terminateAndLogout, we must login agein to issue the Reset command
            Task
              .fromFuture(eventFetcherTerminated.future)
              .onErrorHandle(_ => ())
              .*>(client.login(onlyIfNotLoggedIn = true))  // Login again
              .*>(client
                .commandExecute(AgentCommand.Reset(Some(agentRunId)))
                .materializeIntoChecked
                .map {
                  case Left(problem) => logger.error(s"Reset command failed: $problem")
                  case Right(_) =>
                })
              .runToFuture
              .foreach { _ =>
              // Ignore any Problem or Exception from Reset command
              self ! Internal.Terminate
            }

          case _ =>
            stopIfTerminated()
        }
      }

    case Internal.Terminate =>
      stopIfTerminated()

    case Input.Reset(force) =>
      val sender = this.sender()
      isReset = true
      commandQueue.reset()
      eventFetcher.coupledApi
        .flatMap {
          case None => Task.pure(false)
          case Some(api) =>
            api.commandExecute(AgentCommand.Reset(!force ? agentRunIdOnce.orThrow))
              .materializeIntoChecked
              .map {
                case Left(problem) =>
                  logger.warn(s"Reset: $problem")
                  false
                case Right(_) =>
                  true
              }
        }
        .flatMap(agentHasBeenReset =>
          eventFetcher.terminateAndLogout
            .*>(Task { currentFetchedFuture.foreach(_.cancel()) })
            .*>(cancelObservationAndAwaitTermination)
            .*>(eventFetcher.decouple)
            .as(agentHasBeenReset))
        .runToFuture
        .onComplete { tried =>
          sender ! (tried: Try[Boolean])
          if (tried.isSuccess) {
            context.stop(self)
          }
        }

    case Input.StartFetchingEvents | Internal.FetchEvents =>
      if (changingUri.isEmpty && !isTerminating) {
        assertThat(currentFetchedFuture.isEmpty, "Duplicate fetchEvents")
        currentFetchedFuture = Some(
          observeAndConsumeEvents
            .onCancelRaiseError(CancelledMarker)
            .runToFuture
            .andThen {
              case tried =>
                logger.trace(s"self ! Internal.FetchFinished($tried)")
                self ! Internal.FetchFinished(tried)
            })
      }

    case Internal.OnCoupled(promise, agentOrderIds) =>
      lastCouplingFailed = None
      delayNextReleaseEvents = false
      commandQueue.onCoupled(agentOrderIds)
      promise.success(Completed)

    case Internal.OnDecoupled =>
      commandQueue.onDecoupled()

    case Internal.FetchedEvents(stampedEvents, promise) =>
      assertThat(stampedEvents.nonEmpty)
      val reducedStampedEvents = stampedEvents dropWhile { stamped =>
        val drop = stamped.eventId <= lastFetchedEventId
        if (drop) logger.debug(s"Drop duplicate received event: $stamped")
        drop
      }

      // The events must be journaled and handled by ControllerOrderKeeper
      val lastEventId = stampedEvents.last.eventId
      lastFetchedEventId = lastEventId

      commandQueue.onOrdersAttached(
        reducedStampedEvents.view.collect {
          case Stamped(_, _, KeyedEvent(orderId: OrderId, _: OrderAttachedToAgent)) => orderId
        })
      commandQueue.onOrdersDetached(
        reducedStampedEvents.view.collect {
          case Stamped(_, _, KeyedEvent(orderId: OrderId, OrderDetached)) => orderId
        })

      promiseFuture[Option[EventId]] { p =>
        context.parent ! Output.EventsFromAgent(agentRunIdOnce.orThrow, reducedStampedEvents, p)
      } onComplete {
        case Success(Some(eventId)) =>
          self ! Internal.ReleaseEvents(eventId, promise)

        case Success(None) =>
          promise.success(Completed)

        case Failure(t) =>
          promise.complete(Failure(t))
      }

    case Internal.ReleaseEvents(lastEventId, promise) =>
      lastCommittedEventId = lastEventId
      if (releaseEventsCancelable.isEmpty) {
        val delay = if (delayNextReleaseEvents) conf.releaseEventsPeriod else ZeroDuration
        releaseEventsCancelable = Some(scheduler.scheduleOnce(delay) {
          self ! Internal.ReleaseEventsNow
        })
        delayNextReleaseEvents = true
      }
      promise.success(Completed)

    case Internal.FetchFinished(tried) =>
      // Message is expected only after ChangeUri or after InvalidSessionTokenProblem while executing a command
      logger.debug(s"FetchFinished $tried")
      currentFetchedFuture = None
      eventFetcher.decouple
        .*>(eventFetcher.pauseBeforeNextTry(conf.recouplingStreamReader.delay))
        .runToFuture
        .onComplete { _ =>
          if (!isTerminating) {
            if (changingUri.isDefined) {
              logger.trace("self ! Internal.UriChanged")
              self ! Internal.UriChanged
            } else {
              logger.trace("self ! Internal.FetchEvents")
              self ! Internal.FetchEvents
            }
          }
        }

    case Internal.ReleaseEventsNow =>
      if (!isTerminating) {
        commandQueue.enqueue(ReleaseEventsQueueable(lastCommittedEventId))
      }

    case Internal.CommandQueueReady =>
      commandQueue.maySend()

    case Internal.BatchSucceeded(responses) =>
      lastCouplingFailed = None
      val succeededInputs = commandQueue.handleBatchSucceeded(responses)

      val markedOrders = succeededInputs.view
        .collect { case o: Input.MarkOrder => o.orderId -> o.mark }
        .toMap
      if (markedOrders.nonEmpty) {
        context.parent ! Output.OrdersMarked(markedOrders)
      }

      val releaseEvents = succeededInputs collect { case o: ReleaseEventsQueueable => o }
      if (releaseEvents.nonEmpty) {
        releaseEventsCancelable foreach (_.cancel())
        releaseEventsCancelable = None
      }
      stopIfTerminated()

    case Internal.BatchFailed(inputs, problem) =>
      problem match {
        case DecoupledProblem |
             InvalidSessionTokenProblem |
             RecouplingStreamReader.TerminatedProblem =>
          logger.debug(s"Command batch failed: $problem")
        case _ =>
          logger.warn(s"Command batch failed: $problem")
      }
      logger.trace(s"delayCommandExecutionAfterErrorUntil=${Timestamp.ofDeadline(delayCommandExecutionAfterErrorUntil)}")
      delayCommandExecutionAfterErrorUntil = now + conf.commandErrorDelay
      commandQueue.handleBatchFailed(inputs)
      stopIfTerminated()
  }

  private def cancelObservationAndAwaitTermination: Task[Completed] =
    Task.defer {
      currentFetchedFuture match {
        case None => Task.completed
        case Some(fetchedFuture) =>
          fetchedFuture.cancel()
          Task.fromFuture(fetchedFuture)
            .onErrorRecover { case t =>
              if (t ne CancelledMarker) logger.warn(t.toStringWithCauses)
              Completed
            }
      }
    }

  private def observeAndConsumeEvents: Task[Completed] =
    Task.defer {
      val delay = conf.eventBufferDelay max conf.commitDelay
      eventFetcher.observe(client, after = lastFetchedEventId)
        .pipe(obs =>
          if (delay.isZeroOrBelow)
            obs.bufferIntrospective(conf.eventBufferSize)
          else obs
            .buffer(
              Some(conf.eventBufferDelay max conf.commitDelay),
              maxCount = conf.eventBufferSize)  // ticks
            .filter(_.nonEmpty))   // Ignore empty ticks
        .flatMap(o => Observable
          // When the other cluster node may have failed-over,
          // wait until we know that it hasn't (or this node is aborted).
          // Avoids "Unknown OrderId" failures due to double activation.
          .fromTask(persistence.whenNoFailoverByOtherNode
            .logWhenItTakesLonger("whenNoFailoverByOtherNode"))
          .map(_ => o))
        .mapEval(stampedEvents =>
          promiseTask[Completed] { promise =>
            self ! Internal.FetchedEvents(stampedEvents, promise)
          })
        .completedL
        .map(_ => Completed)
    }

  private def dedicateAgentIfNeeded: Task[Checked[(AgentRunId, EventId)]] =
    Task.defer {
      agentRunIdOnce.toOption match {
        case Some(agentRunId) => Task.pure(Right(agentRunId -> lastFetchedEventId))
        case None =>
          client
            .commandExecute(
              DedicateAgentDirector(agentRef.director, controllerId, agentPath))
            .flatMapT { case DedicateAgentDirector.Response(agentRunId, agentEventId) =>
              (if (noJournal)
                Task.pure(Checked.unit)
              else
                persistence
                  .persistKeyedEvent(
                    agentPath <-: AgentDedicated(agentRunId, Some(agentEventId)))
                  .map(_.map { _ =>
                    // Asynchronous assignment
                    agentRunIdOnce := agentRunId
                    reattachSubagents()
                  }))
              .rightAs(agentRunId -> agentEventId)
            }
      }
    }

  private def reattachSubagents(): Unit = {
    val controllerState = persistence.currentState
    controllerState.itemToAgentToAttachedState
      .foreach {
        case (subagentId: SubagentId, agentToAttachedState) =>
          // After Agent Reset, re-attach SubagentItems
          controllerState.pathToUnsignedSimpleItem.get(subagentId)
            .foreach(item => agentToAttachedState.get(agentPath)
              .foreach {
                case Attachable =>
                  self ! Input.AttachUnsignedItem(item)

                case Attached(rev) =>
                  if (item.itemRevision == rev) {
                    self ! Input.AttachUnsignedItem(item)
                  }

                case _ =>
              })

        case _ =>
      }
  }

  private def stopIfTerminated() =
    if (commandQueue.isTerminated) {
      logger.debug("Stop")
      case object Stop
      currentFetchedFuture.foreach(_.cancel())
      releaseEventsCancelable.foreach(_.cancel())
      Task.fromFuture(eventFetcherTerminated.future)
        .onErrorRecover { case t => logger.debug(t.toStringWithCauses) }
        .flatMap(_ => closeClient)
        .onErrorRecover { case t => logger.debug(t.toStringWithCauses) }
        .foreach(_ => self ! Stop)
      become("stopping") {
        case Stop => context.stop(self)
      }
    }

  private def closeClient: Task[Completed] =
    eventFetcher.invalidateCoupledApi
      .materialize
      .flatMap(_ => client.tryLogout)
      .guarantee(Task {
        client.close()
      })

  override def toString = s"AgentDriver($agentPath)"
}

private[controller] object AgentDriver
{
  private val EventClasses = Set[Class[? <: Event]](
    classOf[OrderEvent],
    classOf[AgentEvent.AgentReady],
    AgentEvent.AgentShutDown.getClass,
    classOf[SubagentItemStateEvent],
    classOf[InventoryItemEvent],
    classOf[OrderWatchEvent])
  private val DecoupledProblem = Problem.pure("Agent has been decoupled")

  def props(agentRef: AgentRef, agentRunId: Option[AgentRunId], eventId: EventId,
    persistence: StatePersistence[ControllerState],
    agentDriverConf: AgentDriverConfiguration, controllerConf: ControllerConfiguration)
    (implicit s: Scheduler)
  =
    Props { new AgentDriver(agentRef, agentRunId, eventId,
      persistence, agentDriverConf, controllerConf) }

  sealed trait Queueable extends Input {
    def toShortString = toString
  }

  private[agent] final case class ReleaseEventsQueueable(agentEventId: EventId) extends Queueable

  sealed trait Input
  object Input {
    case object StartFetchingEvents

    final case class ChangeUri(agentRef: AgentRef, uri: Uri)

    final case class AttachUnsignedItem(item: UnsignedItem)
    extends Input with Queueable

    final case class AttachSignedItem(signed: Signed[SignableItem])
    extends Input with Queueable

    final case class DetachItem(key: InventoryItemKey)
    extends Input with Queueable

    final case class AttachOrder(order: Order[Order.IsFreshOrReady], agentPath: AgentPath)
    extends Input with Queueable {
      override lazy val hashCode = order.id.hashCode

      def orderId = order.id
      override def toShortString = s"AttachOrder(${orderId.string}, ${order.workflowPosition}, ${order.state.getClass.simpleScalaName})"
    }

    final case class DetachOrder(orderId: OrderId) extends Input with Queueable

    final case class MarkOrder(orderId: OrderId, mark: OrderMark) extends Input with Queueable

    final case class Terminate(
      noJournal: Boolean = false,
      reset: Boolean = false)
    extends DeadLetterSuppression

    final case class Reset(force: Boolean) extends DeadLetterSuppression

    final case class ResetSubagent(subagentId: SubagentId, force: Boolean) extends Queueable
  }

  object Output {
    final case class EventsFromAgent(
      agentRunId: AgentRunId,
      stamped: Seq[Stamped[AnyKeyedEvent]],
      promise: Promise[Option[EventId]])

    final case class OrdersMarked(orderToMark: Map[OrderId, OrderMark])
  }

  private object Internal {
    case object CommandQueueReady extends DeadLetterSuppression
    final case class BatchSucceeded(responses: Seq[QueuedInputResponse]) extends DeadLetterSuppression
    final case class BatchFailed(inputs: Seq[Queueable], problem: Problem) extends DeadLetterSuppression
    case object FetchEvents extends DeadLetterSuppression
    final case class FetchedEvents(events: Seq[Stamped[AnyKeyedEvent]], promise: Promise[Completed])
      extends DeadLetterSuppression
    final case class FetchFinished(tried: Try[Completed]) extends DeadLetterSuppression
    final case class OnCoupled(promise: Promise[Completed], orderIds: Set[OrderId]) extends DeadLetterSuppression
    case object OnDecoupled extends DeadLetterSuppression
    final case class ReleaseEvents(lastEventId: EventId, promise: Promise[Completed]) extends DeadLetterSuppression
    case object ReleaseEventsNow extends DeadLetterSuppression
    case object UriChanged extends DeadLetterSuppression
    case object Terminate
  }

  private case object CancelledMarker extends Exception with NoStackTrace
}
