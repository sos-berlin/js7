package js7.controller.agent

import akka.actor.{DeadLetterSuppression, Props}
import cats.data.EitherT
import com.typesafe.config.ConfigUtil
import js7.agent.client.AgentClient
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{CoupleController, CreateAgent}
import js7.agent.data.event.AgentEvent
import js7.base.auth.UserAndPassword
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.crypt.Signed
import js7.base.generic.{Completed, SecretString}
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.promiseTask
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked._
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.problem.{Checked, Problem}
import js7.base.thread.Futures.promiseFuture
import js7.base.thread.Futures.syntax.RichFuture
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.{RichThrowable, _}
import js7.base.utils.SetOnce
import js7.base.web.Uri
import js7.common.akkautils.ReceiveLoggingActor
import js7.common.http.RecouplingStreamReader
import js7.controller.agent.AgentDriver._
import js7.controller.agent.CommandQueue.QueuedInputResponse
import js7.controller.configuration.ControllerConfiguration
import js7.data.agent.AgentRefState.Resetting
import js7.data.agent.AgentRefStateEvent.{AgentCouplingFailed, AgentCreated, AgentReset}
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerState
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, KeyedEvent, Stamped}
import js7.data.item.{InventoryItemEvent, InventoryItemKey, SignableItem, UnsignedSimpleItem}
import js7.data.order.OrderEvent.{OrderAttachedToAgent, OrderDetached}
import js7.data.order.{Order, OrderEvent, OrderId, OrderMark}
import js7.data.orderwatch.OrderWatchEvent
import js7.journal.state.JournaledStatePersistence
import monix.eval.Task
import monix.execution.atomic.AtomicInt
import monix.execution.{Cancelable, CancelableFuture, Scheduler}
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
final class AgentDriver private(agentPath: AgentPath,
  initialUri: Uri,
  initialAgentRunId: Option[AgentRunId],
  initialEventId: EventId,
  persistence: JournaledStatePersistence[ControllerState],
  conf: AgentDriverConfiguration,
  controllerConfiguration: ControllerConfiguration)
  (implicit protected val scheduler: Scheduler)
extends ReceiveLoggingActor.WithStash
{
  import controllerConfiguration.controllerId

  protected def journalConf = controllerConfiguration.journalConf

  private val logger = Logger.withPrefix[this.type](agentPath.string)
  private val agentUserAndPassword: Option[UserAndPassword] =
    controllerConfiguration.config.optionAs[SecretString]("js7.auth.agents." + ConfigUtil.joinPath(agentPath.string))
      .map(password => UserAndPassword(controllerConfiguration.controllerId.toUserId, password))

  private val agentRunIdOnce = SetOnce.fromOption(initialAgentRunId)
  private var client = newAgentClient(initialUri)
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
      Task(persistence.currentState.pathToAgentRefState.checked(agentPath))
        .flatMapT(agentRefState =>
          ((agentRefState.couplingState, agentRunIdOnce.toOption) match {
            case (Resetting, Some(agentRunId)) =>
              client.commandExecute(AgentCommand.Reset(agentRunId))
                .flatMapT(_ => persistence.persistKeyedEvent(agentPath <-: AgentReset))
            case _ =>
              Task.pure(Checked.unit)
          })
        .flatMapT(_ => createAgentIfNeeded)
        .flatMapT(agentRunId =>
          client.commandExecute(CoupleController(agentPath, agentRunId, eventId = eventId))
            .map(_.map { case CoupleController.Response(orderIds) =>
              logger.trace(s"CoupleController returned attached OrderIds={${orderIds.toSeq.sorted.mkString(" ")}}")
              attachedOrderIds = orderIds
              Completed
            })))

    protected def getObservable(api: AgentClient, after: EventId) =
      Task { logger.debug(s"getObservable(after=$after)") } >>
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
      } yield response
    }

    protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) =
      self ! Internal.BatchSucceeded(queuedInputResponses)

    protected def asyncOnBatchFailed(inputs: Vector[Queueable], problem: Problem) =
      if (problem == DecoupledProblem/*avoid loop*/ || isTerminating) {
        self ! Internal.BatchFailed(inputs, problem)
      } else
        (eventFetcher.invalidateCoupledApi >>
          Task { currentFetchedFuture.foreach(_.cancel()) } >>
          cancelObservationAndAwaitTermination >>
          eventFetcher.decouple >>
          Task {
            self ! Internal.BatchFailed(inputs, problem)
          }
        ).runToFuture.onFailure { case t =>
          logger.error("asyncOnBatchFailed: " + t.toStringWithCauses, t.nullIfNoStackTrace)
        }
  }

  override def preStart() = {
    super.preStart()
    logger.debug("preStart")
  }

  override def postStop() = {
    super.postStop()
    logger.debug("postStop")
  }

  protected def key = agentPath  // Only one version is active at any time

  private def newAgentClient(uri: Uri): AgentClient =
    AgentClient(uri, agentUserAndPassword, label = agentPath.toString,
      controllerConfiguration.keyStoreRefOption, controllerConfiguration.trustStoreRefs)(context.system)

  def receive = {
    case input: Input with Queueable if sender() == context.parent && !isTerminating =>
      val ok = commandQueue.enqueue(input)
      if (ok) scheduler.scheduleOnce(conf.commandBatchDelay) {
        self ! Internal.CommandQueueReady  // (Even with commandBatchDelay == 0) delay maySend() such that Queueable pending in actor's mailbox can be queued
      }

    case Input.ChangeUri(uri) =>
      if (uri != client.baseUri || isReset) {
        logger.debug(s"ChangeUri $uri")
        // TODO Changing URI in quick succession is not properly solved
        isReset = false
        for (u <- changingUri) logger.warn(s"Already changing URI to $u ?")
        changingUri = Some(uri)
        (cancelObservationAndAwaitTermination >>
          eventFetcher.decouple
        ).runToFuture.onFailure { case t =>
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

    case Input.Terminate(noJournal) =>
      this.noJournal = noJournal
      // Wait until all pending Agent commands are responded, and do not accept further commands
      if (!isTerminating) {
        logger.debug(s"Terminate " + (noJournal ?? "noJournal"))
        isTerminating = true
        commandQueue.terminate()
        currentFetchedFuture.foreach(_.cancel())
        eventFetcherTerminated.completeWith(
          eventFetcher.terminate.runToFuture)  // Rejects current commands waiting for coupling
        stopIfTerminated()
      }

    case Input.Reset =>
      val sender = this.sender()
      isReset = true
      commandQueue.reset()
      (eventFetcher.coupledApi
        .flatMap {
          case None => Task.pure(false)
          case Some(api) =>
            api.commandExecute(AgentCommand.Reset(agentRunIdOnce.orThrow))
              .map(_.isRight)
        }
        .flatMap(agentHasBeenReset =>
          eventFetcher.invalidateCoupledApi >>
            Task { currentFetchedFuture.foreach(_.cancel()) } >>
            cancelObservationAndAwaitTermination >>
            eventFetcher.decouple
              .as(agentHasBeenReset)
          ) .runToFuture
            .onComplete { tried =>
              sender ! (tried: Try[Boolean])
              context.stop(self)
            })

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

      commandQueue.onOrdersAttached(reducedStampedEvents.view.collect {
        case Stamped(_, _, KeyedEvent(orderId: OrderId, _: OrderAttachedToAgent)) => orderId
      })
      commandQueue.onOrdersDetached(reducedStampedEvents.view.collect {
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
      (eventFetcher.decouple >>
        eventFetcher.pauseBeforeNextTry(conf.recouplingStreamReader.delay)
      ).runToFuture
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

      val markedOrders = succeededInputs.view.collect { case o: Input.MarkOrder => o.orderId -> o.mark }.toMap
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

  protected def observeAndConsumeEvents: Task[Completed] =
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
        .mapEval(stampedEvents =>
          promiseTask[Completed] { promise =>
            self ! Internal.FetchedEvents(stampedEvents, promise)
          })
        .completedL
        .map(_ => Completed)
    }

  private def createAgentIfNeeded: Task[Checked[AgentRunId]] =
    Task.defer {
      agentRunIdOnce.toOption match {
        case Some(agentRunId) => Task.pure(Right(agentRunId))
        case None =>
          client.commandExecute(CreateAgent(agentPath, controllerId)).map(_.map(_.agentRunId))
            .flatMapT(agentRunId =>
              if (noJournal)
                Task.pure(Right(agentRunId))
              else
                persistence.persistKeyedEvent(agentPath <-: AgentCreated(agentRunId))
                  .map(_.map { case (_, _) =>
                    // asynchronous
                    agentRunIdOnce := agentRunId
                    agentRunId
                  }))
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
  private val EventClasses = Set[Class[_ <: Event]](
    classOf[OrderEvent],
    classOf[AgentEvent.AgentReady],
    classOf[AgentEvent.AgentShutDown],
    classOf[InventoryItemEvent],
    classOf[OrderWatchEvent])
  private val DecoupledProblem = Problem.pure("Agent has been decoupled")

  def props(agentPath: AgentPath, uri: Uri, agentRunId: Option[AgentRunId], eventId: EventId,
    persistence: JournaledStatePersistence[ControllerState],
    agentDriverConf: AgentDriverConfiguration, controllerConf: ControllerConfiguration)
    (implicit s: Scheduler)
  =
    Props { new AgentDriver(agentPath, uri, agentRunId, eventId,
      persistence, agentDriverConf, controllerConf) }

  sealed trait Queueable extends Input {
    def toShortString = toString
  }

  private[agent] final case class ReleaseEventsQueueable(agentEventId: EventId) extends Queueable

  sealed trait Input
  object Input {
    case object StartFetchingEvents

    final case class ChangeUri(uri: Uri)

    final case class AttachUnsignedItem(item: UnsignedSimpleItem)
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

    final case class Terminate(noJournal: Boolean = false) extends DeadLetterSuppression

    final case object Reset extends DeadLetterSuppression
  }

  object Output {
    final case class EventsFromAgent(
      agentRunId: AgentRunId,
      stamped: Seq[Stamped[AnyKeyedEvent]],
      promise: Promise[Option[EventId]])

    final case class OrdersMarked(orderToMark: Map[OrderId, OrderMark])
  }

  private object Internal {
    final case object CommandQueueReady extends DeadLetterSuppression
    final case class BatchSucceeded(responses: Seq[QueuedInputResponse]) extends DeadLetterSuppression
    final case class BatchFailed(inputs: Seq[Queueable], problem: Problem) extends DeadLetterSuppression
    final case object FetchEvents extends DeadLetterSuppression
    final case class FetchedEvents(events: Seq[Stamped[AnyKeyedEvent]], promise: Promise[Completed])
      extends DeadLetterSuppression
    final case class FetchFinished(tried: Try[Completed]) extends DeadLetterSuppression
    final case class OnCoupled(promise: Promise[Completed], orderIds: Set[OrderId]) extends DeadLetterSuppression
    final case object OnDecoupled extends DeadLetterSuppression
    final case class ReleaseEvents(lastEventId: EventId, promise: Promise[Completed]) extends DeadLetterSuppression
    final case object ReleaseEventsNow extends DeadLetterSuppression
    final case object UriChanged extends DeadLetterSuppression
  }

  private case object CancelledMarker extends Exception with NoStackTrace
}
