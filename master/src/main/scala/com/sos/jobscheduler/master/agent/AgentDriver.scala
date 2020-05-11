package com.sos.jobscheduler.master.agent

import akka.actor.{ActorRef, DeadLetterSuppression, Props}
import cats.data.EitherT
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{CoupleMaster, RegisterAsMaster}
import com.sos.jobscheduler.agent.data.event.AgentMasterEvent
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.crypt.Signed
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.base.utils.SetOnce
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.akkautils.ReceiveLoggingActor
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.http.RecouplingStreamReader
import com.sos.jobscheduler.common.scalautil.Futures.promiseFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.promiseTask
import com.sos.jobscheduler.core.event.journal.{JournalActor, KeyedJournalingActor}
import com.sos.jobscheduler.data.agent.{AgentRefPath, AgentRunId}
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, Stamped}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.agent.AgentDriver._
import com.sos.jobscheduler.master.agent.CommandQueue.QueuedInputResponse
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterState
import com.sos.jobscheduler.master.data.events.MasterAgentEvent
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.{AgentCouplingFailed, AgentRegisteredMaster}
import com.typesafe.config.ConfigUtil
import monix.eval.Task
import monix.execution.atomic.{AtomicInt, AtomicLong}
import monix.execution.{Cancelable, CancelableFuture, Scheduler}
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success, Try}
import shapeless.tag.@@

/**
  * Couples to an Agent, sends orders, and fetches events.
  *
  * @author Joacim Zschimmer
  */
final class AgentDriver private(agentRefPath: AgentRefPath,
  initialUri: Uri,
  initialAgentRunId: Option[AgentRunId],
  initialEventId: EventId,
  conf: AgentDriverConfiguration,
  masterConfiguration: MasterConfiguration,
  protected val journalActor: ActorRef @@ JournalActor.type)
  (implicit scheduler: Scheduler)
extends KeyedJournalingActor[MasterState, MasterAgentEvent]
with ReceiveLoggingActor.WithStash
{
  private val logger = Logger.withPrefix[this.type](agentRefPath.string)
  private val agentUserAndPassword: Option[UserAndPassword] =
    masterConfiguration.config.optionAs[SecretString]("jobscheduler.auth.agents." + ConfigUtil.joinPath(agentRefPath.string))
      .map(password => UserAndPassword(masterConfiguration.masterId.toUserId, password))

  private val agentRunIdOnce = SetOnce.fromOption(initialAgentRunId)
  private var client = newAgentClient(initialUri)
  /** Only filled when coupled */
  private var lastFetchedEventId = initialEventId
  private var lastCommittedEventId = initialEventId
  private var lastProblem: Option[Problem] = None
  private var currentFetchedFuture: Option[CancelableFuture[Completed]] = None
  private var releaseEventsCancelable: Option[Cancelable] = None
  private var delayNextReleaseEvents = false
  private var delayCommandExecutionAfterErrorUntil = now
  private var isTerminating = false
  private var changingUri: Option[Uri] = None
  private val sessionNumber = AtomicInt(0)

  private val eventFetcher = new RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], AgentClient](
    _.eventId, conf.recouplingStreamReader)
  {
    private var attachedOrderIds: Set[OrderId] = null

    override protected def couple(eventId: EventId) =
      registerAsMasterIfNeeded >>
        client.commandExecute(CoupleMaster(agentRunIdOnce.orThrow, eventId = eventId))
          .map(_.map { case CoupleMaster.Response(orderIds) =>
            logger.trace(s"CoupleMaster returned attached OrderIds={${orderIds.toSeq.sorted.mkString(" ")}}")
            attachedOrderIds = orderIds
            Completed
          })

    protected def getObservable(api: AgentClient, after: EventId) =
      Task { logger.debug(s"getObservable(after=$after)") } >>
        api.mastersEventObservable(EventRequest[Event](EventClasses, after = after, timeout = Some(idleTimeout)))

    override protected def onCouplingFailed(api: AgentClient, problem: Problem) =
      promiseTask[Completed] {
        promise => self ! Internal.OnCouplingFailed(problem, promise)
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
      val expectedSessionNumber: Int = sessionNumber.get
      for {
        _ <- Task.defer {
          val delay = delayCommandExecutionAfterErrorUntil.timeLeft
          if (delay >= Duration.Zero) logger.debug(s"${AgentDriver.this.toString}: Delay command after error until ${Timestamp.now + delay}")
          Task.sleep(delay)
        }
        checkedApi <- eventFetcher.coupledApi.map(_.toRight(DecoupledProblem))
        response <-
          (for {
            // Fail on recoupling, later read restarted Agent's attached OrderIds before issuing again AttachOrder
            api <- EitherT(Task.pure(checkedApi))
            response <- EitherT(
              if (sessionNumber.get != expectedSessionNumber)
                Task.pure(Left(DecoupledProblem))
              else
                // TODO Still a small possibility for race-condition? May log a AgentDuplicateOrderProblem
                api.commandExecute(command))
          } yield response).value
      } yield response
    }

    protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) =
      self ! Internal.BatchSucceeded(queuedInputResponses)

    protected def asyncOnBatchFailed(inputs: Vector[Queueable], problem: Problem) =
      if (problem == DecoupledProblem) {  // Avoid loop
        self ! Internal.BatchFailed(inputs, problem)
      } else
        (eventFetcher.invalidateCoupledApi >>
          Task { currentFetchedFuture.foreach(_.cancel()) } >>
          cancelObservationAndAwaitTermination >>
          eventFetcher.decouple >>
          Task {
            self ! Internal.BatchFailed(inputs, problem)
          }
        ).runToFuture
  }

  protected def key = agentRefPath  // Only one version is active at any time
  protected def recoverFromSnapshot(snapshot: Any) = throw new NotImplementedError
  protected def recoverFromEvent(event: MasterAgentEvent) = throw new NotImplementedError
  protected def snapshot = None  // MasterOrderKeeper provides the AgentSnapshot

  override def postStop() = {
    eventFetcher.invalidateCoupledApi.materialize
      .flatMap(_ =>
        client.logout())
          .onErrorHandle(_ => ())
          .guarantee(Task(client.close()))
          .runAsyncAndForget
    currentFetchedFuture foreach (_.cancel())
    releaseEventsCancelable foreach (_.cancel())
    super.postStop()
  }

  private def newAgentClient(uri: Uri): AgentClient =
    AgentClient(uri, agentUserAndPassword,
      masterConfiguration.keyStoreRefOption, masterConfiguration.trustStoreRefOption)(context.system)

  def receive = {
    case input: Input with Queueable if sender() == context.parent && !isTerminating =>
      commandQueue.enqueue(input)
      scheduler.scheduleOnce(conf.commandBatchDelay) {
        self ! Internal.CommandQueueReady  // (Even with commandBatchDelay == 0) delay maySend() such that Queueable pending in actor's mailbox can be queued
      }

    case Input.ChangeUri(uri) =>
      if (uri != client.baseUri) {
        logger.debug(s"ChangeUri $uri")
        // TODO Changing URI in quick succession is not properly solved
        for (u <- changingUri) logger.warn(s"Already changing URI to $u ?")
        changingUri = Some(uri)
        (cancelObservationAndAwaitTermination >>
          eventFetcher.decouple
        ).runToFuture
          .onComplete {
            case Success(_) =>
            case Failure(t) => logger.error(t.toStringWithCauses)
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

    case Input.Terminate =>
      // Wait until all pending Agent commands are responded, and do not accept further commands
      logger.debug("Terminate")
      isTerminating = true
      currentFetchedFuture foreach (_.cancel())
      commandQueue.terminate()
      eventFetcher.terminate.runAsyncAndForget  // Rejects current commands waiting for coupling
      stopIfTerminated()

    case Input.StartFetchingEvents | Internal.FetchEvents =>
      if (changingUri.isEmpty && !isTerminating) {
        assertThat(currentFetchedFuture.isEmpty, "Duplicate fetchEvents")
        currentFetchedFuture = Some(
          observeAndConsumeEvents
            .onCancelRaiseError(CancelledMarker)
            .executeWithOptions(_.enableAutoCancelableRunLoops)
            .runToFuture
            .andThen {
              case tried =>
                logger.trace(s"self ! Internal.FetchFinished($tried)")
                self ! Internal.FetchFinished(tried)
            })
      }

    case Internal.OnCoupled(promise, agentOrderIds) =>
      lastProblem = None
      delayNextReleaseEvents = false
      commandQueue.onCoupled(agentOrderIds)
      promise.success(Completed)

    case Internal.OnCouplingFailed(problem, promise) =>
      promise.completeWith(
        if (lastProblem.contains(problem)) {
          logger.debug(s"Coupling failed: $problem")
          Future.successful(Completed)
        } else {
          lastProblem = Some(problem)
          logger.warn(s"Coupling failed: $problem")
          for (t <- problem.throwableOption if t.getStackTrace.nonEmpty) logger.debug(s"Coupling failed: $problem", t)
          persist(AgentCouplingFailed(problem), async = true) { (_, _) =>
            Completed
          }
        })

    case Internal.OnDecoupled =>
      commandQueue.onDecoupled()

    case Internal.FetchedEvents(stampedEvents, promise) =>
      assertThat(stampedEvents.nonEmpty)
      val reducedStampedEvents = stampedEvents dropWhile { stamped =>
        val drop = stamped.eventId <= lastFetchedEventId
        if (drop) logger.debug(s"Drop duplicate received event: $stamped")
        drop
      }

      // The events must be journaled and handled by MasterOrderKeeper
      val lastEventId = stampedEvents.last.eventId
      lastFetchedEventId = lastEventId

      promiseFuture[Completed] { p =>
        context.parent ! Output.EventsFromAgent(reducedStampedEvents, p)
      } onComplete {
        case Success(Completed) =>
          self ! Internal.EventsAccepted(lastEventId, promise)
        case o => promise.complete(o)
      }

    case Internal.EventsAccepted(lastEventId, promise) =>
      lastCommittedEventId = lastEventId
      if (releaseEventsCancelable.isEmpty) {
        val delay = if (delayNextReleaseEvents) conf.releaseEventsPeriod else Duration.Zero
        releaseEventsCancelable = Some(scheduler.scheduleOnce(delay) {
          self ! Internal.ReleaseEvents
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

    case Internal.ReleaseEvents =>
      if (!isTerminating) {
        commandQueue.enqueue(ReleaseEventsQueueable(lastCommittedEventId))
      }

    case Internal.CommandQueueReady =>
      commandQueue.maySend()

    case Internal.BatchSucceeded(responses) =>
      lastProblem = None
      val succeededInputs = commandQueue.handleBatchSucceeded(responses)

      val detachedOrderIds = succeededInputs collect { case Input.DetachOrder(orderId) => orderId }
      if (detachedOrderIds.nonEmpty) {
        context.parent ! Output.OrdersDetached(detachedOrderIds.toSet)
      }

      val cancelledOrderIds = succeededInputs collect { case o: Input.CancelOrder => o.orderId }
      if (cancelledOrderIds.nonEmpty) {
        context.parent ! Output.OrdersCancellationMarked(cancelledOrderIds.toSet)
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
             RecouplingStreamReader.TerminatedProblem =>
          logger.debug(s"Command batch failed: $problem")
        case _ =>
          logger.warn(s"Command batch failed: $problem")
      }
      delayCommandExecutionAfterErrorUntil = now + conf.commandErrorDelay
      logger.trace(s"delayCommandExecutionAfterErrorUntil=${Timestamp.ofDeadline(delayCommandExecutionAfterErrorUntil)}")
      commandQueue.handleBatchFailed(inputs)
      stopIfTerminated()
  }

  private def cancelObservationAndAwaitTermination: Task[Completed] =
    currentFetchedFuture match {
      case None => Task.pure(Completed)
      case Some(fetchedFuture) =>
        fetchedFuture.cancel()
        Task.fromFuture(fetchedFuture)
          .onErrorRecover { case t =>
            if (t ne CancelledMarker) logger.warn(t.toStringWithCauses)
            Completed
          }
    }

  protected def observeAndConsumeEvents: Task[Completed] =
    eventFetcher.observe(client, after = lastFetchedEventId)
      //.map { a => logEvent(a); a }
      .bufferTimedAndCounted(
        conf.eventBufferDelay max conf.commitDelay,
        maxCount = conf.eventBufferSize)  // ticks
      .filter(_.nonEmpty)   // Ignore empty ticks
      .map(_.toSeq)
      .mapEval(stampedEvents =>
        promiseTask[Completed] { promise =>
          self ! Internal.FetchedEvents(stampedEvents, promise)
        })
      .completedL
      .map(_ => Completed)

  private def registerAsMasterIfNeeded: Task[Completed] =
    if (agentRunIdOnce.nonEmpty)
      Task.pure(Completed)
    else
      client.commandExecute(RegisterAsMaster)
        .map(_.map(_.agentRunId).orThrowWithoutStacktrace/*TODO Safe original Problem, package in special ProblemException and let something like Problem.pure don't _wrap_ the Problem?*/)
        .flatMap { agentRunId =>
          val event = AgentRegisteredMaster(agentRunId)
          persistTask(event) { (_, _) =>
            // asynchronous
            agentRunIdOnce := agentRunId
            Completed
          }.map(_.orThrow)
        }

  private object logEvent {
    // This object is used asynchronously
    private val logEventCount = AtomicLong(0)
    def apply(stampedEvent: Stamped[AnyKeyedEvent]): Unit =
      logger.whenTraceEnabled {
        val i = logEventCount.incrementAndGet()
        logger.trace(s"#$i $stampedEvent")
      }
  }

  private def stopIfTerminated() =
    if (commandQueue.isTerminated) {
      logger.debug("Stop")
      context.stop(self)
    }

  override def toString = s"AgentDriver($agentRefPath)"
}

private[master] object AgentDriver
{
  private val EventClasses = Set[Class[_ <: Event]](classOf[OrderEvent], classOf[AgentMasterEvent.AgentReadyForMaster])
  private val DecoupledProblem = Problem.pure("Agent has been decoupled")

  def props(agentRefPath: AgentRefPath, uri: Uri, agentRunId: Option[AgentRunId], eventId: EventId,
    agentDriverConfiguration: AgentDriverConfiguration, masterConfiguration: MasterConfiguration,
    journalActor: ActorRef @@ JournalActor.type)(implicit s: Scheduler)
  =
    Props { new AgentDriver(agentRefPath, uri, agentRunId, eventId, agentDriverConfiguration, masterConfiguration, journalActor) }

  sealed trait Queueable extends Input {
    def toShortString = toString
  }

  private[agent] final case class ReleaseEventsQueueable(agentEventId: EventId) extends Queueable

  sealed trait Input
  object Input {
    case object StartFetchingEvents

    final case class ChangeUri(uri: Uri)

    final case class AttachOrder(order: Order[Order.IsFreshOrReady], agentRefPath: AgentRefPath, signedWorkflow: Signed[Workflow])
    extends Input with Queueable {
      def orderId = order.id
      override def toShortString = s"AttachOrder(${orderId.string})"
    }

    final case class DetachOrder(orderId: OrderId) extends Input with Queueable

    final case class CancelOrder(orderId: OrderId, mode: CancelMode) extends Input with Queueable

    case object Terminate
  }

  object Output {
    final case class EventsFromAgent(stamped: Seq[Stamped[AnyKeyedEvent]], promise: Promise[Completed])
    final case class OrdersDetached(orderIds: Set[OrderId])
    final case class OrdersCancellationMarked(orderIds: Set[OrderId])
  }

  private object Internal {
    final case object CommandQueueReady extends DeadLetterSuppression
    final case class BatchSucceeded(responses: Seq[QueuedInputResponse]) extends DeadLetterSuppression
    final case class BatchFailed(inputs: Seq[Queueable], problem: Problem) extends DeadLetterSuppression
    final case object FetchEvents extends DeadLetterSuppression
    final case class FetchedEvents(events: Seq[Stamped[AnyKeyedEvent]], promise: Promise[Completed])
      extends DeadLetterSuppression
    final case class FetchFinished(tried: Try[Completed]) extends DeadLetterSuppression
    final case class OnCouplingFailed(problem: Problem, promise: Promise[Completed]) extends DeadLetterSuppression
    final case class OnCoupled(promise: Promise[Completed], orderIds: Set[OrderId]) extends DeadLetterSuppression
    final case object OnDecoupled extends DeadLetterSuppression
    final case class EventsAccepted(lastEventId: EventId, promise: Promise[Completed]) extends DeadLetterSuppression
    final case object ReleaseEvents extends DeadLetterSuppression
    final case object UriChanged extends DeadLetterSuppression
  }

  private object CancelledMarker extends Exception with NoStackTrace
}
