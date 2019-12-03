package com.sos.jobscheduler.master.agent

import akka.actor.{ActorRef, DeadLetterSuppression, Props}
import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.event.AgentMasterEvent
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.problem.Problems.InvalidSessionTokenProblem
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.common.akkautils.ReceiveLoggingActor
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.http.RecouplingStreamReader
import com.sos.jobscheduler.common.scalautil.Futures.promiseFuture
import com.sos.jobscheduler.common.scalautil.MonixUtils.promiseTask
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.core.event.journal.KeyedJournalingActor
import com.sos.jobscheduler.data.agent.{AgentRefPath, AgentRunId}
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.crypt.Signed
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, Stamped}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.agent.AgentDriver._
import com.sos.jobscheduler.master.agent.CommandQueue.QueuedInputResponse
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.events.MasterAgentEvent
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentRegisteredMaster
import com.typesafe.config.ConfigUtil
import monix.eval.Task
import monix.execution.atomic.AtomicLong
import monix.execution.{Cancelable, CancelableFuture, Scheduler}
import scala.collection.immutable.Seq
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success, Try}

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
  protected val journalActor: ActorRef)
  (implicit scheduler: Scheduler)
extends KeyedJournalingActor[MasterAgentEvent]
with ReceiveLoggingActor.WithStash
{
  private val logger = Logger.withPrefix[this.type](agentRefPath.string)
  private val agentUserAndPassword: Option[UserAndPassword] =
    masterConfiguration.config.optionAs[SecretString]("jobscheduler.auth.agents." + ConfigUtil.joinPath(agentRefPath.string))
      .map(password => UserAndPassword(masterConfiguration.masterId.toUserId, password))

  private val agentRunIdOnce = SetOnce.fromOption(initialAgentRunId)
  private var client = newAgentClient(initialUri)
  /** Only filled when coupled */
  private var lastEventId = initialEventId
  private var lastProblem: Option[Problem] = None
  private var currentFetchedFuture: Option[CancelableFuture[Completed]] = None
  private var keepEventsCancelable: Option[Cancelable] = None
  private var delayNextKeepEvents = false
  private var delayCommandExecutionAfterErrorUntil = now
  private var isTerminating = false

  private val eventFetcher = new RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], AgentClient](
    _.eventId, agentUserAndPassword, conf.recouplingStreamReader)
  {
    override protected def couple(eventId: EventId) =
      registerAsMasterIfNeeded >>
        client.commandExecute(AgentCommand.CoupleMaster(agentRunIdOnce.orThrow, eventId = eventId))
          .map(_.map((_: AgentCommand.Response.Accepted) => Completed))

    protected def getObservable(api: AgentClient, after: EventId) =
      Task { logger.debug(s"getObservable(after=$after)") } >>
        api.mastersEventObservable(EventRequest[Event](EventClasses, after = after, timeout = Some(conf.recouplingStreamReader.timeout)))

    override protected def onCouplingFailed(api: AgentClient, problem: Problem) =
      promiseTask[Completed] {
        promise => self ! Internal.OnCouplingFailed(problem, promise)
      }

    override protected def onCoupled(api: AgentClient, after: EventId) =
      promiseTask[Completed] { promise =>
        logger.info(s"Coupled with $api after=${EventId.toString(after)}")
        self ! Internal.OnCoupled(promise)
      }

    override protected def logEvent(stampedEvent: Stamped[AnyKeyedEvent]) =
      AgentDriver.this.logEvent(stampedEvent)

    protected def stopRequested = false
  }

  private val commandQueue = new CommandQueue(logger, batchSize = conf.commandBatchSize) {
    protected def commandParallelism = conf.commandParallelism

    protected def executeCommand(command: AgentCommand.Batch) =
      for {
        _ <- Task.sleep(delayCommandExecutionAfterErrorUntil.timeLeft)
        client <- eventFetcher.coupledApi  // wait until coupled
        response <- client.commandExecute(command)
      } yield response

    protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) =
      self ! Internal.BatchSucceeded(queuedInputResponses)

    protected def asyncOnBatchFailed(inputs: Vector[Queueable], problem: Problem) = {
      if (problem == InvalidSessionTokenProblem) {
        // Force recoupling with new login
        eventFetcher.invalidateCoupledApi
          .foreach { _ =>  // Asynchronous
            currentFetchedFuture foreach (_.cancel())
          }
      }
      self ! Internal.BatchFailed(inputs, problem)
    }
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
    keepEventsCancelable foreach (_.cancel())
    super.postStop()
  }

  private def newAgentClient(uri: Uri): AgentClient =
    AgentClient(uri, masterConfiguration.keyStoreRefOption, masterConfiguration.trustStoreRefOption)(context.system)

  def receive = {
    case input: Input with Queueable if sender() == context.parent && !isTerminating =>
      commandQueue.enqueue(input)
      scheduler.scheduleOnce(conf.commandBatchDelay) {
        self ! Internal.CommandQueueReady  // (Even with commandBatchDelay == 0) delay maySend() such that Queueable pending in actor's mailbox can be queued
      }

    case Input.ChangeUri(uri) =>
      if (uri != client.baseUri) {
        logger.debug(s"ChangeUri $uri")
        (cancelObservationAndAwaitTermination >>
          eventFetcher.decouple >>
          Task { self ! Internal.UriChanged(uri) }
        ).runToFuture
          .onComplete {
            case Success(_) =>
            case Failure(t) => logger.error(t.toStringWithCauses)
          }
      }

    case Internal.UriChanged(uri) =>
      client.close()
      logger.debug(s"new AgentClient($uri)")
      client = newAgentClient(uri)
      self ! Internal.FetchEvents

    case Input.Terminate =>
      // Wait until all pending Agent commands are responded, and do not accept further commands
      logger.debug("Terminate")
      isTerminating = true
      currentFetchedFuture foreach (_.cancel())
      commandQueue.terminate()
      eventFetcher.terminate.runAsyncAndForget  // Rejects current commands waiting for coupling
      stopIfTerminated()

    case Input.StartFetchingEvents | Internal.FetchEvents =>
      assert(currentFetchedFuture.isEmpty, "Duplicate fetchEvents")
      currentFetchedFuture = Some(
        observeAndConsumeEventsUntilCanceled
          .executeWithOptions(_.enableAutoCancelableRunLoops)
          .onCancelRaiseError(CanceledMarker)
          .runToFuture
          .andThen {
            case tried => self ! Internal.FetchFinished(tried)
          })

    case Internal.OnCoupled(promise) =>
      lastProblem = None
      delayNextKeepEvents = false
      commandQueue.onCoupled()
      commandQueue.maySend()
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
          persist(MasterAgentEvent.AgentCouplingFailed(problem), async = true) { _ =>
            Completed
          }
        })

    case Internal.FetchedEvents(stampedEvents, promise) =>
      assert(stampedEvents.nonEmpty)
      val newStampedEvents = stampedEvents dropWhile { stamped =>
        val drop = stamped.eventId <= lastEventId
        if (drop) logger.debug(s"Dropped duplicate received event: $stamped")
        drop
      }
      promiseFuture[Completed] { p =>
        context.parent ! Output.EventsFromAgent(newStampedEvents, p)
      } onComplete {
        case Success(Completed) =>
          self ! Internal.EventsAccepted(stampedEvents.last.eventId, promise)
        case o => promise.complete(o)
      }

    case Internal.EventsAccepted(after, promise) =>
      lastEventId = after
      if (keepEventsCancelable.isEmpty) {
        val delay = if (delayNextKeepEvents) conf.keepEventsPeriod else Duration.Zero
        keepEventsCancelable = Some(scheduler.scheduleOnce(delay) {
          self ! Internal.KeepEvents(after)
        })
        delayNextKeepEvents = true
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
          tried match {
            case Failure(CanceledMarker) =>  // Internal.UriChanged handles this case
            case _ => self ! Internal.FetchEvents
          }
        }

    case Internal.KeepEvents(agentEventId) =>
      if (!isTerminating) {
        commandQueue.enqueue(KeepEventsQueueable(agentEventId))
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

      val canceledOrderIds = succeededInputs collect { case o: Input.CancelOrder => o.orderId }
      if (canceledOrderIds.nonEmpty) {
        context.parent ! Output.OrdersCancelationMarked(canceledOrderIds.toSet)
      }

      val keepEvents = succeededInputs collect { case o: KeepEventsQueueable => o }
      if (keepEvents.nonEmpty) {
        keepEventsCancelable foreach (_.cancel())
        keepEventsCancelable = None
      }
      stopIfTerminated()

    case Internal.BatchFailed(inputs, problem) =>
      if (problem == RecouplingStreamReader.TerminatedProblem) {
        logger.debug(s"Command batch failed: $problem")
      } else {
        logger.warn(s"Command batch failed: $problem")
      }
      delayCommandExecutionAfterErrorUntil = now + conf.commandErrorDelay
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
            logger.warn(t.toStringWithCauses)
            Completed
          }
    }

  protected def observeAndConsumeEventsUntilCanceled: Task[Completed] =
    eventFetcher.observe(client, after = lastEventId)
      //.map { a => logEvent(a); a }
      .bufferTimedAndCounted(
        conf.eventBufferDelay max conf.commitDelay,
        maxCount = conf.eventBufferLimit)  // ticks
      .filter(_.nonEmpty)   // Ignore empty ticks
      .map(_.toImmutableSeq)
      .mapEval(stampedEvents =>
        promiseTask[Completed] { promise =>
          self ! Internal.FetchedEvents(stampedEvents, promise)
        })
      .completedL
      .map(_ => Completed)

  private def registerAsMasterIfNeeded: Task[Completed] =
    if (agentRunIdOnce.nonEmpty)
      Task.pure(Completed)
    else {
      val parent = context.parent
      client.commandExecute(AgentCommand.RegisterAsMaster)
        .map(_.map(_.agentRunId).orThrowWithoutStacktrace/*TODO Safe original Problem, package in special ProblemException and let something like Problem.pure don't _wrap_ the Problem?*/)
        .flatMap(agentRunId =>
          persistTask(AgentRegisteredMaster(agentRunId)) { _ =>
            // asynchronous
            agentRunIdOnce := agentRunId
            parent ! Output.RegisteredAtAgent(agentRunId)
            Completed
          })
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

  def props(agentRefPath: AgentRefPath, uri: Uri, agentRunId: Option[AgentRunId], eventId: EventId,
    agentDriverConfiguration: AgentDriverConfiguration, masterConfiguration: MasterConfiguration,
    journalActor: ActorRef)(implicit s: Scheduler)
  =
    Props { new AgentDriver(agentRefPath, uri, agentRunId, eventId, agentDriverConfiguration, masterConfiguration, journalActor) }

  sealed trait Queueable extends Input {
    def toShortString = toString
  }

  private[agent] final case class KeepEventsQueueable(agentEventId: EventId) extends Queueable

  sealed trait Input
  object Input {
    case object StartFetchingEvents

    final case class ChangeUri(uri: Uri)

    final case class AttachOrder(order: Order[Order.FreshOrReady], agentRefPath: AgentRefPath, signedWorkflow: Signed[Workflow]) extends Input with Queueable {
      def orderId = order.id
      override def toShortString = s"AttachOrder($orderId)"
    }

    final case class DetachOrder(orderId: OrderId) extends Input with Queueable

    final case class CancelOrder(orderId: OrderId, mode: CancelMode) extends Input with Queueable

    case object Terminate
  }

  object Output {
    final case class RegisteredAtAgent(agentRunId: AgentRunId)
    final case class EventsFromAgent(stamped: Seq[Stamped[AnyKeyedEvent]], promise: Promise[Completed])
    final case class OrdersDetached(orderIds: Set[OrderId])
    final case class OrdersCancelationMarked(orderIds: Set[OrderId])
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
    final case class OnCoupled(promise: Promise[Completed]) extends DeadLetterSuppression
    final case class EventsAccepted(agentEventId: EventId, promise: Promise[Completed]) extends DeadLetterSuppression
    final case class KeepEvents(agentEventId: EventId) extends DeadLetterSuppression
    final case class UriChanged(uri: Uri) extends DeadLetterSuppression
  }

  private object CanceledMarker extends Exception with NoStackTrace
}
