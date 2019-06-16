package com.sos.jobscheduler.master.agent

import akka.actor.{ActorRef, DeadLetterSuppression, Props}
import akka.http.scaladsl.model.Uri
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.flatMap._
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.event.AgentMasterEvent
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.Problems.InvalidSessionTokenProblem
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.common.akkautils.ReceiveLoggingActor
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
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
import com.sos.jobscheduler.master.data.agent.AgentSnapshot
import com.sos.jobscheduler.master.data.events.MasterAgentEvent
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentRegisteredMaster
import com.typesafe.config.ConfigUtil
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import scala.collection.immutable.Seq
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.Success

/**
  * Couples to an Agent, sends orders, and fetches events.
  *
  * @author Joacim Zschimmer
  */
final class AgentDriver private(agentRefPath: AgentRefPath,
  initialUri: Uri, initialAgentRunId: Option[AgentRunId], initialEventId: EventId,
  conf: AgentDriverConfiguration, masterConfiguration: MasterConfiguration,
  protected val journalActor: ActorRef)
  (implicit scheduler: Scheduler)
extends KeyedJournalingActor[MasterAgentEvent]
with ReceiveLoggingActor.WithStash
{
  private val logger = Logger.withPrefix[AgentDriver](agentRefPath.string)
  private val agentUserAndPassword: Option[UserAndPassword] =
    masterConfiguration.config.optionAs[SecretString]("jobscheduler.auth.agents." + ConfigUtil.joinPath(agentRefPath.string))
      .map(password => UserAndPassword(masterConfiguration.masterId.toUserId, password))

  private var uri = initialUri
  private val agentRunIdOnce = SetOnce.fromOption(initialAgentRunId)
  private var client = newAgentClient()
  /** Only filled when coupled */
  private val coupledClient = MVar.empty[Task, AgentClient]().runSyncUnsafe()
  private val recouplePause = new RecouplingPause
  private var lastEventId = initialEventId
  private var lastFetchAt = now - 1.hour
  private var lastProblem: Option[Problem] = None
  private var fetchEventsCancelable: Option[Cancelable] = None
  private var keepEventsCancelable: Option[Cancelable] = None
  private var delayNextKeepEvents = false
  private var delayCommandExecutionAfterErrorUntil = now

  private val commandQueue = new CommandQueue(logger, batchSize = conf.commandBatchSize) {
    protected def commandParallelism = conf.commandParallelism

    protected def executeCommand(command: AgentCommand.Batch) =
      for {
        _ <- Task.sleep(delayCommandExecutionAfterErrorUntil.timeLeft)
        client <- coupledClient.read  // wait until coupled
        response <- client.commandExecute(command)
      } yield response

    protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) =
      self ! Internal.BatchSucceeded(queuedInputResponses)

    protected def asyncOnBatchFailed(inputs: Vector[Queueable], problem: Problem) =
      self ! Internal.BatchFailed(inputs, problem)
  }

  protected def key = agentRefPath  // Only one version is active at any time
  protected def recoverFromSnapshot(snapshot: Any) = throw new NotImplementedError
  protected def recoverFromEvent(event: MasterAgentEvent) = throw new NotImplementedError
  protected def snapshot = Some(AgentSnapshot(agentRefPath, agentRunIdOnce.toOption, lastEventId))

  override def preStart() = {
    super.preStart()
    self ! Internal.FetchEvents
  }

  override def postStop() = {
    // Close coupledClient to terminate waiting commands ???
    coupledClient.tryTake.runSyncUnsafe()
    client.logout()
      .materialize  // Ignore errors
      .guarantee(Task(client.close()))
      .runAsyncAndForget
    fetchEventsCancelable foreach (_.cancel())
    keepEventsCancelable foreach (_.cancel())
    super.postStop()
  }

  private def newAgentClient(): AgentClient =
    AgentClient(uri, masterConfiguration.keyStoreRefOption, masterConfiguration.trustStoreRefOption)(context.system)

  def receive: Receive = {
    case input: Input with Queueable if sender() == context.parent =>
      commandQueue.enqueue(input)
      scheduler.scheduleOnce(conf.commandBatchDelay) {
        self ! Internal.CommandQueueReady  // (Even with commandBatchDelay == 0) delay maySend() such that Queueable pending in actor's mailbox can be queued
      }

    case Input.ChangeUri(uri_) =>
      uri = uri_
      if (uri != client.baseUri) {
        fetchEventsCancelable foreach (_.cancel())
      }

    case Input.Terminate => context.stop(self)

    case Internal.FetchEvents =>
      assert(fetchEventsCancelable.isEmpty, "Duplicate fetchEvents")
      val future = startFetchingEventsFromUri().runToFuture
      fetchEventsCancelable = Some(future)

    case Internal.OnCoupled(promise) =>
      logger.info(s"Coupled with ${client.baseUri}")
      lastProblem = None
      recouplePause.onCouplingSucceeded()
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
          persist(MasterAgentEvent.AgentCouplingFailed(problem)) { _ =>
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
      promiseFuture[Checked[Completed]] { p =>
        context.parent ! Output.EventsFromAgent(newStampedEvents, p)
      } onComplete {
        case Success(Valid(Completed)) =>
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
      promise.success(Valid(Completed))

    case Internal.FetchFinished(checkedCompleted) =>
      fetchEventsCancelable = None
      val delay = checkedCompleted match {
        case Valid(Completed) | Invalid(CanceledProblem) =>
          logger.debug(s"FetchFinished $checkedCompleted")
          conf.eventFetchDelay - lastFetchAt.elapsed
        case Invalid(problem) =>
          logger.warn(s"Error when fetching events: $problem")
          recouplePause.nextPause()
      }
      (coupledClient.tryTake/*force recoupling*/ >>
        Task.sleep(delay) >>
        Task { self ! Internal.FetchEvents }
      ).runAsyncAndForget

    case Internal.KeepEvents(agentEventId) =>
      commandQueue.enqueue(KeepEventsQueueable(agentEventId))

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

    case Internal.BatchFailed(inputs, throwable) =>
      logger.warn(s"Command batch failed: $throwable")
      delayCommandExecutionAfterErrorUntil = now + conf.commandErrorDelay
      commandQueue.handleBatchFailed(inputs)
  }

  private def startFetchingEventsFromUri(): Task[Completed.type] = {
    val loop = Task.tailRecM(lastEventId)(after =>
      coupleIfNeeded >>
        fetchEvents(after = after)
          .flatMap {
            case Valid(Completed) => Task.pure(Left(lastEventId))
            case Invalid(InvalidSessionTokenProblem) =>
              coupledClient.tryTake >>
              client.logout().materialize.delayResult(recouplePause.nextPause()) >>
                Task.pure(Left(lastEventId))
            case Invalid(problem) =>
              Task.pure(Right(Invalid(problem)))
          })
    loop
      .executeWithOptions(_.enableAutoCancelableRunLoops)
      .doOnCancel(Task {
        logger.debug("Fetching canceled")
        self ! Internal.FetchFinished(Invalid(CanceledProblem))
      })
      .materialize.map(o => Checked.fromTry(o).flatten)
      .map { checked =>
        self ! Internal.FetchFinished(checked)
        Completed
      }
  }

  private def coupleIfNeeded: Task[Completed] =
    changeClientUriIfNeeded >>
      coupledClient.tryRead.flatMap {  // Already coupled?
        case Some(_) => Task.pure(Completed)
        case None => couple
      }

  private def changeClientUriIfNeeded: Task[Unit] =
    if (client.baseUri == uri)
      Task.unit
    else
      coupledClient.tryTake/*force recoupling*/ >>
        client.logout().materialize >>
          Task {
            client.close()
            client = newAgentClient()
            logger.debug(s"new AgentClient($uri)")
          }

  private def couple: Task[Completed] =
    Task.tailRecM(())(_ =>
      (for {
        otherCoupledClient <- coupledClient.tryRead
        _ <- if (otherCoupledClient.isEmpty) Task.unit else Task.raiseError(new IllegalStateException("Coupling while already coupled"))
        _ <- Task { recouplePause.onCouple() }
        _ <- client.login(agentUserAndPassword)
        _ <- registerAsMasterIfNeeded
        checkedResponse <- client.commandExecute(AgentCommand.CoupleMaster(agentRunIdOnce(), eventId = lastEventId))
      } yield checkedResponse)
        .materialize.map(o => Checked.fromTry(o).flatten)
        .flatMap {
          case Invalid(problem) =>
            for {
              _ <- client.logout().materialize  // Ignore error
              _ <- promiseTask[Completed] { promise => self ! Internal.OnCouplingFailed(problem, promise) }
              _ <- Task.sleep(recouplePause.nextPause())
            } yield Left(())

          case Valid(AgentCommand.Response.Accepted) =>
            for {
              _ <- coupledClient.tryPut(client)
              completed <- promiseTask[Completed] { promise => self ! Internal.OnCoupled(promise) }
            } yield Right(completed)
        })

  private def registerAsMasterIfNeeded: Task[Completed] =
    if (agentRunIdOnce.isEmpty) registerAsMaster else Task.pure(Completed)

  private def registerAsMaster: Task[Completed] =
    client.commandExecute(AgentCommand.RegisterAsMaster)
      .map(_.map(_.agentRunId).orThrowWithoutStacktrace/*TODO Safe original Problem, package in special ProblemException and let something like Problem.pure don't _wrap_ the Problem?*/)
      .flatMap(agentRunId =>
        persistTask(AgentRegisteredMaster(agentRunId)) { stamped =>
          update(stamped.value.event)
          Completed
        }
      )

  private def fetchEvents(after: EventId): Task[Checked[Completed]] =
    Task { logger.trace(s"fetchEvents(after=$after)")} >>
    Task {
      lastFetchAt = now
    } >>
      client.mastersEventObservable(EventRequest[Event](EventClasses, after = after, timeout = Some(conf.eventFetchTimeout)))
        .materialize.map(o => Checked.fromTry(o).flatten)
        .flatMap(checked =>
          checked.traverse(eventObservable => consumeEvents(eventObservable)))
        .map(_.flatten)
        .executeWithOptions(_.enableAutoCancelableRunLoops)

  private def consumeEvents(eventObservable: Observable[Stamped[AnyKeyedEvent]]): Task[Checked[Completed]] =
    eventObservable
      .timeoutOnSlowUpstream(conf.eventFetchTimeout + TimeoutReserve)   // throws UpstreamTimeoutException
      .map { o => logEvent(o); o }
      .bufferTimedAndCounted(conf.eventBufferDelay max conf.commitDelay, maxCount = conf.eventBufferLimit)  // ticks
      .filter(_.nonEmpty)   // Ignore empty ticks
      .map(_.toImmutableSeq)
      .mapEval(stampedEvents =>
        promiseTask[Checked[Completed]] { promise =>
          self ! Internal.FetchedEvents(stampedEvents, promise)
        })
      .collect { case o @ Invalid(_) => o }
      .headOptionL
      .map {
        case None => Checked.completed
        case Some(checked) => checked
      }
    .guaranteeCase(exitCase => Task { logger.trace(s"Observable ended: $exitCase") })

  private object logEvent {
    private var logEventCount = 0
    def apply(stampedEvent: Stamped[AnyKeyedEvent]): Unit =
      logger.whenTraceEnabled {
        logEventCount += 1
        logger.trace(s"#$logEventCount $stampedEvent")
      }
  }

  private def update(event: MasterAgentEvent): Unit =
    event match {
      case AgentRegisteredMaster(agentRunId) =>
        agentRunIdOnce := agentRunId

      case _ => sys.error(s"Unexpected event: $event")
    }

  override def toString = s"AgentDriver($agentRefPath)"
}

private[master] object AgentDriver
{
  private val TimeoutReserve = 10.s
  private val EventClasses = Set[Class[_ <: Event]](classOf[OrderEvent], classOf[AgentMasterEvent.AgentReadyForMaster])

  def props(agentRefPath: AgentRefPath, uri: Uri, agentRunId: Option[AgentRunId], eventId: EventId,
    agentDriverConfiguration: AgentDriverConfiguration, masterConfiguration: MasterConfiguration,
    journalActor: ActorRef)(implicit s: Scheduler)
  =
    Props { new AgentDriver(agentRefPath, uri, agentRunId, eventId, agentDriverConfiguration, masterConfiguration, journalActor) }

  private class RecouplingPause {
    private val Minimum = 1.second
    private var pauses = initial
    private var lastCouplingTriedAt = now

    def onCouple() = lastCouplingTriedAt = now
    def onCouplingSucceeded() = pauses = initial
    def nextPause() = (lastCouplingTriedAt + pauses.next()).timeLeft max Minimum

    private def initial = Iterator(Minimum, 1.second, 1.second, 1.second, 2.seconds, 5.seconds) ++
      Iterator.continually(10.seconds)
  }

  sealed trait Queueable extends Input {
    def toShortString = toString
  }

  private[agent] final case class KeepEventsQueueable(agentEventId: EventId) extends Queueable

  sealed trait Input
  object Input {
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
    final case class EventsFromAgent(stamped: Seq[Stamped[AnyKeyedEvent]], promise: Promise[Checked[Completed]])
    final case class OrdersDetached(orderIds: Set[OrderId])
    final case class OrdersCancelationMarked(orderIds: Set[OrderId])
  }

  private object Internal {
    final case object CommandQueueReady extends DeadLetterSuppression
    final case class BatchSucceeded(responses: Seq[QueuedInputResponse]) extends DeadLetterSuppression
    final case class BatchFailed(inputs: Seq[Queueable], problem: Problem) extends DeadLetterSuppression
    final case object FetchEvents extends DeadLetterSuppression
    final case class FetchedEvents(events: Seq[Stamped[AnyKeyedEvent]], promise: Promise[Checked[Completed]])
      extends DeadLetterSuppression
    final case class FetchFinished(result: Checked[Completed]) extends DeadLetterSuppression
    final case class OnCouplingFailed(problem: Problem, promise: Promise[Completed]) extends DeadLetterSuppression
    final case class OnCoupled(promise: Promise[Completed]) extends DeadLetterSuppression
    final case class EventsAccepted(agentEventId: EventId, promise: Promise[Checked[Completed]]) extends DeadLetterSuppression
    final case class KeepEvents(agentEventId: EventId) extends DeadLetterSuppression
  }

  private case object CanceledProblem extends Problem.Eager("Canceled")  // Marker only
}
