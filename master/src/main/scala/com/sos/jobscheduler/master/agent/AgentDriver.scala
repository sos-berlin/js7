package com.sos.jobscheduler.master.agent

import akka.actor.{Actor, ActorRef, DeadLetterSuppression, Props}
import akka.http.scaladsl.model.Uri
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.flatMap._
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.event.AgentMasterEvent
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.akkautils.ReceiveLoggingActor
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.core.event.journal.KeyedJournalingActor
import com.sos.jobscheduler.data.agent.{AgentRefPath, AgentRunId}
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.crypt.Signed
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.agent.AgentDriver._
import com.sos.jobscheduler.master.agent.CommandQueue.QueuedInputResponse
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.agent.AgentSnapshot
import com.sos.jobscheduler.master.data.events.MasterAgentEvent
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentRegisteredMaster
import com.typesafe.config.ConfigUtil
import monix.eval.Task
import monix.execution.atomic.AtomicInt
import monix.execution.{Cancelable, Scheduler}
import scala.collection.immutable.Seq
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

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
  private val config = masterConfiguration.config
  private val authConfigPath = "jobscheduler.auth.agents." + ConfigUtil.joinPath(agentRefPath.string)
  private val recouplePause = new RecouplingPause
  private var uri = initialUri
  private var changedUri: Uri = null
  private var client: AgentClient = null

  private val agentUserAndPassword: Option[UserAndPassword] =
    config.optionAs[SecretString](authConfigPath)
      .map(password => UserAndPassword(masterConfiguration.masterId.toUserId, password))

  private val agentRunIdOnce = SetOnce[AgentRunId]
  initialAgentRunId foreach agentRunIdOnce.:=

  private var lastEventId = initialEventId
  private var startInputReceived = false
  private var isCoupled = false
  private var couplingNumber = 0L
  private var isFetchingEvents = false
  @volatile
  private var keepEventsCancelable: Option[Cancelable] = None
  private var delayKeepEvents = false
  private val logEventCount = AtomicInt(0)
  private var lastFetchAt = now - conf.eventFetchDelay - 1.hour
  private var lastError: Option[(Long, Problem)] = None

  private val commandQueue = new CommandQueue(logger, batchSize = conf.batchSize) {
    protected def executeCommand(command: AgentCommand.Batch) =
      client.commandExecute(command)

    protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) =
      self ! Internal.BatchSucceeded(queuedInputResponses)

    protected def asyncOnBatchFailed(inputs: Vector[Queueable], problem: Problem) =
      self ! Internal.BatchFailed(inputs, problem)
  }

  protected def key = agentRefPath  // Only one version is active at any time
  protected def recoverFromSnapshot(snapshot: Any) = {}
  protected def recoverFromEvent(event: MasterAgentEvent) = {}

  protected def snapshot = Some(AgentSnapshot(agentRefPath, agentRunIdOnce.toOption, lastEventId))

  override def preStart() = {
    super.preStart()
    become("decoupled")(decoupled)
    self ! Internal.Couple
  }

  override def postStop() = {
    if (client != null) {
      client.logout()
        .guarantee(Task(client.close()))
        .runAsyncAndForget
    }
    keepEventsCancelable foreach (_.cancel())
    super.postStop()
  }

  def receive = Actor.emptyBehavior

  private def decoupled: Receive = {
    case Internal.Couple =>
      delayKeepEvents = false
      recouplePause.onCouple()
      if (client == null || client.baseUri != uri) {
        if (client != null) client.close()
        client = AgentClient(uri, masterConfiguration.keyStoreRefOption, masterConfiguration.trustStoreRefOption)(context.system)
      }
      agentRunIdOnce.toOption match {
        case None => registerMaster()
        case Some(agentRunId) => coupleMaster(agentRunId)
      }

    case Input.Reconnect(uri_) =>
      uri = uri_

    case _ =>
      stash()
  }

  private def registerMaster(): Unit = {
    if (lastEventId != EventId.BeforeFirst) {
      sys.error(s"Agent coupling internal error: No AgentRunId but lastEventId=$lastEventId")
    }
    unstashAll()
    become("registering")(registering)
    (client.login(agentUserAndPassword) >>
     client.commandExecute(AgentCommand.RegisterAsMaster))
      .map(_.map(_.agentRunId))
      .materialize
      .foreach { tried =>
        self ! Internal.AfterMasterRegistering(Checked.fromTry(tried).flatten)
      }
  }

  private def registering: Receive = {
    case Internal.AfterMasterRegistering(Valid(agentRunId)) =>
      unstashAll()
      become("coupling")(coupling)
      persist(AgentRegisteredMaster(agentRunId)) { event =>
        update(event)
        self ! Internal.AfterCoupling(Valid(Completed))
      }

    case Internal.AfterMasterRegistering(Invalid(problem)) =>
      handleError(problem) {
        recouple()
      }

    case _ =>
      stash()
  }

  private def coupleMaster(agentRunId: AgentRunId): Unit = {
    unstashAll()
    become("coupling")(coupling)
    (client.login(agentUserAndPassword) >>
     client.commandExecute(AgentCommand.CoupleMaster(agentRunId, eventId = lastEventId)))
      .materialize
      .foreach { tried =>
        self ! Internal.AfterCoupling(
          Checked.fromTry(tried).flatten.map((_: AgentCommand.Response.Accepted) => Completed))
      }
  }

  private def coupling: Receive = {
    case Internal.AfterCoupling(Valid(Completed)) =>
      lastError = None
      isCoupled = true
      recouplePause.onCouplingSucceeded()
      commandQueue.onRecoupled()
      logger.info(s"Coupled with ${client.baseUri}")
      unstashAll()
      become("waitingForStart")(waitingForStart)
      if (startInputReceived) {
        self ! Input.Start
      }

    case Internal.AfterCoupling(Invalid(problem)) =>
      handleError(problem) {
        recouple()
      }

    case _ =>
      stash()
  }

  private def waitingForStart: Receive = {
    case Input.Start =>
      startInputReceived = true
      commandQueue.maySend()
      unstashAll()
      become("ready")(ready)
      self ! Internal.FetchEvents(couplingNumber)

    case _ =>
      stash()
  }

  private def ready: Receive = {
    case Input.Reconnect(uri_) =>
      changedUri = uri_

    case Internal.FetchEvents(fetchCouplingNumber) =>
      if (changedUri != null && changedUri != uri) {
        uri = changedUri
        changedUri = null
        recouple(immediately = true)
      } else if (!isCoupled || fetchCouplingNumber != couplingNumber || isFetchingEvents) {
        logger.debug(s"Discarding FetchEvents due to isCoupled=$isCoupled or (fetchCouplingNumber=$fetchCouplingNumber but coulingNumber=$couplingNumber) or isFetchingEvents=$isFetchingEvents")
      } else {
        isFetchingEvents = true
        val after = lastEventId
        client.mastersEvents(EventRequest[Event](EventClasses, after = after, Some(conf.eventFetchTimeout), limit = conf.eventLimit))
          .materialize foreach { tried =>
            // Asynchronous
            self ! Internal.Fetched(Checked.fromTry(tried).flatten, after, fetchCouplingNumber)
          }
      }

    case Internal.KeepEventsDelayed(agentEventId) =>
      if (isCoupled) {
        commandQueue.enqueue(KeepEventsQueueable(agentEventId))
      }

    case Internal.CommandQueueReady =>
      commandQueue.maySend()
  }

  override protected def become(state: String)(recv: Receive) =
    super.become(state)(handleStandardMessage orElse recv orElse handleOtherMessage)

  private def handleStandardMessage: Receive = {
    case Input.Terminate =>
      // TODO HTTP-Anfragen abbrechen und Antwort mit discardBytes() verwerfen, um folgende Akka-Warnungen zu vermeiden
      // WARN  akka.http.impl.engine.client.PoolGateway - [0 (WaitingForResponseEntitySubscription)] LoggedIn entity was not subscribed after 1 second. Make sure to read the response entity body or call `discardBytes()` on it. GET /agent/api/master/event Empty -> 200 OK Chunked
      context.stop(self)
  }

  private def handleOtherMessage: Receive = {
    case input: Input with Queueable if sender() == context.parent =>
      commandQueue.enqueue(input)
      scheduler.scheduleOnce(conf.batchDelay) {
        self ! Internal.CommandQueueReady  // (Even with batchDelay == 0) delay maySend() such that Queueable pending in actor's mailbox can be queued
      }

    case Input.EventsAccepted(after) =>
      assert(after == lastEventId, s"Input.EventsAccepted($after) ≠ lastEventId=$lastEventId ?")
      if (isCoupled/*???*/) {
        if (keepEventsCancelable.isEmpty) {
          val delay = if (delayKeepEvents) conf.keepEventsPeriod else Duration.Zero
          delayKeepEvents = true
          keepEventsCancelable = Some(
            scheduler.scheduleOnce(delay) {
              self ! Internal.KeepEventsDelayed(after)
            })
        }
        scheduler.scheduleOnce(conf.eventFetchDelay) {
          self ! Internal.FetchEvents(couplingNumber)
        }
      }

    case Internal.BatchSucceeded(responses) =>
      lastError = None
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
      commandQueue.handleBatchFailed(inputs)
      handleError(throwable) {
        if (isCoupled) {
          recouple()
          //throwable match {
          //  case t: AkkaHttpClient.HttpException =>
          //    handleError(t)
          //
          //  case t =>
          //    handleError(t)
          //    // TODO Retry several times, finally inform commander about failure (dropping the order)
          //    // 1) Wenn es ein Verbindungsfehler ist (oder ein HTTP-Fehlercode eines Proxys), dann versuchen wir endlos weiter.
          //    // Der Verbindungsfehler kann die eine TCP-Verbindung betreffen. Dann genügt ein neuer Versuch.
          //    // Wenn wir einige (zwei?) Fehler in Folge ohne Erfolg zwischendurch erhalten, dann unterbrechen wir die Verbindung zum Agenten
          //    // für kurze Zeit, bevor wir sie wiederaufleben lassen.
          //    // Diesen Mechanismus haben wir schon.
          //    // 2) akka.stream.BufferOverflowException: Später wieder versuchen
          //    // 3) Mehrere Inputs (alle in inputQueue, doch limitiert) gebündelt dem Agenten schicken,
          //    // Agent antwortet mit entsprechened vielen Okays oder Fehlermeldungen
          //    // 4) Prüfen, ob akka.http.host-connection-pool.client.max-retries von 0 auf 5 (die Voreinstellung) erhöht werden kann.
          //}
        }
      }

    case Internal.CommandQueueReady =>

    case Internal.Fetched(Invalid(problem), after, fetchCouplingNumber) =>
      if (fetchCouplingNumber != couplingNumber) {
        logger.debug(s"Discarding obsolete Agent response Internal.Fetched(after=$after, $problem)")
      } else if (isCoupled) {
        isFetchingEvents = false
        handleError(problem) {
          recouple()
        }
      }

    case Internal.Fetched(Valid(eventSeq), after, fetchCouplingNumber) =>
      if (fetchCouplingNumber != couplingNumber) {
        logger.debug(s"Discarding obsolete Agent response Internal.Fetched(after=$after)")
      } else {
        isFetchingEvents = false
        eventSeq match {
          case EventSeq.NonEmpty(stampedEvents) =>
            lastError = None
            logger.whenTraceEnabled { for (stamped <- stampedEvents) { logEventCount += 1; logger.trace(s"#$logEventCount $stamped") } }

            context.parent ! Output.EventsFromAgent(stampedEvents)  // TODO Possible OutOfMemoryError. Use reactive stream or acknowledge

            for (last <- stampedEvents.lastOption) {
              assert(lastEventId < last.eventId, s"last.eventId=${last.eventId} <= lastEventId=$lastEventId ?")
              lastEventId = last.eventId
            }

          case EventSeq.Empty(lastEventId_) =>  // No events after timeout
            if (isCoupled) scheduler.scheduleOnce(conf.eventTimeoutDelay) {
              assert(lastEventId <= lastEventId_)
              lastEventId = lastEventId_
              self ! Internal.FetchEvents(couplingNumber)
            }

          case torn: TearableEventSeq.Torn =>
            val problem = AgentLostEventsProblem(agentRefPath, uri = client.baseUri.toString, torn, requestingAfter = after)
            logger.error(problem.toString)
            persist(MasterAgentEvent.AgentCouplingFailed(problem)) { _ =>
              if (isCoupled) scheduler.scheduleOnce(15.second) {
                self ! Internal.FetchEvents(couplingNumber)
              }
            }
        }
      }
  }

  private def handleError(problem: Problem)(andThen: => Unit): Unit = {
    if (/*?lastError.forall(_._1 == couplingNumber) &&*/ lastError.forall(_._2 != problem)) {
      lastError = Some(couplingNumber -> problem)
      logger.warn(problem.toString)
      for (t <- problem.throwableOption if t.getStackTrace.nonEmpty) logger.debug(problem.toString, t)
      persist(MasterAgentEvent.AgentCouplingFailed(problem)) { _ =>
        andThen
      }
    } else {
      logger.debug(problem.toString)
      andThen
    }
  }

  private def recouple(immediately: Boolean = false) = {
    keepEventsCancelable foreach (_.cancel())
    keepEventsCancelable = None
    isCoupled = false
    couplingNumber += 1
    isFetchingEvents = false  // A pending GET will be ignored due to different couplingNumber
    client.logout().materialize foreach { _ =>
      self ! Internal.MaybeLoggedOut  // We ignore an error due to unknown SessionToken (because Agent may have been restarted) or Agent shutdown
    }
    become("MaybeLoggedOut") {
      case Internal.MaybeLoggedOut =>
        val delay = if (immediately) Duration.Zero else recouplePause.nextPause()
        scheduler.scheduleOnce(delay) {
          self ! Internal.Couple
        }
        unstashAll()
        become("decoupled")(decoupled)

      case _ =>
        stash()
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
  private val EventClasses = Set[Class[_ <: Event]](classOf[OrderEvent], classOf[AgentMasterEvent.AgentReadyForMaster])

  def props(agentRefPath: AgentRefPath, uri: Uri, agentRunId: Option[AgentRunId], eventId: EventId,
    agentDriverConfiguration: AgentDriverConfiguration, masterConfiguration: MasterConfiguration,
    journalActor: ActorRef)(implicit s: Scheduler)
  =
    Props { new AgentDriver(agentRefPath, uri, agentRunId, eventId, agentDriverConfiguration, masterConfiguration, journalActor) }

  private class RecouplingPause {
    private val Minimum = 1.second
    private var pauses = initial
    private var lastCoupling = now

    def onCouple() = lastCoupling = now
    def onCouplingSucceeded() = pauses = initial
    def nextPause() = (lastCoupling + pauses.next()).timeLeft max Minimum

    private def initial = Iterator(Minimum, 1.second, 1.second, 1.second, 2.seconds, 5.seconds) ++
      Iterator.continually(10.seconds)
  }

  sealed trait Queueable extends Input {
    def toShortString = toString
  }

  private[agent] final case class KeepEventsQueueable(agentEventId: EventId) extends Queueable

  sealed trait Input
  object Input {
    final case class Reconnect(uri: Uri)

    case object Start

    final case class AttachOrder(order: Order[Order.FreshOrReady], agentRefPath: AgentRefPath, signedWorkflow: Signed[Workflow]) extends Input with Queueable {
      def orderId = order.id
      override def toShortString = s"AttachOrder($orderId)"
    }

    final case class DetachOrder(orderId: OrderId) extends Input with Queueable

    final case class CancelOrder(orderId: OrderId, mode: CancelMode) extends Input with Queueable

    final case class EventsAccepted(agentEventId: EventId)

    case object Terminate
  }

  object Output {
    final case class EventsFromAgent(stamped: Seq[Stamped[AnyKeyedEvent]])
    final case class OrdersDetached(orderIds: Set[OrderId])
    final case class OrdersCancelationMarked(orderIds: Set[OrderId])
  }

  private object Internal {
    final case object Couple
    final case class AfterMasterRegistering(result: Checked[AgentRunId]) extends DeadLetterSuppression
    final case class AfterCoupling(result: Checked[Completed]) extends DeadLetterSuppression
    final case object MaybeLoggedOut extends DeadLetterSuppression
    final case object Ready
    final case object CommandQueueReady extends DeadLetterSuppression
    final case class BatchSucceeded(responses: Seq[QueuedInputResponse]) extends DeadLetterSuppression
    final case class BatchFailed(inputs: Seq[Queueable], problem: Problem) extends DeadLetterSuppression
    final case class FetchEvents(couplingNumber: Long) extends DeadLetterSuppression/*TODO Besser: Antwort empfangen und mit discardBytes() verwerfen, um Akka-Warnung zu vermeiden*/
    final case class Fetched(stampedChecked: Checked[TearableEventSeq[Seq, KeyedEvent[Event]]], after: EventId, couplingNumber: Long) extends DeadLetterSuppression
    final case class KeepEventsDelayed(agentEventId: EventId) extends DeadLetterSuppression
  }

  private final case class AgentLostEventsProblem(agentRefPath: AgentRefPath, uri: String, torn: TearableEventSeq.Torn,
    requestingAfter: EventId) extends Problem.Coded
  {
    def arguments = Map(
      "agentRefPath" -> agentRefPath.string,
      "uri" -> uri,
      "torn" -> torn.toString,
      "requestingAfter" -> EventId.toString(requestingAfter))
  }
}
