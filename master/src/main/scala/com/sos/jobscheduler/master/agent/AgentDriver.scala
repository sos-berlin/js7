package com.sos.jobscheduler.master.agent

import akka.actor.{Actor, ActorRef, DeadLetterSuppression, PoisonPill, Props}
import akka.http.scaladsl.model.Uri
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.event.AgentMasterEvent
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.problem.Checked.CheckedOption
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.akkautils.ReceiveLoggingActor
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.Logger.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.event.journal.KeyedJournalingActor
import com.sos.jobscheduler.data.agent.AgentId
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.agent.AgentDriver._
import com.sos.jobscheduler.master.agent.CommandQueue.QueuedInputResponse
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.events.MasterAgentEvent
import com.typesafe.config.ConfigUtil
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * Keeps connection to an Agent, sends orders, and fetches events.
  *
  * @author Joacim Zschimmer
  */
final class AgentDriver private(agentId: AgentId, uri: Uri, masterConfiguration: MasterConfiguration, protected val journalActor: ActorRef)
  (implicit scheduler: Scheduler)
extends KeyedJournalingActor[MasterAgentEvent]
with ReceiveLoggingActor.WithStash {

  private val logger = Logger.withPrefix[AgentDriver](agentId.toSimpleString + " " + uri)
  private val config = masterConfiguration.config
  private val batchSize         = config.getInt     ("jobscheduler.master.agent-driver.command-batch-size")
  private val batchDelay        = config.getDuration("jobscheduler.master.agent-driver.command-batch-delay").toFiniteDuration
  private val eventFetchTimeout = config.getDuration("jobscheduler.master.agent-driver.event-fetch-timeout").toFiniteDuration
  private val eventFetchDelay   = config.getDuration("jobscheduler.master.agent-driver.event-fetch-delay").toFiniteDuration
  private val keepEventsPeriod  = config.getDuration("jobscheduler.master.agent-driver.keep-events-period").toFiniteDuration
  private val authConfigPath = "jobscheduler.auth.agents." + ConfigUtil.joinPath(agentId.path.string)
  private val reconnectPause = new ReconnectPause
  private val client = AgentClient(uri, masterConfiguration.keyStoreRefOption, masterConfiguration.trustStoreRefOption)(context.system)

  private val agentUserAndPassword: Checked[UserAndPassword] =
    config.optionAs[SecretString](authConfigPath)
      .map(password ⇒ UserAndPassword(masterConfiguration.masterId.toUserId, password))
      .toChecked(Problem(s"Missing password for '${agentId.path}', no configuration entry for $authConfigPath"))

  private var startInputReceived = false
  private var isConnected = false
  private var isAwaitingEventResponse = false
  private var lastEventId = EventId.BeforeFirst
  @volatile
  private var keepEventsCancelable: Option[Cancelable] = None
  private var delayKeepEvents = false
  private var logCount = 0
  private var terminating = false

  become("disconnected")(disconnected)
  self ! Internal.Connect

  private val commandQueue = new CommandQueue(logger, batchSize = batchSize) {
    protected def executeCommand(command: AgentCommand.Batch) =
      client.commandExecute(command)

    protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) =
      self ! Internal.BatchSucceeded(queuedInputResponses)

    protected def asyncOnBatchFailed(inputs: Vector[Input.QueueableInput], throwable: Throwable) =
      self ! Internal.BatchFailed(inputs, throwable)
  }

  protected def key = agentId.path  // Only one version is active at any time
  protected def recoverFromSnapshot(snapshot: Any) = {}
  protected def recoverFromEvent(event: MasterAgentEvent) = {}
  protected def snapshot = None

  override def postStop() = {
    client.logout().runAsync onComplete {
      _ ⇒ client.close()  // Don't await response
    }
    keepEventsCancelable foreach (_.cancel())
    super.postStop()
  }

  def receive = Actor.emptyBehavior

  private def disconnected: Receive = {
    case Internal.Connect ⇒
      delayKeepEvents = false
      reconnectPause.onConnect()
      agentUserAndPassword match {
        case Invalid(problem) ⇒
          logger.error(problem.toString)
          // Actor freezes here. TODO 1) reflect state in web service /api/agent; 2) publish an event to notify about this error

        case Valid(userAndPassword) ⇒
          ( for {
              _ ← client.login(Some(userAndPassword))  // Separate commands because AgentClient catches the SessionToken of Login.Response
              _ ← if (lastEventId == EventId.BeforeFirst)
                    client.commandExecute(AgentCommand.RegisterAsMaster)
                  else
                    Task.pure(Completed)
            } yield Completed
          ).runAsync.onComplete { tried ⇒
            self ! Internal.AfterConnect(tried)
          }
          unstashAll()
          become("connecting")(connecting)
      }

    case _ ⇒
      stash()
  }

  private def connecting: Receive = {
    case Internal.AfterConnect(Success(Completed)) ⇒
      isConnected = true
      reconnectPause.onConnectSucceeded()
      commandQueue.onReconnected()
      unstashAll()
      become("waitingForStart")(waitingForStart)
      if (startInputReceived) {
        self ! Input.Start(lastEventId)
      }

    case Internal.AfterConnect(Failure(throwable)) ⇒
      handleConnectionError(throwable) {
        reconnect()
      }

    case _ ⇒
      stash()
  }

  private def waitingForStart: Receive = {
    case Input.Start(eventId) ⇒
      lastEventId = eventId
      startInputReceived = true
      logger.info(s"Fetching events after ${EventId.toString(lastEventId)}")
      commandQueue.maySend()
      unstashAll()
      become("ready")(ready)
      self ! Internal.FetchEvents(lastEventId)

    case _ ⇒
      stash()
  }

  private def ready: Receive = {
    case Internal.FetchEvents(after) if isConnected && !isAwaitingEventResponse ⇒
      if (!terminating) {
        lastEventId = after
        isAwaitingEventResponse = true
        client.mastersEvents(EventRequest[Event](EventClasses, after = after, eventFetchTimeout)).runAsync
          .andThen { case _ ⇒
            isAwaitingEventResponse = false
          }
          .onComplete {
            // Asynchronous!
            case Failure(t) ⇒
              self ! Internal.Fetched(Failure(t))

            case Success(EventSeq.Empty(lastEventId_)) ⇒  // No events after timeout
              if (isConnected) scheduler.scheduleOnce(1.second) {
                self ! Internal.FetchEvents(lastEventId_)
              }

            case Success(EventSeq.NonEmpty(stampedEvents)) ⇒
              self ! Internal.Fetched(Success(stampedEvents))

            case Success(torn: TearableEventSeq.Torn) ⇒
              val problem = Problem(s"Bad response from Agent $agentId $uri: $torn, requested events after=$after")
              logger.error(problem)
              persist(MasterAgentEvent.AgentCouplingFailed(problem.toString)) { _ ⇒
                if (isConnected) scheduler.scheduleOnce(15.second) {
                  self ! Internal.FetchEvents(after)
                }
              }
          }
        }

    case Internal.EventsAccepted(eventId) ⇒  // TODO isConnected, terminating?
      client.commandExecute(AgentCommand.KeepEvents(after = eventId)).runOnComplete { tried ⇒
        tried.failed.foreach(t ⇒ logger.warn("AgentCommand.KeepEvents failed: " + t.toStringWithCauses))
        keepEventsCancelable = None  // Asynchronous!
      }

    case Internal.CommandQueueReady ⇒
      if (!terminating) {
        commandQueue.maySend()
      }
  }

  override protected def become(state: String)(recv: Receive) =
    super.become(state)(recv orElse handleOtherMessage)

  private def handleOtherMessage: Receive = {
    case input: Input.QueueableInput if sender() == context.parent ⇒
      commandQueue.enqueue(input)
      scheduler.scheduleOnce(batchDelay) {
        self ! Internal.CommandQueueReady  // (Even with batchDelay == 0) delay maySend() such that QueueableInput pending in actor's mailbox can be queued
      }

    case Input.EventsAccepted(eventId) ⇒
      assert(eventId == lastEventId, s"Input.EventsAccepted($eventId) ≠ lastEventId=$lastEventId ?")
      if (isConnected) {
        if (keepEventsCancelable.isEmpty) {
          val delay = if (delayKeepEvents) keepEventsPeriod else Duration.Zero
          delayKeepEvents = true
          keepEventsCancelable = Some(
            scheduler.scheduleOnce(delay) {
              self ! Internal.EventsAccepted(eventId)
            })
        }
        scheduler.scheduleOnce(eventFetchDelay) {
          self ! Internal.FetchEvents(lastEventId)
        }
      }

    case Input.Terminate ⇒
      terminating = true
      isConnected = false
      client.logout().runAsync onComplete { _ ⇒
        // TODO HTTP-Anfragen abbrechen und Antwort mit discardBytes() verwerfen, um Akka-Warnungen zu vermeiden
        // WARN  akka.http.impl.engine.client.PoolGateway - [0 (WaitingForResponseEntitySubscription)] Response entity was not subscribed after 1 second. Make sure to read the response entity body or call `discardBytes()` on it. GET /agent/api/master/event Empty -> 200 OK Chunked
        self ! PoisonPill
      }

    case Internal.BatchSucceeded(responses) ⇒
      val succeededInputs = commandQueue.handleBatchSucceeded(responses)
      val detachedOrderIds = succeededInputs collect { case Input.DetachOrder(orderId) ⇒ orderId }
      if (detachedOrderIds.nonEmpty) {
        context.parent ! Output.OrdersDetached(detachedOrderIds.toSet)
      }

    case Internal.BatchFailed(inputs, throwable) ⇒
      commandQueue.handleBatchFailed(inputs)
      handleConnectionError(throwable) {
        if (isConnected) {
          reconnect()
          //throwable match {
          //  case t: AkkaHttpClient.HttpException ⇒
          //    handleConnectionError(t)
          //
          //  case t ⇒
          //    handleConnectionError(t)
          //    // TODO Retry several times, finally inform commander about failure (dropping the order)
          //    // 1) Wenn es ein Verbindungsfehler ist (oder ein HTTP-Fehlercode eines Proxys), dann versuchen wir endlos weiter.
          //    // Der Verbindungsfehler kann die eine TCP-Verbindung betreffen. Dann genügt ein neuer Versuch.
          //    // Wenn wir einige (zwei?) Fehler in Folge ohne Erfolg zwischendurch erhalten, dann unterbrechen wir die Verbindung zum Agenten
          //    // für kurze Zeit, bevor wir sie wiederaufleben lassen.
          //    // Diesen Mechanisms haben wir schon.
          //    // 2) akka.stream.BufferOverflowException: Später wieder versuchen
          //    // 3) Mehrere Inputs (alle in inputQueue, doch limitiert) gebündelt dem Agenten schicken,
          //    // Agent antwortet mit entsprechened vielen Okays oder Fehlermeldungen
          //    // 4) Prüfen, ob akka.http.host-connection-pool.client.max-retries von 0 auf 5 (die Voreinstellung) erhöht werden kann.
          //}
        }
      }

    case Internal.CommandQueueReady ⇒

    case Internal.Fetched(Failure(throwable)) ⇒
      handleConnectionError(throwable) {
        if (isConnected) {
          reconnect()
        }
      }

    case Internal.Fetched(Success(stampedEvents)) ⇒
      if (logger.underlying.isTraceEnabled) for (stamped ← stampedEvents) { logCount += 1; logger.trace(s"#$logCount $stamped") }

      context.parent ! Output.EventsFromAgent(stampedEvents)  // TODO Possible OutOfMemoryError. Use reactive stream or acknowledge

      for (last ← stampedEvents.lastOption) {
        assert(lastEventId < last.eventId, s"last.eventId=${last.eventId} <= lastEventId=$lastEventId ?")
        lastEventId = last.eventId
      }
  }

  private def handleConnectionError(throwable: Throwable)(andThen: ⇒ Unit): Unit = {
    if (terminating)
      logger.debug(s"While terminating: ${throwable.toStringWithCauses}")
    else {
      logger.warn(throwable.toStringWithCauses)
      if (throwable.getStackTrace.nonEmpty) logger.debug("", throwable)
      persist(MasterAgentEvent.AgentCouplingFailed(throwable.toStringWithCauses)) { _ ⇒
        andThen
      }
    }
  }

  private def reconnect() = {
    if (!terminating) {
      isConnected = false
      client.logout().runAsync onComplete { _ ⇒
        self ! Internal.LoggedOut  // We ignore an error due to unknown SessionToken (because Agent may have been restarted) or Agent shutdown
      }
      become("LoggedOut") {
        case Internal.LoggedOut ⇒
          scheduler.scheduleOnce(reconnectPause.nextPause()) {
            self ! Internal.Connect
          }
          unstashAll()
          become("disconnected")(disconnected)

        case _ ⇒
          stash()
      }
    }
  }

  override def toString = s"AgentDriver(${agentId.toSimpleString}: $uri)"
}

private[master] object AgentDriver
{
  private val EventClasses = Set[Class[_ <: Event]](classOf[OrderEvent], classOf[AgentMasterEvent.AgentReadyForMaster])

  def props(agentId: AgentId, uri: Uri, masterConfiguration: MasterConfiguration, journalActor: ActorRef)(implicit ts: TimerService, s: Scheduler) =
    Props { new AgentDriver(agentId, uri, masterConfiguration, journalActor) }

  private class ReconnectPause {
    private val Minimum = 1.second
    private var pauses = initial
    private var lastConnect = now

    def onConnect() = lastConnect = now
    def onConnectSucceeded() = pauses = initial
    def nextPause() = (lastConnect + pauses.next() - now) max Minimum

    private def initial = Iterator(Minimum, Minimum, Minimum, Minimum, 2.seconds, 5.seconds) ++
      Iterator.continually(10.seconds)
  }

  sealed trait Input
  object Input {
    final case class Start(lastAgentEventId: EventId)

    sealed trait QueueableInput extends Input {
      def orderId: OrderId
      def toShortString: String
    }

    final case class AttachOrder(order: Order[Order.Idle], agentId: AgentId, workflow: Workflow) extends QueueableInput {
      def orderId = order.id
      def toShortString = s"AttachOrder($orderId)"
    }

    final case class DetachOrder(orderId: OrderId) extends QueueableInput {
      def toShortString = s"DetachOrder($orderId)"
    }

    final case class EventsAccepted(agentEventId: EventId)

    case object Terminate
  }

  object Output {
    final case class EventsFromAgent(stamped: Seq[Stamped[AnyKeyedEvent]])
    final case class OrdersDetached(orderIds: Set[OrderId])
  }

  private object Internal {
    final case object Connect
    final case class AfterConnect(result: Try[Completed]) extends DeadLetterSuppression
    final case object LoggedOut extends DeadLetterSuppression
    final case object Ready
    final case object CommandQueueReady extends DeadLetterSuppression
    final case class BatchSucceeded(responses: Seq[QueuedInputResponse]) extends DeadLetterSuppression
    final case class BatchFailed(inputs: Seq[Input.QueueableInput], throwable: Throwable) extends DeadLetterSuppression
    final case class FetchEvents(after: EventId) extends DeadLetterSuppression/*TODO Besser: Antwort empfangen ubd mit discardBytes() verwerfen, um Akka-Warnung zu vermeiden*/
    final case class Fetched(stampedTry: Try[Seq[Stamped[KeyedEvent[Event]]]]) extends DeadLetterSuppression
    final case class EventsAccepted(agentEventId: EventId) extends DeadLetterSuppression
  }
}
