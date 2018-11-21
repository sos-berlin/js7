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
  * Couples to an Agent, sends orders, and fetches events.
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
  private val terminationLogoutTimeout = config.getDuration("jobscheduler.master.agent-driver.termination-logout-timeout").toFiniteDuration
  private val authConfigPath = "jobscheduler.auth.agents." + ConfigUtil.joinPath(agentId.path.string)
  private val recouplePause = new RecouplingPause
  private val client = AgentClient(uri, masterConfiguration.keyStoreRefOption, masterConfiguration.trustStoreRefOption)(context.system)

  private val agentUserAndPassword: Checked[UserAndPassword] =
    config.optionAs[SecretString](authConfigPath)
      .map(password ⇒ UserAndPassword(masterConfiguration.masterId.toUserId, password))
      .toChecked(Problem(s"Missing password for '${agentId.path}', no configuration entry for $authConfigPath"))

  private var startInputReceived = false
  private var isCoupled = false
  private var isAwaitingFetchedEvents = false
  private var lastEventId = EventId.BeforeFirst
  private var couplingNumber = 0L
  @volatile
  private var keepEventsCancelable: Option[Cancelable] = None
  private var delayKeepEvents = false
  private var logCount = 0
  private var lastError: Option[(Long, String)] = None
  private var terminating = false

  become("decoupled")(decoupled)
  self ! Internal.Couple

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

  private def decoupled: Receive = {
    case Internal.Couple ⇒
      delayKeepEvents = false
      recouplePause.onCouple()
      agentUserAndPassword match {
        case Invalid(problem) ⇒
          logger.error(problem.toString)
          // Actor freezes here. TODO 1) reflect state in web service /api/agent; 2) publish an event to notify about this error

        case Valid(userAndPassword) ⇒
          ( for {
              _ ← client.login(Some(userAndPassword))  // Separate commands because AgentClient catches the SessionToken of Login.LoggedIn
              _ ← if (lastEventId == EventId.BeforeFirst)
                    client.commandExecute(AgentCommand.RegisterAsMaster)
                  else
                    Task.pure(Completed)
            } yield Completed
          ).runAsync.onComplete { tried ⇒
            self ! Internal.AfterCoupling(tried)
          }
          unstashAll()
          become("coupling")(coupling)
      }

    case _ ⇒
      stash()
  }

  private def coupling: Receive = {
    case Internal.AfterCoupling(Success(Completed)) ⇒
      couplingNumber += 1
      lastError = None
      isCoupled = true
      recouplePause.onCouplingSucceeded()
      commandQueue.onRecoupled()
      unstashAll()
      become("waitingForStart")(waitingForStart)
      if (startInputReceived) {
        self ! Input.Start(lastEventId)
      }

    case Internal.AfterCoupling(Failure(throwable)) ⇒
      handleConnectionError(throwable) {
        recouple()
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
    case Internal.FetchEvents(after) if isCoupled && !isAwaitingFetchedEvents ⇒
      if (!terminating) {
        isAwaitingFetchedEvents = true
        lastEventId = after
        val fetchCouplingNumber = couplingNumber
        client.mastersEvents(EventRequest[Event](EventClasses, after = after, eventFetchTimeout, limit = EventLimit))
          .runAsync
          .onComplete { tried ⇒
            // *** Asynchronous ***
            self ! Internal.Fetched(tried, after, fetchCouplingNumber)
          }
        }

    case Internal.KeepEvents(after) ⇒
      if (!terminating && isCoupled) {
        // TODO Use CommandQueue to avoid a third TCP connection
        client.commandExecute(AgentCommand.KeepEvents(after = after)).runOnComplete { tried ⇒
          tried.failed.foreach(t ⇒ logger.warn("AgentCommand.KeepEvents failed: " + t.toStringWithCauses))
          keepEventsCancelable = None  // Asynchronous!
        }
      }

    case Internal.CommandQueueReady ⇒
      if (!terminating) {
        commandQueue.maySend()
      }
  }

  override protected def become(state: String)(recv: Receive) =
    super.become(state)(handleStandardMessage orElse recv orElse handleOtherMessage)

  private def handleStandardMessage: Receive = {
    case Input.Terminate ⇒
      terminating = true
      if (!isCoupled) {
        self ! PoisonPill
      } else {
        isCoupled = false
        // TODO HTTP-Anfragen abbrechen und Antwort mit discardBytes() verwerfen, um folgende Akka-Warnungen zu vermeiden
        // WARN  akka.http.impl.engine.client.PoolGateway - [0 (WaitingForResponseEntitySubscription)] LoggedIn entity was not subscribed after 1 second. Make sure to read the response entity body or call `discardBytes()` on it. GET /agent/api/master/event Empty -> 200 OK Chunked
        client.logout().timeout(terminationLogoutTimeout).runAsync onComplete { tried ⇒
          for (t ← tried.failed) logger.debug(s"Logout failed: " ++ t.toStringWithCauses)
          self ! PoisonPill
        }
      }
  }

  private def handleOtherMessage: Receive = {
    case input: Input.QueueableInput if sender() == context.parent ⇒
      if (!terminating) {
        commandQueue.enqueue(input)
        scheduler.scheduleOnce(batchDelay) {
          self ! Internal.CommandQueueReady  // (Even with batchDelay == 0) delay maySend() such that QueueableInput pending in actor's mailbox can be queued
        }
      }

    case Input.EventsAccepted(after) ⇒
      assert(after == lastEventId, s"Input.EventsAccepted($after) ≠ lastEventId=$lastEventId ?")
      if (isCoupled/*???*/) {
        if (keepEventsCancelable.isEmpty) {
          val delay = if (delayKeepEvents) keepEventsPeriod else Duration.Zero
          delayKeepEvents = true
          keepEventsCancelable = Some(
            scheduler.scheduleOnce(delay) {
              self ! Internal.KeepEvents(after)
            })
        }
        scheduler.scheduleOnce(eventFetchDelay) {
          self ! Internal.FetchEvents(lastEventId)
        }
      }

    case Internal.BatchSucceeded(responses) ⇒
      lastError = None
      val succeededInputs = commandQueue.handleBatchSucceeded(responses)
      val detachedOrderIds = succeededInputs collect { case Input.DetachOrder(orderId) ⇒ orderId }
      if (detachedOrderIds.nonEmpty) {
        context.parent ! Output.OrdersDetached(detachedOrderIds.toSet)
      }

    case Internal.BatchFailed(inputs, throwable) ⇒
      commandQueue.handleBatchFailed(inputs)
      handleConnectionError(throwable) {
        if (isCoupled) {
          recouple()
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
          //    // Diesen Mechanismus haben wir schon.
          //    // 2) akka.stream.BufferOverflowException: Später wieder versuchen
          //    // 3) Mehrere Inputs (alle in inputQueue, doch limitiert) gebündelt dem Agenten schicken,
          //    // Agent antwortet mit entsprechened vielen Okays oder Fehlermeldungen
          //    // 4) Prüfen, ob akka.http.host-connection-pool.client.max-retries von 0 auf 5 (die Voreinstellung) erhöht werden kann.
          //}
        }
      }

    case Internal.CommandQueueReady ⇒

    case Internal.Fetched(Failure(throwable), _, fetchCouplingNumber) ⇒
      isAwaitingFetchedEvents = false
      if (fetchCouplingNumber == couplingNumber && isCoupled) {
        handleConnectionError(throwable) {
          recouple()
        }
      }

    case Internal.Fetched(Success(eventSeq), after, fetchCouplingNumber) ⇒
      isAwaitingFetchedEvents = false
      if (fetchCouplingNumber != couplingNumber) {
        logger.debug(s"Discarding obsolete Agent response Internal.Fetched(after=$after)")
      } else
        eventSeq match {
          case EventSeq.NonEmpty(stampedEvents) ⇒
            lastError = None
            if (logger.underlying.isTraceEnabled) for (stamped ← stampedEvents) { logCount += 1; logger.trace(s"#$logCount $stamped") }

            context.parent ! Output.EventsFromAgent(stampedEvents)  // TODO Possible OutOfMemoryError. Use reactive stream or acknowledge

            for (last ← stampedEvents.lastOption) {
              assert(lastEventId < last.eventId, s"last.eventId=${last.eventId} <= lastEventId=$lastEventId ?")
              lastEventId = last.eventId
            }

          case EventSeq.Empty(lastEventId_) ⇒  // No events after timeout
            if (isCoupled) scheduler.scheduleOnce(1.second) {
              self ! Internal.FetchEvents(lastEventId_)
            }

          case torn: TearableEventSeq.Torn ⇒
            val problem = Problem(s"Bad response from Agent $agentId $uri: $torn, requested events after=$after")
            logger.error(problem.toString)
            persist(MasterAgentEvent.AgentCouplingFailed(problem.toString)) { _ ⇒
              if (isCoupled) scheduler.scheduleOnce(15.second) {
                self ! Internal.FetchEvents(after)
              }
            }
        }
  }

  private def handleConnectionError(throwable: Throwable)(andThen: ⇒ Unit): Unit = {
    val msg = throwable.toStringWithCauses
    if (terminating)
      logger.debug(s"While terminating: $msg")
    else if (lastError.forall(_._1 == couplingNumber) && lastError.forall(_._2 != msg)) {
      lastError = Some(couplingNumber → msg)
      logger.warn(msg)
      if (throwable.getStackTrace.nonEmpty) logger.debug(msg, throwable)
      persist(MasterAgentEvent.AgentCouplingFailed(throwable.toStringWithCauses)) { _ ⇒
        andThen
      }
    } else {
      logger.debug(msg)
      andThen
    }
  }

  private def recouple() = {
    if (!terminating) {
      isCoupled = false
      client.logout().runAsync onComplete { _ ⇒
        self ! Internal.LoggedOut  // We ignore an error due to unknown SessionToken (because Agent may have been restarted) or Agent shutdown
      }
      become("LoggedOut") {
        case Internal.LoggedOut ⇒
          scheduler.scheduleOnce(recouplePause.nextPause()) {
            self ! Internal.Couple
          }
          unstashAll()
          become("decoupled")(decoupled)

        case _ ⇒
          stash()
      }
    }
  }

  override def toString = s"AgentDriver(${agentId.toSimpleString}: $uri)"
}

private[master] object AgentDriver
{
  private val EventLimit = 1000  // OrderStdWritten may be up to 10000 characters (30000 UTF-8 characters), so memory usage may be 30MB
  private val EventClasses = Set[Class[_ <: Event]](classOf[OrderEvent], classOf[AgentMasterEvent.AgentReadyForMaster])

  def props(agentId: AgentId, uri: Uri, masterConfiguration: MasterConfiguration, journalActor: ActorRef)(implicit ts: TimerService, s: Scheduler) =
    Props { new AgentDriver(agentId, uri, masterConfiguration, journalActor) }

  private class RecouplingPause {
    private val Minimum = 1.second
    private var pauses = initial
    private var lastCoupling = now

    def onCouple() = lastCoupling = now
    def onCouplingSucceeded() = pauses = initial
    def nextPause() = (lastCoupling + pauses.next() - now) max Minimum

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

    final case class AttachOrder(order: Order[Order.FreshOrReady], agentId: AgentId, workflow: Workflow) extends QueueableInput {
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
    final case object Couple
    final case class AfterCoupling(result: Try[Completed]) extends DeadLetterSuppression
    final case object LoggedOut extends DeadLetterSuppression
    final case object Ready
    final case object CommandQueueReady extends DeadLetterSuppression
    final case class BatchSucceeded(responses: Seq[QueuedInputResponse]) extends DeadLetterSuppression
    final case class BatchFailed(inputs: Seq[Input.QueueableInput], throwable: Throwable) extends DeadLetterSuppression
    final case class FetchEvents(after: EventId) extends DeadLetterSuppression/*TODO Besser: Antwort empfangen ubd mit discardBytes() verwerfen, um Akka-Warnung zu vermeiden*/
    final case class Fetched(stampedTry: Try[TearableEventSeq[Seq, KeyedEvent[Event]]], after: EventId, couplingNumber: Long) extends DeadLetterSuppression
    final case class KeepEvents(agentEventId: EventId) extends DeadLetterSuppression
  }
}
