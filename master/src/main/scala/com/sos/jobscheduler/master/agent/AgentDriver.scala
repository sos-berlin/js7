package com.sos.jobscheduler.master.agent

import akka.actor.{Actor, DeadLetterSuppression, Props, Stash}
import akka.http.scaladsl.model.Uri
import akka.pattern.pipe
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.problem.Checked.CheckedOption
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.agent.AgentId
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, EventId, EventRequest, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.agent.AgentDriver._
import com.sos.jobscheduler.master.agent.CommandQueue.QueuedInputResponse
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.typesafe.config.ConfigUtil
import java.time.Instant.now
import monix.execution.Scheduler
import scala.collection.immutable.Seq
import scala.util.{Failure, Success, Try}

/**
  * Keeps connection to an Agent, sends orders, and fetches events (using `EventFetcher`).
  *
  * @author Joacim Zschimmer
  */
final class AgentDriver private(agentId: AgentId, uri: Uri, masterConfiguration: MasterConfiguration)
(implicit timerService: TimerService, scheduler: Scheduler)
extends Actor
with Stash {

  private val logger = Logger.withPrefix[AgentDriver](agentId.toSimpleString + " " + uri)
  private val config = masterConfiguration.config
  private val client = AgentClient(uri)(context.system)
  private var startInputReceived = false
  private var eventFetcher: EventFetcher[OrderEvent] = null
  private var lastEventId = EventId.BeforeFirst
  private val reconnectPause = new ReconnectPause

  become(disconnected)
  self ! Internal.Connect

  private val commandQueue = new CommandQueue(logger, batchSize = config.getInt("jobscheduler.master.agent-driver.command-batch-size")) {
    protected def executeCommand(command: AgentCommand.Batch) =
      client.executeCommand(command)

    protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) =
      self ! Internal.BatchSucceeded(queuedInputResponses)

    protected def asyncOnBatchFailed(inputs: Vector[Input.QueueableInput], throwable: Throwable) =
      self ! Internal.BatchFailed(inputs, throwable)
  }

  override def postStop() = {
    if (eventFetcher != null) {
      eventFetcher.close()
    }
    if (client.hasSession) {
      client.logout().runAsync onComplete {
        _ ⇒ client.close()
      }
      // Don't await response
    } else {
      client.close()
    }
    super.postStop()
  }

  def receive = Actor.emptyBehavior

  private def disconnected: Receive = {
    case Internal.Connect ⇒
      reconnectPause.onConnect()
      agentUserAndPassword match {
        case Invalid(problem) ⇒
          logger.error(problem.toString)
          // Actor freezes here. TODO 1) reflect state in web service /api/agent; 2) publish an event to notify about this error

        case Valid(userAndPassword) ⇒
          ( for {
              _ ← client.login(Some(userAndPassword))  // Separate commands because AgentClient catches the SessionToken of Login.Response
              _ ← client.executeCommand(AgentCommand.RegisterAsMaster)
            } yield Internal.Connected
          ).runAsync
            .recover {
              case t ⇒ Internal.ConnectFailed(t)
            } pipeTo self
          unstashAll()
          become(connecting)
      }

    case _ ⇒
      stash
  }

  private def connecting: Receive = {
    case Internal.Connected ⇒
      logger.info("Connected")
      reconnectPause.onConnectSucceeded()
      commandQueue.onReconnected()
      unstashAll()
      become(waitingForStart)
      if (startInputReceived) {
        self ! Input.Start(lastEventId)
      }

    case msg: Internal.ConnectFailed ⇒
      reconnectPause.onConnectFailed()
      onConnectionError(msg.toString)

    case _ ⇒
      stash
  }

  private def waitingForStart: Receive = {
    case Input.Start(eventId) ⇒
      lastEventId = eventId
      startInputReceived = true
      startEventFetcher()
      commandQueue.maySend()
      unstashAll()
      become(ready)
      logger.info("Ready")

    case _ ⇒
      stash
  }

  private def ready: Receive = {
    case msg @ Internal.EventFetcherTerminated(Failure(_)) ⇒
      onConnectionError(msg.toString)

    case Internal.CommandQueueReady ⇒
      commandQueue.maySend()
  }

  private def become(r: Receive) = context.become(r orElse handleOtherMessage)

  private def startEventFetcher(): Unit = {
    assert(eventFetcher == null)
    logger.info(s"Fetching events after ${EventId.toString(lastEventId)}")
    eventFetcher = new EventFetcher[OrderEvent](lastEventId) {
      def config = AgentDriver.this.config
      def fetchEvents(request: EventRequest[OrderEvent]) = client.mastersEvents(request).runAsync
      def onEvents(stamped: Seq[Stamped[KeyedEvent[OrderEvent]]]) = self ! Internal.AgentEvents(stamped)
    }
    eventFetcher.start() onComplete {
      o ⇒ self ! Internal.EventFetcherTerminated(o)
    }
  }

  private def agentUserAndPassword: Checked[UserAndPassword] = {
    val configPath = "jobscheduler.auth.agents." + ConfigUtil.joinPath(agentId.path.string)
    config.optionAs[SecretString](configPath)
      .map(password ⇒ UserAndPassword(masterConfiguration.masterId.toUserId, password))
      .toChecked(Problem(s"Missing password for '${agentId.path}', no configuration entry for $configPath"))
  }

  private def onConnectionError(error: String): Unit = {
    logger.warn(error)
    if (eventFetcher != null) {
      eventFetcher.close()
      eventFetcher = null
    }
    become(disconnecting)
    if (client.hasSession) {
      client.logout().runAsync onComplete { _ ⇒
        self ! Internal.LoggedOut  // We ignore an error due to unknown SessionToken (because Agent has been restarted) or Agent shutdown
      }
    } else {
      self ! Internal.LoggedOut
    }
  }

  private def disconnecting: Receive = {
    case Internal.LoggedOut ⇒
      client.clearSession()
      context.system.scheduler.scheduleOnce(reconnectPause.pause.toFiniteDuration, self, Internal.Connect)
      become(disconnected)
  }

  private def handleOtherMessage: Receive = {
    case input: Input.QueueableInput if sender() == context.parent ⇒
      commandQueue.enqueue(input)
      self ! Internal.CommandQueueReady  // Delay maySend() such that QueuableInput pending in actor's mailbox can be queued

    case msg @ Internal.EventFetcherTerminated(Success(Completed)) ⇒
      logger.debug(msg.toString)

    case Internal.BatchSucceeded(responses) ⇒
      val succeededInputs = commandQueue.handleBatchSucceeded(responses)
      val detachedOrderIds = succeededInputs collect { case Input.DetachOrder(orderId) ⇒ orderId }
      if (detachedOrderIds.nonEmpty) {
        context.parent ! Output.OrdersDetached(detachedOrderIds.toSet)
      }

    case Internal.BatchFailed(inputs, throwable) ⇒
      commandQueue.handleBatchFailed(inputs)
      throwable match {
        case t: AkkaHttpClient.HttpException ⇒
          onConnectionError(t.toStringWithCauses)

        case t ⇒
          logger.error(s"$t", t)
          onConnectionError(t.toStringWithCauses)
          // TODO Retry several times, finally inform commander about failure (dropping the order)
          // 1) Wenn es ein Verbindungsfehler ist (oder ein HTTP-Fehlercode eines Proxys), dann versuchen wir endlos weiter.
          // Der Verbindungsfehler kann die eine TCP-Verbindung betreffen. Dann genügt ein neuer Versuch.
          // Wenn wir einige (zwei?) Fehler in Folge ohne Erfolg zwischendurch erhalten, dann unterbrechen wir die Verbindung zum Agenten
          // für kurze Zeit, bevor wir sie wiederaufleben lassen.
          // Diesen Mechanisms haben wir schon.
          // 2) akka.stream.BufferOverflowException: Später wieder versuchen
          // 3) Mehrere Inputs (alle in inputQueue, doch limitiert) gebündelt dem Agenten schicken,
          // Agent antwortet mit entsprechened vielen Okays oder Fehlermeldungen
          // 4) Prüfen, ob akka.http.host-connection-pool.client.max-retries von 0 auf 5 (die Voreinstellung) erhöht werden kann.
      }

    case Internal.CommandQueueReady ⇒

    case Internal.AgentEvents(stamped) ⇒
      context.parent ! Output.EventsFromAgent(stamped)  // TODO Possible OutOfMemoryError. Use reactive stream ?
      for (last ← stamped.lastOption) {
        lastEventId = last.eventId
      }
  }

  override def toString = s"AgentDriver(${agentId.toSimpleString}: $uri)"
}

private[master] object AgentDriver
{
  def props(agentId: AgentId, uri: Uri, masterConfiguration: MasterConfiguration)(implicit ts: TimerService, s: Scheduler) =
    Props { new AgentDriver(agentId, uri, masterConfiguration) }

  private class ReconnectPause {
    private val ShortPause = 1.s
    private val LongPause = 10.s
    private var nextPause = ShortPause
    private var lastConnect = now

    def onConnect() = lastConnect = now
    def onConnectSucceeded() = nextPause = ShortPause
    def onConnectFailed() = nextPause = 2 * nextPause min LongPause
    def pause = (lastConnect + nextPause - now) max ShortPause
  }

  sealed trait Input
  object Input {
    final case class Start(lastAgentEventId: EventId)

    sealed trait QueueableInput extends Input {
      def orderId: OrderId
      def toShortString: String
    }

    final case class AttachOrder(order: Order[Order.Idle], agentId: AgentId,  workflow: Workflow) extends QueueableInput {
      def orderId = order.id
      def toShortString = s"AttachOrder($orderId)"
    }

    final case class DetachOrder(orderId: OrderId) extends QueueableInput {
      def toShortString = s"DetachOrder($orderId)"
    }
  }

  object Output {
    final case class EventsFromAgent(stamped: Seq[Stamped[AnyKeyedEvent]])
    final case class OrdersDetached(orderIds: Set[OrderId])
  }

  private object Internal {
    final case object Connect
    final case object Connected
    final case class ConnectFailed(throwable: Throwable)
    final case object LoggedOut
    final case object Ready
    final case object CommandQueueReady
    final case class AgentEvents(stamped: Seq[Stamped[KeyedEvent[OrderEvent]]])
    final case class EventFetcherTerminated(completed: Try[Completed]) extends DeadLetterSuppression
    final case class BatchSucceeded(responses: Seq[QueuedInputResponse])
    final case class BatchFailed(inputs: Seq[Input.QueueableInput], throwable: Throwable)
  }
}
