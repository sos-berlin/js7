package com.sos.jobscheduler.master.order.agent

import akka.actor.{Actor, ActorRef, DeadLetterSuppression, Stash}
import akka.http.scaladsl.model.Uri
import akka.pattern.pipe
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{Accepted, Compound}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, EventId, EventRequest, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.jobnet.Jobnet
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.master.order.agent.AgentDriver._
import com.typesafe.config.Config
import com.typesafe.scalalogging.{Logger ⇒ ScalaLogger}
import java.time.Instant.now
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

/**
  * Keeps connection to an Agent, sends orders, and fetches events (using `EventFetcher`).
  *
  * @author Joacim Zschimmer
  */
final class AgentDriver(agentPath: AgentPath, uri: Uri, config: Config)
(implicit timerService: TimerService, executionContext: ExecutionContext)
extends Actor
with Stash {

  private val logger = Logger.withPrefix[AgentDriver](agentPath.string)
  private val client = AgentClient(uri)(context.system)
  private var startInputReceived = false
  private var eventFetcher: EventFetcher[OrderEvent] = null
  private var lastEventId = EventId.BeforeFirst
  private val reconnectPause = new ReconnectPause

  become(disconnected)
  self ! Internal.Connect

  private val commandQueue = new CommandQueue(client, self, logger)

  override def postStop() = {
    if (eventFetcher != null) {
      eventFetcher.close()
    }
    if (client.hasSession) {
      client.executeCommand(AgentCommand.Logout) onComplete {
        _ ⇒ client.close()  // Ignoring the response
      }
    } else {
      client.close()
    }
    super.postStop()
  }

  def receive = Actor.emptyBehavior

  private def disconnected: Receive = {
    case Internal.Connect ⇒
      logger.info(s"Trying to connect $uri")
      reconnectPause.onConnect()
      (for {
        _ ← client.executeCommand(AgentCommand.Login)  // Separate commands because AgentClient catches the SessionToken of Login.Response
        _ ← client.executeCommand(AgentCommand.RegisterAsMaster)
      } yield Internal.Connected)
      .recover {
        case t ⇒ Internal.ConnectFailed(t)
      } pipeTo self
      unstashAll()
      become(connecting)

    case _ ⇒
      stash
  }

  private def connecting: Receive = {
    case Internal.Connected ⇒
      logger.info("Connected")
      reconnectPause.onConnectSucceeded()
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
      commandQueue.processQueued()
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
      commandQueue.processQueued()
  }

  private def become(r: Receive) = context.become(r orElse handleOtherMessage)

  private def startEventFetcher(): Unit = {
    assert(eventFetcher == null)
    logger.info(s"Fetching events after ${EventId.toString(lastEventId)}")
    eventFetcher = new EventFetcher[OrderEvent](lastEventId) {
      def config = AgentDriver.this.config
      def fetchEvents(request: EventRequest[OrderEvent]) = client.mastersEvents(request)
      def onEvent(stamped: Stamped[KeyedEvent[OrderEvent]]) = self ! Internal.AgentEvent(stamped)  // TODO Possible OutOfMemoryError
    }
    eventFetcher.start() onComplete {
      o ⇒ self ! Internal.EventFetcherTerminated(o)
    }
  }

  private def onConnectionError(error: String): Unit = {
    logger.warn(error)
    if (eventFetcher != null) {
      eventFetcher.close()
      eventFetcher = null
    }
    become(disconnecting)
    if (client.hasSession) {
      client.executeCommand(AgentCommand.Logout) onComplete { _ ⇒
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
    case input: Input ⇒
      handleInput(input)

    case msg @ Internal.EventFetcherTerminated(Success(Completed)) ⇒
      logger.debug(msg.toString)

    case msg: Internal.CompoundResponse ⇒
      commandQueue.handleCompoundResponse(msg)

    case failed: Internal.CompoundFailed ⇒
      commandQueue.handleCompoundFailed(failed)
      failed.throwable match {
        case t: AgentClient.HttpException ⇒
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

    case Internal.AgentEvent(stamped) ⇒
      context.parent ! Output.EventFromAgent(stamped)  // TODO Possible OutOfMemoryError. Use reactive stream ?
      lastEventId = stamped.eventId
  }

  private def handleInput(input: Input): Unit = input match {
    case cmd: Input.AttachOrder ⇒
      commandQueue.enqueue(sender(), cmd)
      self ! Internal.CommandQueueReady

    case msg: Input.DetachOrder ⇒
      commandQueue.enqueue(sender(), msg)
      self ! Internal.CommandQueueReady
  }

  override def toString = s"AgentDriver($agentPath: $uri)"
}

private[master] object AgentDriver {

  private val OpenRequestsMaximum = 2

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
    final case class AttachOrder(order: Order[Order.Idle], jobnet: Jobnet) extends QueueableInput {
      def orderId = order.id
      def toShortString = s"AttachOrder($orderId, ${jobnet.path})"
    }
    final case class DetachOrder(orderId: OrderId) extends QueueableInput {
      def toShortString = s"DetachOrder($orderId)"
    }
  }

  object Output {
    final case class EventFromAgent(stamped: Stamped[AnyKeyedEvent])
    final case class OrderDetached(orderId: OrderId)
  }

  private object Internal {
    final case object Connect
    final case object Connected
    final case class ConnectFailed(throwable: Throwable)
    final case object LoggedOut
    final case object Ready
    final case class CompoundResponse(responses: Seq[QueuedInputResponse])
    final case class QueuedInputResponse(input: QueuedInput, response: Compound.SingleResponse)
    final case class CompoundFailed(inputs: Seq[QueuedInput], throwable: Throwable)
    final case object CommandQueueReady
    final case class AgentEvent(stamped: Stamped[KeyedEvent[OrderEvent]])
    final case class EventFetcherTerminated(completed: Try[Completed]) extends DeadLetterSuppression
  }

  private case class QueuedInput(sender: ActorRef, input: Input.QueueableInput)

  private class CommandQueue(client: AgentClient, self: ActorRef, logger: ScalaLogger)(implicit ec: ExecutionContext) {
    private val inputQueue = mutable.Queue[QueuedInput]()
    private val executingInputs = mutable.Set[QueuedInput]()
    private val openRequestCount = new AtomicInteger

    def enqueue(sender: ActorRef, input: Input.QueueableInput): Unit =
      inputQueue += QueuedInput(sender, input)

    def processQueued(): Unit =
      if (openRequestCount.get < OpenRequestsMaximum) {
        val cmds = inputQueue.filterNot(executingInputs).toVector
        if (cmds.nonEmpty) {
          executingInputs ++= cmds
          openRequestCount.incrementAndGet()
          client.executeCommand(Compound(cmds map (_.input) map inputToAgentCommand)) onComplete {
            case Success(Compound.Response(responses)) ⇒
              self ! Internal.CompoundResponse(for ((cmd, o) ← cmds zip responses) yield Internal.QueuedInputResponse(cmd, o))
            case Failure(t) ⇒
              self ! Internal.CompoundFailed(cmds, t)
          }
        }
      }

    private def inputToAgentCommand(input: Input.QueueableInput): AgentCommand =
      input match {
        case Input.AttachOrder(order, jobnet) ⇒
          AgentCommand.AttachOrder(order, jobnet)
        case Input.DetachOrder(orderId) ⇒
          AgentCommand.DetachOrder(orderId)
      }

    def handleCompoundResponse(compoundResponse: Internal.CompoundResponse): Unit = {
      for (Internal.QueuedInputResponse(QueuedInput(sender, cmd), response) ← compoundResponse.responses) {
        handleSingleResponse(sender, cmd, response)
      }
      val inputs = (compoundResponse.responses map (_.input)).toSet
      inputQueue.dequeueAll(inputs)
      onQueuedInputsProcessed(inputs)
    }

    private def handleSingleResponse(sender: ActorRef, cmd: Input.QueueableInput, response: Compound.SingleResponse): Unit =
      response match {
         case Compound.Succeeded(Accepted) ⇒
           cmd match {
             case cmd: Input.AttachOrder ⇒
               logger.info(s"Order '${cmd.orderId}' attached to Agent")
             case cmd: Input.DetachOrder ⇒
               logger.debug(s"Order '${cmd.orderId}' detached from Agent")
               sender ! Output.OrderDetached(cmd.orderId)
           }

         case Compound.Failed(message) ⇒
           logger.error(s"Agent has rejected the command ${cmd.toShortString}: $message")
           // Agent's state does not match master's state ???
       }

    def handleCompoundFailed(failed: Internal.CompoundFailed): Unit = {
      // Don't remove from inputQueue. Queued inputs will be processed again
      onQueuedInputsProcessed(failed.inputs.toSet)
    }

    private def onQueuedInputsProcessed(inputs: Set[QueuedInput]): Unit = {
      executingInputs --= inputs
      if (openRequestCount.decrementAndGet() == 0) {
        self ! Internal.CommandQueueReady
      }
    }
  }
}
