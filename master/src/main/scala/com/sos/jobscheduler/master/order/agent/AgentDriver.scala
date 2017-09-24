package com.sos.jobscheduler.master.order.agent

import akka.actor.{Actor, DeadLetterSuppression, Stash}
import akka.http.scaladsl.model.Uri
import akka.pattern.pipe
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.data.commands.AgentCommand
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
import java.time.Instant.now
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
  * Keeps connection to an Agent, sends orders, and fetches events (using `EventFetcher`).
  *
  * @author Joacim Zschimmer
  */
final class AgentDriver(agentPath: AgentPath, uri: Uri)
(implicit timerService: TimerService, executionContext: ExecutionContext)
extends Actor
with Stash {

  private val logger = Logger.withPrefix[AgentDriver](agentPath.string)
  private val client = AgentClient(uri)(context.system)
  private var startCommandReceived = false
  private var eventFetcher: EventFetcher[OrderEvent] = null
  private var lastEventId = EventId.BeforeFirst
  private val commandQueue = mutable.Queue[Input.AttachOrder]()
  private val executingCommands = mutable.Set[Input.AttachOrder]()
  private val openRequestCount = new AtomicInteger
  private val reconnectPause = new ReconnectPause

  become(disconnected)
  self ! Internal.Connect

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
      if (startCommandReceived) {
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
      startCommandReceived = true
      startEventFetcher()
      processQueuedCommands()
      unstashAll()
      become(ready)
      logger.info("Ready")

    case _ ⇒
      stash
  }

  private def ready: Receive = {
    case msg @ Internal.EventFetcherTerminated(Failure(_)) ⇒
      onConnectionError(msg.toString)

    case Internal.CommandReady ⇒
      processQueuedCommands()
  }

  private def become(r: Receive) = context.become(r orElse handleOtherMessage)

  def handleOtherMessage: Receive = {
    case input: Input ⇒
      handleInput(input)

    case msg @ Internal.EventFetcherTerminated(Success(Completed)) ⇒
      logger.debug(msg.toString)

    case Internal.OrderAttachedToAgent(cmd, outcome) ⇒
      executingCommands -= cmd
      outcome match {
        case AttachOrderResult.Attached ⇒
          logger.info(s"Order '${cmd.order.id}' attached to Agent")
          commandQueue.dequeueFirst(_ == cmd)
          self ! Internal.CommandReady

        case AttachOrderResult.Rejected(message) ⇒
          commandQueue.dequeueFirst(_ == cmd)
          logger.error(s"Agent has rejected the order: $message")
          // Agent has rejected the order ???

        case AttachOrderResult.Failure(t) if AgentClient.sessionIsPossiblyLost(t) ⇒
          commandQueue.dequeueFirst(_ == cmd)
          onConnectionError(t.toString)

        case AttachOrderResult.Failure(e: AgentClient.HttpException)  ⇒
          commandQueue.dequeueFirst(_ == cmd)
          onConnectionError(e.toString)

        case AttachOrderResult.Failure(t) ⇒
          logger.error(s"${cmd.order.id}: ${t.toStringWithCauses}", t)
          // TODO Retry several times, finally inform commander about failure (dropping the order)
          // 1) Wenn es ein Verbindungsfehler ist (oder ein HTTP-Fehlercode eines Proxys), dann versuchen wir endlos weiter.
          // Der Verbindungsfehler kann die eine TCP-Verbindung betreffen. Dann genügt ein neuer Versuch.
          // Wenn wir einige (zwei?) Fehler in Folge ohne Erfolg zwischendurch erhalten, dann unterbrechen wir die Verbindung zum Agenten
          // für kurze Zeit, bevor wir sie wiederaufleben lassen.
          // Diesen Mechanisms haben wir schon.
          // 2) akka.stream.BufferOverflowException: Später wieder versuchen
          // 3) Mehrere Kommandos (alle in commandQueue, doch limitiert) gebündelt dem Agenten schicken,
          // Agent antwortet mit entsprechened vielen Okays oder Fehlermeldungen
          // 4) Prüfen, ob akka.http.host-connection-pool.client.max-retries von 0 auf 5 (die Voreinstellung) erhöht werden kann.
          commandQueue.dequeueFirst(_ == cmd)
          self ! Internal.CommandReady
      }

    case Internal.CommandReady ⇒

    case Internal.AgentEvent(stamped) ⇒
      context.parent ! Output.EventFromAgent(stamped)  // TODO Possible OutOfMemoryError. Use reactive stream ?
      lastEventId = stamped.eventId
  }

  private def startEventFetcher(): Unit = {
    assert(eventFetcher == null)
    logger.info(s"Fetching events after ${EventId.toString(lastEventId)}")
    eventFetcher = new EventFetcher[OrderEvent](lastEventId) {
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

  private def handleInput(input: Input): Unit = input match {
    case cmd: Input.AttachOrder ⇒
      commandQueue += cmd
      self ! Internal.CommandReady

    case Input.DetachOrder(orderId) ⇒
      val cmd = AgentCommand.DetachOrder(orderId)   // TODO commandQueue sollte auch DetachOrder aufnehmen, so dass mehrere als Compound geschickt werden können.
      val sender = this.sender()
      client.executeCommand(cmd) onComplete {
        // Closure
        case Success(_) ⇒
          logger.info(s"$orderId detached from Agent")
          sender ! Output.OrderDetached(orderId)
        case Failure(t) ⇒
          logger.error(s"$cmd: ${t.toStringWithCauses}", t)
      }
  }

  private def processQueuedCommands(): Unit = {
    if (openRequestCount.get < OpenRequestsMaximum) {
      val cmds = commandQueue.filter(o ⇒ !executingCommands(o)).toVector
      if (cmds.nonEmpty) {
        executingCommands ++= cmds
        val compoundCommand = AgentCommand.Compound(
          for (c ← cmds; cmd ← AgentCommand.AttachJobnet(c.jobnet) :: AgentCommand.AttachOrder(c.order) :: Nil)
          yield cmd)
        val factor = 2  // TODO AttachOrder und AttachJobNet zu einem Kommando verschmelzen, so dass factor und grouped entfallen können. commandQueue soll direkt AttachOrder aufnehmen
        assert(compoundCommand.commands.size == factor * cmds.size)
        openRequestCount.incrementAndGet()
        val whenOrderResponses: Future[Seq[AttachOrderOutcome]] =
          client.executeCommand(compoundCommand) map { compoundResponse ⇒
            for (response ← (compoundResponse.responses grouped factor).toVector) yield
              response collectFirst {
                case AgentCommand.Compound.Failed(message) ⇒ message
              } match {
                case None ⇒ AttachOrderResult.Attached
                case Some(message) ⇒ AttachOrderResult.Rejected(message)
              }
            } recover {
              case t ⇒ Vector.fill(cmds.size) { AttachOrderResult.Failure(t) }
            }
        for (orderResponses ← whenOrderResponses) {
          for ((cmd, o) ← cmds zip orderResponses) {
            self ! Internal.OrderAttachedToAgent(cmd, o)
          }
        }
        whenOrderResponses onComplete { _ ⇒
          if (openRequestCount.decrementAndGet() == 0) {
            self ! Internal.CommandReady
          }
        }
      }
    }
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
    final case class AttachOrder(order: Order[Order.Idle], jobnet: Jobnet) extends Input
    final case class DetachOrder(orderId: OrderId) extends Input
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
    final case class OrderAttachedToAgent(command: Input.AttachOrder, outcome: AttachOrderOutcome)
    final case object CommandReady
    final case class AgentEvent(stamped: Stamped[KeyedEvent[OrderEvent]])
    final case class EventFetcherTerminated(completed: Try[Completed]) extends DeadLetterSuppression
  }

  private sealed trait AttachOrderOutcome
  private object AttachOrderResult {
    case class Rejected(string: String) extends AttachOrderOutcome
    case object Attached extends AttachOrderOutcome
    case class Failure(throwable: Throwable) extends AttachOrderOutcome
  }
}
