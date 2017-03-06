package com.sos.jobscheduler.master.order.agent

import akka.Done
import akka.actor.{Actor, DeadLetterSuppression, Stash}
import akka.pattern.pipe
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.data.commands.{AddJobnet, AddOrder, DetachOrder, Login, Logout, RegisterAsMaster}
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
import scala.collection.immutable.Iterable
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}
import spray.http.Uri

/**
  * @author Joacim Zschimmer
  */
final class AgentDriver(agentPath: AgentPath, uri: Uri)
(implicit timerService: TimerService, executionContext: ExecutionContext)
extends Actor
with Stash {

  private val logger = Logger.withPrefix[AgentDriver](agentPath.string)
  private val client = AgentClient(uri)
  private var eventFetcher: EventFetcher[OrderEvent] = null
  private var lastEventId = EventId.BeforeFirst
  private val commandQueue = mutable.Queue[Input.AttachOrder]()   // == Stash ???
  private val executingCommands = mutable.Set[Input.AttachOrder]()
  private val orderIds = mutable.Set[OrderId]()
  private var startCommandReceived = false
  private val reconnectPause = new ReconnectPause

  become(disconnected)
  self ! Internal.Connect

  override def postStop() = {
    if (eventFetcher != null) {
      eventFetcher.close()
    }
    if (client.hasSession) {
      client.executeCommand(Logout)  // Ignoring response
    }
    super.postStop()
  }

  def receive = Actor.emptyBehavior

  private def disconnected: Receive = {
    case Internal.Connect ⇒
      logger.info(s"Trying to connect $uri")
      reconnectPause.onConnect()
      (for (_ ← client.executeCommand(Login);
            _ ← client.executeCommand(RegisterAsMaster))
        yield Internal.Connected
      ) recover {
        case t ⇒ Internal.ConnectFailed(t)
      } pipeTo
        self
      unstashAll()
      become(connecting)

    case _ ⇒
      stash
  }

  private def connecting: Receive = {
    case Internal.Connected ⇒
      reconnectPause.onConnectSucceeded()
      unstashAll()
      become(waitingForStart)
      logger.info("Connected")
      if (startCommandReceived) {
        self ! Input.Start
      }

    case msg: Internal.ConnectFailed ⇒
      reconnectPause.onConnectFailed()
      onConnectionError(msg.toString)

    case _ ⇒
      stash
  }

  private def waitingForStart: Receive = {
    case Input.Recover(eventId, recoveredOrderIds) ⇒
      lastEventId = eventId
      orderIds ++= recoveredOrderIds
      sender() ! Output.Recovered

    case Input.Start ⇒
      startCommandReceived = true
      startEventFetcher()
      unstashAll()
      processQueuedCommands()
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

    case msg @ Internal.OrderAttachedToAgent(cmd, tried) ⇒
      executingCommands -= cmd
      tried match {
        case Success(Done) ⇒
          logger.info(s"Order '${cmd.order.id}' attached to Agent")
          commandQueue.dequeueFirst(_ == cmd)
          self ! Internal.CommandReady
        case Failure(t) if AgentClient.sessionIsPossiblyLost(t) ⇒
          onConnectionError(msg.toString)
        case Failure(_: spray.can.Http.ConnectionException)  ⇒
          onConnectionError(msg.toString)
        case Failure(t) ⇒
          logger.error(s"${cmd.order.id}: ${t.toStringWithCauses}")
          // TODO How to inform the commander ?
          commandQueue.dequeueFirst(_ == cmd)
          self ! Internal.CommandReady
      }

    case Internal.OrderDetached(orderId) ⇒
      orderIds -= orderId

    case Internal.CommandReady ⇒

    case Internal.AgentEvent(stamped) ⇒
      context.parent ! Output.EventFromAgent(stamped)  // TODO Possible Akka Mailbox overflow. Use reactive stream ?
      lastEventId = stamped.eventId
  }

  private def startEventFetcher(): Unit = {
    assert(eventFetcher == null)
    logger.info(s"Fetching events after ${EventId.toString(lastEventId)}")
    eventFetcher = new EventFetcher[OrderEvent](lastEventId) {
      protected def fetchEvents(request: EventRequest[OrderEvent]) = client.mastersEvents(request)
      protected def onEvent(stamped: Stamped[KeyedEvent[OrderEvent]]) = self ! Internal.AgentEvent(stamped)
    }
    eventFetcher.start().onComplete {
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
      client.executeCommand(Logout) onComplete { _ ⇒
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
      val orderId = cmd.order.id
      if (orderIds contains orderId) {
        val msg = s"Duplicate $orderId"  // Error handling ???
        logger.warn(msg)
      } else {
        orderIds += orderId
        commandQueue += cmd
        self ! Internal.CommandReady
      }

    case Input.DetachOrder(orderId) ⇒
      val cmd = DetachOrder(orderId)
      val sender = this.sender()
      client.executeCommand(cmd) onComplete {
        case Success(_) ⇒
          // FIXME Closure
          logger.info(s"$orderId detached from Agent")
          self ! Internal.OrderDetached(orderId)
          sender ! Output.OrderDetached(orderId)
        case Failure(t) ⇒
          logger.error(s"$cmd: ${t.toStringWithCauses}")
      }
  }

  private def processQueuedCommands(): Unit = {
    for (cmd ← commandQueue.find(o ⇒ !executingCommands(o))) {
      executingCommands += cmd
      (for (_ ← client.executeCommand(AddJobnet(cmd.jobnet));
            _ ← client.executeCommand(AddOrder(cmd.order)))
        yield Done)
        .onComplete {
          tried ⇒ self ! Internal.OrderAttachedToAgent(cmd, tried)
        }
    }
  }

  override def toString = s"AgentDriver($agentPath: $uri)"
}

private[master] object AgentDriver {

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
    case object Start
    final case class Recover(lastAgentEventId: EventId, orderIds: Iterable[OrderId])
    final case class AttachOrder(order: Order[Order.Idle], jobnet: Jobnet) extends Input
    final case class DetachOrder(orderId: OrderId) extends Input
  }

  object Output {
    final case class EventFromAgent(stamped: Stamped[AnyKeyedEvent])
    final case object Recovered
    final case class OrderDetached(orderId: OrderId)
  }

  private object Internal {
    final case object Connect
    final case object Connected
    final case class ConnectFailed(throwable: Throwable)
    final case object LoggedOut
    final case object Ready
    final case class OrderAttachedToAgent(command: Input.AttachOrder, result: Try[Done])
    final case object CommandReady
    final case class OrderDetached(orderId: OrderId)
    final case class AgentEvent(stamped: Stamped[KeyedEvent[OrderEvent]])
    final case class EventFetcherTerminated(completed: Try[Completed]) extends DeadLetterSuppression
  }
}
