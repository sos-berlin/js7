package com.sos.scheduler.engine.tunnel.server

import akka.actor.ActorRef
import akka.agent.Agent
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.{Logger, SetOnce}
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.common.utils.Exceptions._
import com.sos.scheduler.engine.http.server.heartbeat.HeartbeatService
import com.sos.scheduler.engine.tunnel.data.{TunnelStatistics, TunnelToken, TunnelView}
import com.sos.scheduler.engine.tunnel.server.Handle._
import java.net.{InetAddress, InetSocketAddress}
import java.time.{Duration, Instant}
import scala.concurrent.{ExecutionContext, Promise}

/**
  * @author Joacim Zschimmer
  */
private[server] class Handle(
  connectorHandler: ActorRef,
  val tunnelToken: TunnelToken,
  val startedByHttpIpOption: Option[InetAddress],
  val connectedPromise: Promise[InetSocketAddress],
  val listener: Agent[TunnelListener],
  var state: Handle.State = Handle.Uninitialized,
  val statistics: Statistics = Statistics())
  (implicit timerService: TimerService)
extends TunnelHandle {

  private val startedAt = Instant.now
  val heartbeatService = new HeartbeatService
  private val onInactivityCallback = new SetOnce[Instant ⇒ Unit]

  def close(): Unit = connectorHandler ! ConnectorHandler.CloseTunnel(tunnelToken)

  def request(requestMessage: ByteString, timeout: Option[Duration]) = {
    val responsePromise = Promise[ByteString]()
    connectorHandler ! ConnectorHandler.DirectedRequest(tunnelToken, requestMessage, responsePromise, timeout)
    responsePromise.future
  }

  override def toString = s"TunnelHandler($id,HTTP client ${startedByHttpIpOption getOrElse "unknown"} -> TCP server ${serverAddressOption getOrElse "not yet connected"})"

  private def serverAddressOption: Option[InetSocketAddress] = connectedPromise.future.value map { _.get }

  def connected = connectedPromise.future

  def remoteAddressString: String = remoteAddressStringOption getOrElse "(not connected via TCP)"

  def remoteAddressStringOption: Option[String] = connectedPromise.future.value flatMap { _.toOption } map { _.toString stripPrefix "/" }

  def onRequest(request: Connector.Request)(implicit ec:ExecutionContext): Unit = {
    statistics.updateWith(request)
    listener send { _.onRequest(request.message) }
  }

  def onInactivity(callback: Instant ⇒ Unit) = {
    onInactivityCallback := callback
  }

  private[server] def callOnInactivity(since: Instant): Unit = {
    for (callback ← onInactivityCallback) {
      ignoreException(logger.error) {
        callback(since)
      }
    }
  }

  def view = TunnelView(
    id,
    startedAt,
    startedByHttpIpOption,
    remoteAddressStringOption,
    heartbeatService.overview,
    TunnelStatistics(
      requestCount = statistics.requestCount,
      messageByteCount = statistics.messageByteCount,
      currentRequestIssuedAt = statistics.currentRequestIssuedAt,
      statistics.failure map { _.toString })  )
}

private object Handle {
  private val logger = Logger(getClass)

  private[server] sealed trait State {
    override def toString = getClass.getSimpleName
  }
  private[server] case object Uninitialized extends State
  private[server] case class RequestBeforeConnected(request: Connector.Request) extends State
  private[server] case class ConnectedConnector(connector: ActorRef) extends State
}
