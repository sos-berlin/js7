package com.sos.scheduler.engine.tunnel.server

import akka.actor.ActorRef
import akka.agent.Agent
import com.sos.scheduler.engine.common.scalautil.{Logger, SetOnce}
import com.sos.scheduler.engine.common.utils.Exceptions._
import com.sos.scheduler.engine.tunnel.data.TunnelToken
import com.sos.scheduler.engine.tunnel.server.Handle._
import java.net.{InetAddress, InetSocketAddress}
import java.time.Instant
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
extends TunnelHandle {
  private val onInactivityCallback = new SetOnce[Instant ⇒ Unit]

  def close(): Unit = connectorHandler ! ConnectorHandler.CloseTunnel(tunnelToken)

  override def toString = s"TunnelHandler($id,HTTP client ${startedByHttpIpOption getOrElse "unknown"} -> TCP server ${serverAddressOption getOrElse "not yet connected"})"

  private def serverAddressOption: Option[InetSocketAddress] = connectedPromise.future.value map { _.get }

  def connected = connectedPromise.future

  def remoteAddressString: String = remoteAddressStringOption getOrElse "(not connected via TCP)"

  def remoteAddressStringOption: Option[String] = connectedPromise.future.value flatMap { _.toOption } map { _.toString stripPrefix "/" }

  def onRequestSent(request: Connector.Request)(implicit ec:ExecutionContext): Unit = {
    statistics.updateWith(request)
    listener send { _.onRequest(request.message) }
  }

  def heartbeat(): Unit = connectorHandler ! Connector.Heartbeat

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
}

private object Handle {
  private val logger = Logger(getClass)

  sealed trait State
  case object Uninitialized extends State
  case class RequestBeforeConnected(request: Connector.Request) extends State
  case class ConnectedConnector(connector: ActorRef) extends State
}
