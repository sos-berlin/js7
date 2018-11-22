package com.sos.jobscheduler.agent.task

import akka.util.ByteString
import java.net.InetAddress
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
final class LocalApiConnection(
  onRequest: ByteString ⇒ Future[ByteString],
  val startedByHttpIpOption: Option[InetAddress])
  (implicit executionContext: ExecutionContext)
extends ApiConnection {

  def request(requestMessage: ByteString) = onRequest(requestMessage)

  def close() = {}

  override def toString = "LocalApiConnection"
}
