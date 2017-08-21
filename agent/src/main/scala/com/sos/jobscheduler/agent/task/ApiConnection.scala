package com.sos.jobscheduler.agent.task

import akka.util.ByteString
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
trait ApiConnection extends AutoCloseable {
  def request(request: ByteString): Future[ByteString]
}
