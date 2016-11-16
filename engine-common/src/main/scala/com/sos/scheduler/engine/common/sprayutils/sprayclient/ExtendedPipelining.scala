/*
 * Copyright © 2011-2015 the spray project <http://spray.io>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.sos.scheduler.engine.common.sprayutils.sprayclient

import akka.actor.{ActorRef, ActorRefFactory}
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.{ExecutionContext, Future}
import spray.can.Http
import spray.can.Http.HostConnectorSetup
import spray.http._
import spray.httpx.{RequestBuilding, ResponseTransformation}
import spray.util.actorSystem

/**
  * This is a modified copy of spray.client.pipelining.scala
  * to enable HostConnectorSetup needed for self-signed server certificates.
  */
object ExtendedPipelining extends RequestBuilding with ResponseTransformation {
  type SendReceive = HttpRequest ⇒ Future[HttpResponse]

  def extendedSendReceive(futureTimeout: Timeout, setup: Option[HostConnectorSetup] = None)
    (implicit refFactory: ActorRefFactory, executionContext: ExecutionContext)
  : SendReceive =
    extendedSendReceive(IO(Http)(actorSystem(refFactory)), setup)(executionContext, futureTimeout)

  private def extendedSendReceive(transport: ActorRef, setup: Option[HostConnectorSetup])(implicit ec: ExecutionContext, futureTimeout: Timeout): SendReceive =
    request ⇒
      transport ? (setup map request.→ getOrElse request) map {
        case x: HttpResponse ⇒ x
        case x: HttpResponsePart ⇒ sys.error("sendReceive doesn't support chunked responses, try sendTo instead")
        case x: Http.ConnectionClosed ⇒ sys.error("Connection closed before reception of response: " + x)
        case x ⇒ sys.error("Unexpected response from HTTP transport: " + x)
      }
}
