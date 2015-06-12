package com.sos.scheduler.engine.agent.web

import akka.actor.ActorRefFactory
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait RouteStandards {
  implicit def actorRefFactory: ActorRefFactory

  protected final val agentStandard = decompressRequest() & compressResponseIfRequested(()) & pathPrefix("jobscheduler")
}
