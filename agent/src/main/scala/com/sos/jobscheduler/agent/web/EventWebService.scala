package com.sos.jobscheduler.agent.web

import com.sos.jobscheduler.agent.scheduler.event.KeyedEventJsonFormats.AgentKeyedEventJsonFormat
import com.sos.jobscheduler.agent.web.EventWebService._
import com.sos.jobscheduler.agent.web.common.AgentWebService
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.event.collector.EventDirectives._
import com.sos.jobscheduler.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.jobscheduler.common.sprayutils.SprayUtils.pathSegments
import com.sos.jobscheduler.data.event.{Event, KeyedTypedEventJsonFormat}
import scala.concurrent.ExecutionContext
import spray.routing.Directives._

/**
  * @author Joacim Zschimmer
  */
private trait EventWebService extends AgentWebService {
// UNUSED

  protected implicit def executionContext: ExecutionContext
  protected def eventIdGenerator: EventIdGenerator
  protected def eventCollector: EventCollector

  routeBuilder.addApiRoute { _ ⇒
    pathSegments("event") {
      pathEnd {
        eventRequest[Event](defaultReturnType = Some("Event")).apply { request ⇒
          complete {
            eventIdGenerator.wrapInSnapshot {
              eventCollector.byPredicate[Event](
                request,
                predicate = KeyedEventJsonFormat.canSerialize)
            }
          }
        }
      }
    }
  }
}

object EventWebService {
  private val KeyedEventJsonFormat = implicitly[KeyedTypedEventJsonFormat[Event]]
}
