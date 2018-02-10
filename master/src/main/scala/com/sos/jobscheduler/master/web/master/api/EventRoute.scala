package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.data.event.{Event, SomeEventRequest}
import com.sos.jobscheduler.master.KeyedEventJsonCodecs.MasterKeyedEventJsonCodec.keyedEventJsonCodec
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
trait EventRoute {
  protected def eventCollector: EventCollector
  protected def eventIdGenerator: EventIdGenerator
  protected implicit def executionContext: ExecutionContext

  final val eventRoute: Route =
    pathEnd {
      get {
        eventRequest[Event](defaultReturnType = Some("Event")).apply {
          case request: SomeEventRequest[Event] ⇒
            intelliJuseImport(jsonOrYamlMarshaller)
            complete {
              eventIdGenerator.stampTearableEventSeq {
                eventCollector.byPredicate[Event](request, predicate = _ ⇒ true)
              }
            }
          case _ ⇒
            reject
        }
      }
    }
}
