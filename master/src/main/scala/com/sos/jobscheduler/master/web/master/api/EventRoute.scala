package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.data.event.{Event, EventSeq, KeyedEvent, SomeEventRequest, TearableEventSeq}
import com.sos.jobscheduler.master.configuration.KeyedEventJsonCodecs.MasterKeyedEventJsonCodec.keyedEventJsonCodec
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
trait EventRoute {
  protected def eventCollector: EventCollector
  protected implicit def executionContext: ExecutionContext

  final val eventRoute: Route =
    pathEnd {
      get {
        eventRequest[Event](defaultReturnType = Some("Event")).apply {
          case request: SomeEventRequest[Event] ⇒
              val whenEvents: Future[TearableEventSeq[Seq, KeyedEvent[Event]]] =
                eventCollector.byPredicate[Event](request, predicate = _ ⇒ true) map {
                  case EventSeq.NonEmpty(events) ⇒ EventSeq.NonEmpty(events.toImmutableSeq)
                  case o: EventSeq.Empty ⇒ o
                  case o: TearableEventSeq.Torn ⇒ o
                }
            intelliJuseImport(jsonOrYamlMarshaller)
            complete(whenEvents)
          case _ ⇒
            reject
        }
      }
    }
}
