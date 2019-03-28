package com.sos.jobscheduler.core.web

import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import com.sos.jobscheduler.common.akkahttp.JsonStreamingSupport
import com.sos.jobscheduler.data.event.EventId

/**
  * @author Joacim Zschimmer
  */
object StampedStreamingSupport
{
  def stampedCirceStreamingSupport(eventId: EventId): JsonEntityStreamingSupport =
    EntityStreamingSupport
      .json(maxObjectLength = JsonStreamingSupport.JsonObjectMaxSize)
      .withParallelMarshalling(parallelism = sys.runtime.availableProcessors, unordered = false)
      .withFramingRenderer(Flow[ByteString].intersperse(
        ByteString(s"""{"eventId":$eventId,"array":["""),
        ByteString(","),
        ByteString("]}")))
}
