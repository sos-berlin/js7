package js7.core.web

import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import js7.common.http.JsonStreamingSupport
import js7.data.event.EventId

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
