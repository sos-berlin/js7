package js7.common.akkahttp

import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import js7.common.http.JsonStreamingSupport

/**
  * @author Joacim Zschimmer
  */
object EventSeqStreamingSupport:
  val NonEmptyEventSeqJsonStreamingSupport: JsonEntityStreamingSupport =
    EntityStreamingSupport
      .json(maxObjectLength = JsonStreamingSupport.JsonObjectMaxSize)
      .withParallelMarshalling(parallelism = sys.runtime.availableProcessors, unordered = false)
      .withFramingRenderer(Flow[ByteString].intersperse(
        ByteString("""{"TYPE":"NonEmpty","stamped":["""),
        ByteString(","),
        ByteString("]}")))
