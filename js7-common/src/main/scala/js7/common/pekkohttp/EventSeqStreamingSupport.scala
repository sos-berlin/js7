package js7.common.pekkohttp

import js7.common.http.JsonStreamingSupport
import org.apache.pekko.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import org.apache.pekko.stream.scaladsl.Flow
import org.apache.pekko.util.ByteString

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
