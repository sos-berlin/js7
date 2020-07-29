package js7.common.http

import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.HttpCharsets.`UTF-8`
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.model.{ContentType, HttpEntity, MediaRange, MediaType}
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import io.circe.Encoder
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.utils.Ascii

/**
  * @author Joacim Zschimmer
  */
object JsonStreamingSupport
{
  val `application/json-seq` = MediaType.customWithFixedCharset("application", "json-seq", `UTF-8`)  // https://tools.ietf.org/html/rfc7464
  val `application/x-ndjson` = MediaType.customWithFixedCharset("application", "x-ndjson", `UTF-8`)  // https://github.com/ndjson/ndjson-spec
  val CustomMediaTypes = `application/json-seq` :: `application/x-ndjson` :: Nil

  /** Useable for HTTP request expecting a Checked[Observable] response. */
  val StreamingJsonHeaders: List[Accept] =
    Accept(
      MediaRange.One(`application/x-ndjson`, 1.0f),   // For observed items
      MediaRange.One(`application/json`, 0.9f)) ::    // For Problem response
    Nil

  val JsonObjectMaxSize = 100*1000  // TODO Maybe 10MB? For very big Workflows or snapshot objects
  private val RS = ByteString(Ascii.RS)
  private val LF = ByteString(Ascii.LF)

  val JsonSeqStreamingSupport = jsonSeqStreamingSupport(`application/json-seq`, RS ++ _ ++ LF)
  val NdJsonStreamingSupport = jsonSeqStreamingSupport(`application/x-ndjson`, _ ++ LF)

  private def jsonSeqStreamingSupport(mediaType: MediaType.WithFixedCharset, frame: ByteString => ByteString): JsonEntityStreamingSupport =
    EntityStreamingSupport
      .json(maxObjectLength = JsonObjectMaxSize)
      .withContentType(ContentType(mediaType))
      .withParallelMarshalling(parallelism = sys.runtime.availableProcessors, unordered = false)
      .withFramingRenderer(Flow[ByteString].map(frame))

  def jsonSeqMarshaller[A: Encoder](implicit streamingSupport: JsonEntityStreamingSupport): ToEntityMarshaller[A] =
    Marshaller.withFixedContentType(streamingSupport.contentType)(value =>
      HttpEntity.Strict(streamingSupport.contentType, ByteString(value.asJson.compactPrint)))
}

