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
import js7.common.akkautils.ByteStrings.syntax.*

/**
  * @author Joacim Zschimmer
  */
object JsonStreamingSupport
{
  val `application/json-seq` = MediaType.customWithFixedCharset("application", "json-seq", `UTF-8`)  // https://tools.ietf.org/html/rfc7464
  val `application/x-ndjson` = MediaType.customWithFixedCharset("application", "x-ndjson", `UTF-8`)  // https://github.com/ndjson/ndjson-spec
  val `application/x-ndjson-ContentType` = `application/x-ndjson`.toContentType
  val CustomMediaTypes = `application/json-seq` :: `application/x-ndjson` :: Nil

  /** Useable for HTTP request expecting a Checked[Observable] response. */
  val StreamingJsonHeader = Accept(
    MediaRange.One(`application/x-ndjson`, 1.0f),   // For observed items
    MediaRange.One(`application/json`, 0.9f))       // For Problem response
  val StreamingJsonHeaders: List[Accept] =
    StreamingJsonHeader :: Nil

  val JsonObjectMaxSize = 1024*1024  // TODO Maybe 10MB? For very big Workflows or snapshot objects
  private val LF = ByteString(Ascii.LF)

  val NdJsonStreamingSupport = jsonSeqStreamingSupport(`application/x-ndjson`, _ ++ LF)

  private def jsonSeqStreamingSupport(mediaType: MediaType.WithFixedCharset, frame: ByteString => ByteString): JsonEntityStreamingSupport =
    EntityStreamingSupport
      .json(maxObjectLength = JsonObjectMaxSize)
      .withContentType(ContentType(mediaType))
      .withParallelMarshalling(parallelism = sys.runtime.availableProcessors, unordered = false)  // TODO Does this improve performance?
      .withFramingRenderer(Flow[ByteString].map(frame))

  def jsonSeqMarshaller[A: Encoder](implicit streamingSupport: JsonEntityStreamingSupport): ToEntityMarshaller[A] =
    Marshaller.withFixedContentType(streamingSupport.contentType)(value =>
      HttpEntity.Strict(streamingSupport.contentType, value.asJson.toByteSequence[ByteString]))
}
