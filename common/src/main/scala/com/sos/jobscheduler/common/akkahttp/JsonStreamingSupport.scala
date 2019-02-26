package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.HttpCharsets.`UTF-8`
import akka.http.scaladsl.model.{ContentType, HttpEntity, MediaType}
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import com.google.common.base.Ascii
import com.sos.jobscheduler.base.circeutils.CirceUtils.implicits.CompactPrinter
import io.circe.Encoder
import io.circe.syntax.EncoderOps

/**
  * @author Joacim Zschimmer
  */
object JsonStreamingSupport
{
  val `application/json-seq` = MediaType.customWithFixedCharset("application", "json-seq", `UTF-8`)  // https://tools.ietf.org/html/rfc7464
  val `application/x-ndjson` = MediaType.customWithFixedCharset("application", "x-ndjson", `UTF-8`)  // https://github.com/ndjson/ndjson-spec

  val JsonObjectMaxSize = 100*1000
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
      HttpEntity.Strict(streamingSupport.contentType, ByteString(CompactPrinter.pretty(value.asJson))))
}

