package com.sos.jobscheduler.common.http

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.HttpCharsets.`UTF-8`
import akka.http.scaladsl.model.{HttpEntity, MediaType}
import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils.implicits.CompactPrinter
import io.circe.Encoder
import io.circe.syntax.EncoderOps

/**
  * Helper JSON Sequence RFC 7464.
  * @see https://tools.ietf.org/html/rfc7464
  */
object CirceJsonSeqSupport {

  val `application/json-seq`: MediaType.WithFixedCharset = MediaType.customWithFixedCharset("application", "json-seq", `UTF-8`)

  def jsonSeqMarshaller[A: Encoder]: ToEntityMarshaller[A] =
    Marshaller.withFixedContentType(`application/json-seq`)(value ⇒
      HttpEntity.Strict(`application/json-seq`, ByteString(CompactPrinter.pretty(value.asJson))))
}
