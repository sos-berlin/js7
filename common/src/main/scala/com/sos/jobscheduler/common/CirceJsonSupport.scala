package com.sos.jobscheduler.common

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils.implicits.CompactPrinter
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import io.circe.{Decoder, Encoder, Json, Printer, jawn}

/**
  * Automatic to and from JSON marshalling/unmarshalling using an in-scope circe protocol.
  * The unmarshaller fails fast, throwing the first `Error` encountered.
  *
  * To use automatic codec derivation, user needs to import `io.circe.generic.auto._`.
  */
object CirceJsonSupport {

  implicit final def marshaller[A: Encoder](implicit printer: Printer = CompactPrinter): ToEntityMarshaller[A] =
    jsonMarshaller(printer) compose implicitly[Encoder[A]].apply

  implicit final def jsonMarshaller(implicit printer: Printer = CompactPrinter): ToEntityMarshaller[Json] =
    Marshaller.withFixedContentType(`application/json`) { json =>
      HttpEntity(`application/json`, printer.pretty(json))
    }

  implicit final def unmarshaller[A: Decoder]: FromEntityUnmarshaller[A] =
    jsonUnmarshaller map (json ⇒
      implicitly[Decoder[A]].decodeJson(json).force)

  implicit final val jsonUnmarshaller: FromEntityUnmarshaller[Json] =
    Unmarshaller.byteStringUnmarshaller
      .forContentTypes(`application/json`)
      .map {
        case ByteString.empty ⇒ throw Unmarshaller.NoContentException
        case byteString ⇒ jawn.parseByteBuffer(byteString.asByteBuffer).force
      }
}
