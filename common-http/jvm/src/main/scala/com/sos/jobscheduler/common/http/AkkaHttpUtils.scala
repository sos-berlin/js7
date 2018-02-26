package com.sos.jobscheduler.common.http

import akka.http.scaladsl.coding.{Coder, Deflate, Gzip, NoCoding, StreamDecoder}
import akka.http.scaladsl.model.headers.HttpEncodings.gzip
import akka.http.scaladsl.model.headers.{HttpEncoding, HttpEncodings, `Accept-Encoding`}
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, ResponseEntity}
import akka.stream.Materializer
import akka.util.ByteString
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
object AkkaHttpUtils {

  def encodeGzip(request: HttpRequest): HttpRequest =
    Gzip.encodeMessage(request.copy(headers = `Accept-Encoding`(gzip) :: request.headers.toList))

  /** Decompresses a `HttpResponse` according to `Content-Encoding`. */
  def decodeResponse(response: HttpResponse): HttpResponse =
    httpEncodingToCodec(response.encoding).decodeMessage(response)

  private def httpEncodingToCodec(encoding: HttpEncoding): Coder with StreamDecoder =
    encoding match {
      case HttpEncodings.gzip ⇒ Gzip
      case HttpEncodings.deflate ⇒ Deflate
      case HttpEncodings.identity ⇒ NoCoding
      case o ⇒ throw new RuntimeException(s"Unsupported Encoding: $o")
    }

  implicit final class RichResponseEntity(private val underlying: ResponseEntity) extends AnyVal {
    // TODO Fail if Content-Type is not a (UTF-8 or other) String.
    // TODO Parameter maxLength to truncateWithEllipsis.
    /**
      * Returns the HttpResponse content interpreted as UTF-8, ignoring any Content-Type.
      * May return a very big String.
      */
    def utf8StringFuture(implicit mat: Materializer, ec: ExecutionContext): Future[String] =
      byteStringFuture map (_.utf8String)  // Possible OutOfMemoryError

    /**
      * Returns the HttpResponse content as a `Future[ByteString]`.
      * May return a very big ByteString.
      */
    def byteStringFuture(implicit mat: Materializer, ec: ExecutionContext): Future[ByteString] =
      underlying.dataBytes.runFold(ByteString.empty)(_ ++ _)  // Possible OutOfMemoryError
  }

  implicit final class RichHttpResponse(private val underlying: HttpResponse) extends AnyVal {
    /**
      * Returns the HttpResponse content interpreted as UTF-8, ignoring any Content-Type.
      * May return a very big String.
      */
    def utf8StringFuture(implicit mat: Materializer, ec: ExecutionContext): Future[String] =
      underlying.entity.utf8StringFuture

    /**
      * Returns the HttpResponse content as a `Future[ByteString]`.
      * May return a very big ByteString.
      */
    def byteStringFuture(implicit mat: Materializer, ec: ExecutionContext): Future[ByteString] =
      underlying.entity.byteStringFuture
  }
}
