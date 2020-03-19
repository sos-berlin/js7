package com.sos.jobscheduler.common.http

import akka.http.scaladsl.coding.{Coder, Deflate, Gzip, NoCoding, StreamDecoder}
import akka.http.scaladsl.model.headers.HttpEncodings.gzip
import akka.http.scaladsl.model.headers.{HttpEncoding, HttpEncodings, `Accept-Encoding`}
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, ResponseEntity, Uri => AkkaUri}
import akka.stream.Materializer
import akka.util.ByteString
import com.sos.jobscheduler.data.common.Uri
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scodec.bits.ByteVector

/**
  * @author Joacim Zschimmer
  */
object AkkaHttpUtils
{
  def encodeGzip(request: HttpRequest): HttpRequest =
    Gzip.encodeMessage(request.copy(headers = `Accept-Encoding`(gzip) :: request.headers.toList))

  /** Decompresses a `HttpResponse` according to `Content-Encoding`. */
  def decodeResponse(response: HttpResponse): HttpResponse =
    httpEncodingToCodec(response.encoding).decodeMessage(response)

  private def httpEncodingToCodec(encoding: HttpEncoding): Coder with StreamDecoder =
    encoding match {
      case HttpEncodings.gzip => Gzip
      case HttpEncodings.deflate => Deflate
      case HttpEncodings.identity => NoCoding
      case o => throw new RuntimeException(s"Unsupported Encoding: $o")
    }

  private implicit final class RichResponseEntity(private val underlying: ResponseEntity) extends AnyVal {
    // TODO Fail if Content-Type is not a (UTF-8 or other) String.
    // TODO Parameter maxLength to truncateWithEllipsis.
    /**
      * Returns the HttpResponse content interpreted as UTF-8, ignoring any Content-Type.
      * May return a very big String.
      */
    def utf8StringFuture(timeout: FiniteDuration)(implicit mat: Materializer, ec: ExecutionContext): Future[String] =
      byteStringFuture(timeout) map (_.utf8String)  // Possible OutOfMemoryError

    /**
      * Returns the HttpResponse content as a `Future[ByteString]`.
      * May return a very big ByteString.
      */
    def byteStringFuture(timeout: FiniteDuration)(implicit mat: Materializer, ec: ExecutionContext): Future[ByteString] =
      underlying.toStrict(timeout).flatMap(_.dataBytes.runFold(ByteString.empty)(_ ++ _))  // Possible OutOfMemoryError
  }

  implicit final class RichHttpResponse(private val underlying: HttpResponse) extends AnyVal {
    /**
      * Returns the HttpResponse content interpreted as UTF-8, ignoring any Content-Type.
      * May return a very big String.
      */
    def utf8StringFuture(implicit mat: Materializer, ec: ExecutionContext): Future[String] =
      underlying.entity.utf8StringFuture(99.seconds)

    /**
      * Returns the HttpResponse content as a `Future[ByteString]`.
      * May return a very big ByteString.
      */
    def byteStringFuture(timeout: FiniteDuration)(implicit mat: Materializer, ec: ExecutionContext): Future[ByteString] =
      underlying.entity.byteStringFuture(99.seconds)
  }

  implicit final class ScodecByteString(private val underlying: ByteString) extends AnyVal {
    def toByteVector: ByteVector =
      if (underlying.isCompact)
        ByteVector.view(underlying.asByteBuffer)
      else {
        val a = new Array[Byte](underlying.length)
        underlying.copyToArray(a)
        ByteVector.view(a)
      }
  }

  implicit final class AkkaByteVector(private val underlying: ByteVector) extends AnyVal {
    def toByteString = ByteString.fromArrayUnsafe(underlying.toArray)
  }

  implicit final class RichAkkaUri(private val underlying: Uri) extends AnyVal {
    def asAkka = AkkaUri(underlying.string)
  }

  implicit final class RichAkkaAsUri(private val underlying: AkkaUri) extends AnyVal {
    def asUri = Uri(underlying.toString)
  }
}
