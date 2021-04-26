package js7.common.http

import akka.http.scaladsl.coding.Coder
import akka.http.scaladsl.coding.Coders.{Deflate, Gzip, NoCoding}
import akka.http.scaladsl.model.headers.HttpEncodings.gzip
import akka.http.scaladsl.model.headers.{HttpEncoding, HttpEncodings, `Accept-Encoding`}
import akka.http.scaladsl.model.{HttpEntity, HttpRequest, HttpResponse, Uri => AkkaUri}
import akka.stream.Materializer
import akka.util.ByteString
import js7.base.monixutils.MonixBase.syntax.RichMonixTask
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import monix.eval.Task
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
object AkkaHttpUtils
{
  def encodeGzip(request: HttpRequest): HttpRequest =
    Gzip.encodeMessage(request.withHeaders(`Accept-Encoding`(gzip) :: request.headers.toList))

  /** Decompresses a `HttpResponse` according to `Content-Encoding`. */
  def decompressResponse(response: HttpResponse): HttpResponse =
    httpEncodingToCodec(response.encoding).decodeMessage(response)

  private def httpEncodingToCodec(encoding: HttpEncoding): Coder  =
    encoding match {
      case HttpEncodings.gzip => Gzip
      case HttpEncodings.deflate => Deflate
      case HttpEncodings.identity => NoCoding
      case o => throw new RuntimeException(s"Unsupported Encoding: $o")
    }

  private implicit final class RichResponseEntity(private val underlying: HttpEntity) extends AnyVal {
    // TODO Fail if Content-Type is not a (UTF-8 or other) String.
    // TODO Parameter maxLength to truncateWithEllipsis.
    /**
      * Returns the HttpResponse content interpreted as UTF-8, ignoring any Content-Type.
      * May return a very big String.
      */
    def utf8String(timeout: FiniteDuration)(implicit mat: Materializer): Task[String] =
      byteString(timeout).map(_.utf8String)  // Possible OutOfMemoryError

    /**
      * Returns the HttpResponse content as a `Future[ByteString]`.
      * May return a very big ByteString.
      */
    def byteString(timeout: FiniteDuration)(implicit mat: Materializer): Task[ByteString] =
      Task
        .deferFutureAction(implicit s =>
          underlying.toStrict(timeout).flatMap(_.dataBytes.runFold(ByteString.empty)(_ ++ _)))  // Possible OutOfMemoryError
        .logWhenItTakesLonger("HttpEntity.bytestring")
  }

  implicit final class RichHttpResponse(private val underlying: HttpResponse) extends AnyVal {
    /**
      * Returns the HttpResponse content interpreted as UTF-8, ignoring any Content-Type.
      * May return a very big String.
      */
    def utf8String(implicit mat: Materializer): Task[String] =
      underlying.entity.utf8String(99.s)

    /**
      * Returns the HttpResponse content as a `Future[ByteString]`.
      * May return a very big ByteString.
      */
    def byteString(timeout: FiniteDuration)(implicit mat: Materializer): Task[ByteString] =
      underlying.entity.byteString(timeout)
  }

  implicit final class RichAkkaUri(private val underlying: Uri) extends AnyVal {
    def asAkka = AkkaUri(underlying.string)
  }

  implicit final class RichAkkaAsUri(private val underlying: AkkaUri) extends AnyVal {
    def asUri = Uri(underlying.toString)
  }
}
