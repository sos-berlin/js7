package js7.common.http

import akka.http.scaladsl.coding.Coder
import akka.http.scaladsl.coding.Coders.{Deflate, Gzip, NoCoding}
import akka.http.scaladsl.model.headers.HttpEncodings.gzip
import akka.http.scaladsl.model.headers.{HttpEncoding, HttpEncodings, `Accept-Encoding`}
import akka.http.scaladsl.model.{HttpEntity, HttpRequest, HttpResponse, Uri as AkkaUri}
import akka.stream.Materializer
import akka.util.ByteString
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.web.Uri
import js7.common.akkautils.ByteStrings.syntax.*
import js7.common.http.StreamingSupport.ObservableAkkaSource
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
object AkkaHttpUtils:
  def encodeGzip(request: HttpRequest): HttpRequest =
    Gzip.encodeMessage(request.withHeaders(`Accept-Encoding`(gzip) :: request.headers.toList))

  /** Decompresses a `HttpResponse` according to `Content-Encoding`. */
  def decompressResponse(response: HttpResponse): HttpResponse =
    httpEncodingToCodec(response.encoding).decodeMessage(response)

  private def httpEncodingToCodec(encoding: HttpEncoding): Coder  =
    encoding match
      case HttpEncodings.gzip => Gzip
      case HttpEncodings.deflate => Deflate
      case HttpEncodings.identity => NoCoding
      case o => throw new RuntimeException(s"Unsupported Encoding: $o")

  implicit final class RichResponseEntity(private val underlying: HttpEntity) extends AnyVal:
    def asUtf8String(implicit m: Materializer): Task[String] =
      asByteString.map(_.utf8String)

    // TODO Fail if Content-Type is not a (UTF-8 or other) String.
    // TODO Parameter maxLength to truncateWithEllipsis.
    // May throw OutOfMemoryError
    def asByteString(implicit m: Materializer): Task[ByteString] =
      underlying
        .withoutSizeLimit
        .dataBytes
        .toObservable
        .fold
        .headL
        .logWhenItTakesLonger("HttpEntity.asByteString")

  implicit final class RichHttpResponse(private val underlying: HttpResponse) extends AnyVal:
    /**
      * Returns the HttpResponse content interpreted as UTF-8, ignoring any Content-Type.
      * May return a very big String.
      */
    def utf8String(implicit mat: Materializer): Task[String] =
      underlying.entity.asUtf8String

    /**
      * Returns the HttpResponse content as a `Future[ByteString]`.
      * May return a very big ByteString.
      */
    def byteString(implicit mat: Materializer): Task[ByteString] =
      underlying.entity.asByteString

  implicit final class RichAkkaUri(private val underlying: Uri) extends AnyVal:
    def asAkka = AkkaUri(underlying.string)

  implicit final class RichAkkaAsUri(private val underlying: AkkaUri) extends AnyVal:
    def asUri = Uri(underlying.toString)
