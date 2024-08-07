package js7.common.http

import cats.effect.IO
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.web.Uri
import js7.common.http.StreamingSupport.*
import js7.common.pekkoutils.ByteStrings.syntax.*
import org.apache.pekko.http.scaladsl.coding.Coder
import org.apache.pekko.http.scaladsl.coding.Coders.{Deflate, Gzip, NoCoding}
import org.apache.pekko.http.scaladsl.model.headers.HttpEncodings.gzip
import org.apache.pekko.http.scaladsl.model.headers.{HttpEncoding, HttpEncodings, `Accept-Encoding`}
import org.apache.pekko.http.scaladsl.model.{HttpEntity, HttpRequest, HttpResponse, Uri as PekkoUri}
import org.apache.pekko.stream.Materializer
import org.apache.pekko.util.ByteString

/**
  * @author Joacim Zschimmer
  */
object PekkoHttpUtils:
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
    def asUtf8String(implicit m: Materializer): IO[String] =
      asByteString.map(_.utf8String)

    // TODO Fail if Content-Type is not a (UTF-8 or other) String.
    // TODO Parameter maxLength to truncateWithEllipsis.
    // May throw OutOfMemoryError
    def asByteString(implicit m: Materializer): IO[ByteString] =
      underlying
        .withoutSizeLimit
        .dataBytes
        .asFs2Stream()
        .foldMonoid
        .head
        .compile
        .lastOrError
        .logWhenItTakesLonger("HttpEntity.asByteString")

  implicit final class RichHttpResponse(private val underlying: HttpResponse) extends AnyVal:
    /**
      * Returns the HttpResponse content interpreted as UTF-8, ignoring any Content-Type.
      * May return a very big String.
      */
    def utf8String(implicit mat: Materializer): IO[String] =
      underlying.entity.asUtf8String

    /**
      * Returns the HttpResponse content as a `Future[ByteString]`.
      * May return a very big ByteString.
      */
    def byteString(implicit mat: Materializer): IO[ByteString] =
      underlying.entity.asByteString

  implicit final class RichPekkoUri(private val underlying: Uri) extends AnyVal:
    def asPekko: PekkoUri =
      PekkoUri(underlying.string)

  implicit final class RichPekkoAsUri(private val underlying: PekkoUri) extends AnyVal:
    def asUri: Uri =
      Uri(underlying.toString)
