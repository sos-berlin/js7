package js7.common.pekkohttp

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.{Pipe, Stream}
import io.circe.Encoder
import io.circe.syntax.EncoderOps
import izumi.reflect.Tag
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.StreamExtensions.mapParallelBatch
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.http.PekkoHttpClient.{HttpHeartbeatByteString, `x-js7-request-id`}
import js7.common.http.StreamingSupport.*
import js7.common.http.{JsonStreamingSupport, PekkoHttpClient}
import js7.common.pekkohttp.StandardDirectives.ioRoute
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkoutils.ByteStrings.syntax.*
import org.apache.pekko.http.scaladsl.marshalling.{ToResponseMarshallable, ToResponseMarshaller}
import org.apache.pekko.http.scaladsl.model.headers.Accept
import org.apache.pekko.http.scaladsl.model.{ContentType, HttpEntity, HttpHeader, MediaType, Uri}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.PathMatcher.{Matched, Unmatched}
import org.apache.pekko.http.scaladsl.server.{ContentNegotiator, Directive, Directive0, Directive1, MalformedHeaderRejection, MissingHeaderRejection, PathMatcher, PathMatcher0, Route, UnacceptedResponseContentTypeRejection, ValidationRejection}
import org.apache.pekko.util.ByteString
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
object PekkoHttpServerUtils:

  private val logger = Logger[this.type]

  object implicits:
    implicit final class RichOption[A](private val delegate: Option[A]) extends AnyVal:
      def applyRoute(f: A => Route): Route =
        delegate match
          case Some(a) => f(a)
          case None => reject

  // Test via ConcurrentRequestsLimiterTest
  //def whenResponseTerminated(onTerminated: Try[RouteResult] => Unit)(implicit arf: ActorRefFactory): Directive0 = {
  //  import arf.dispatcher /*use Pekko's ExecutionContext*/
  //  mapInnerRoute {
  //    _ andThen {
  //      _ transform {
  //        case Success(complete @ Complete(response)) =>
  //          Success(Complete(
  //            response mapEntity {
  //              case entity if entity.isKnownEmpty || entity.isInstanceOf[HttpEntity.Strict] =>
  //                onTerminated(Success(complete))
  //                entity
  //              case entity =>
  //                entity.transformDataBytes(Flow[ByteString].watchTermination() { case (NotUsed, whenTerminated) =>
  //                  whenTerminated.map((_: Done) => complete).onComplete(onTerminated)
  //                  NotUsed
  //                })
  //            }))
  //        case o =>
  //          onTerminated(o)
  //          o
  //      }
  //    }
  //  }
  //}

  def accept(mediaType: MediaType, mediaTypes: MediaType*): Directive0 =
    accept(mediaTypes.toSet + mediaType)

  def accept(mediaTypes: Set[MediaType]): Directive0 =
    mapInnerRoute { inner =>
      headerValueByType(Accept):
        case Accept(requestedMediaTypes) if requestedMediaTypes exists { o => mediaTypes exists o.matches } =>
          inner
        case _ =>
          reject(UnacceptedResponseContentTypeRejection(
            mediaTypes.collect[ContentNegotiator.Alternative] {
              case m: MediaType.WithOpenCharset => ContentNegotiator.Alternative.MediaType(m)
              case m: MediaType.WithFixedCharset => ContentNegotiator.Alternative.ContentType(m)
            }))
    }

  /**
    * Passes x iff argument is Some(x).
    */
  def passSome[A](option: Option[A]): Directive1[A] =
    option match
      case Some(o) => Directive(inner => inner(Tuple1(o)))
      case None => reject

  def passRight[R](either: Either[String, R]): Directive1[R] =
    either match
      case Right(r) => Directive(inner =>inner(Tuple1(r)))
      case Left(message) => reject(ValidationRejection(message))

  /**
    * Passes x iff argument is true.
    */
  def passIf(condition: Boolean): Directive0 =
    if condition then
      pass
    else
      reject

  def addHeader(header: HttpHeader): Directive0 =
    mapRequest(o => o.withHeaders(o.headers :+ header))

  /*
  private type ParameterMap = Map[String, String]

  private def eatParameterOption(parameterMap: ParameterMap, key: String) =
    new Directive[ParameterMap :: Option[String] :: HNil] {
      override def tapply(inner: (ParameterMap :: Option[String] :: HNil) => Route) =
        inner((parameterMap - key) :: parameterMap.get(key) :: HNil)
    }

  private def eatParameter(parameterMap: ParameterMap, key: String) =
    new Directive[ParameterMap :: String :: HNil] {
      override def tapply(inner: (ParameterMap :: String :: HNil) => Route) =
        parameterMap.get(key) match {
          case Some(value) => inner((parameterMap - key) :: value :: HNil)
          case None => reject
        }
    }

  def removeParameters(keys: Set[String]): Directive0 =
    mapRequest { request =>
      request.copy(uri = request.uri.copy(query = removeKeysFromQuery(keys, request.uri.query)))
    }

  private def removeKeysFromQuery(keys: Set[String], query: Uri.Query): Uri.Query = {
    query match {
      case Uri.Query.Empty => Uri.Query.Empty
      case q @ Uri.Query.Cons(key, value, tail, keep) =>
        if (keys contains key)
          removeKeysFromQuery(keys, tail)
        else
          Uri.Query.Cons(key, value, removeKeysFromQuery(keys, tail), keep)
      case q: Uri.Query.Raw => q
    }
  }

  def noParameters(keys: Set[String]): Directive0 =
    mapInnerRoute { inner =>
      requestUri { uri =>
        if (uri.query.isEmpty)
          inner
        else
          reject(ValidationRejection(s"Invalid parameters: ${keys mkString ", "}"))
      }
    }
*/

  def emptyParameterMap(parameterMap: Map[String, String]) =
    if parameterMap.isEmpty then
      pass
    else
      reject(ValidationRejection(s"Invalid parameters: ${parameterMap.keys mkString ", "}"))

  //implicit def asFromStringOptionDeserializer[A](implicit stringAsA: As[String, A]) =
  //  simpleFromStringOptionDeserializer(stringAsA.apply)
  //
  //implicit val DurationFromStringOptionDeserializer: FromStringOptionDeserializer[Duration] =
  //  simpleFromStringOptionDeserializer(parseDuration)
  //
  //final def simpleFromStringOptionDeserializer[A](fromString: String => A) =
  //  new FromStringOptionDeserializer[A] {
  //    def apply(value: Option[String]): Deserialized[A] =
  //      value match {
  //        case Some(string) =>
  //          try Right(fromString(string))
  //          catch {
  //            case NonFatal(t) => Left(new MalformedContent(t.toSimplifiedString, Some(t)))
  //          }
  //        case None =>
  //          Left(ContentExpected)
  //      }
  //  }

  implicit final class RichPath(private val delegate: Uri.Path) extends AnyVal:
    import Uri.Path.*

    /**
      * Matches complete segments (not characters, as `startWith`).
      */
    @tailrec
    def startsWithPath(prefix: Uri.Path): Boolean =
      (delegate, prefix) match
        case (Slash(a), Slash(b)) => a startsWithPath b
        case (Segment(aHead, aTail), Segment(bHead, bTail)) => aHead == bHead && (aTail startsWithPath bTail)
        case _ => prefix.isEmpty

    @tailrec
    def drop(n: Int): Uri.Path =
      if n == 0 then
        delegate
      else
        require(n > 0)
        delegate.tail drop n - 1

  /** Like `pathPrefix`, but `prefix` denotes a path segment. */
  def pathSegment(segment: String): Directive0 =
    pathPrefix(matchSegment(segment))

  private def matchSegment(segment: String): PathMatcher0 =
    new SegmentPathMatcher(segment)

  private class SegmentPathMatcher(segment: String) extends PathMatcher0:
    def apply(path: Uri.Path) = path match
      case Uri.Path.Segment(`segment`, tail) =>
        Matched(tail, ())
      case _ =>
        Unmatched

  /**
    * Like `pathPrefix`, but `prefix` denotes a path of complete path segments.
    */
  def pathSegments(prefix: String): Directive0 =
    if prefix.isEmpty then
      pass
    else
      pathSegments(Uri.Path(prefix))

  /**
    * Like `pathPrefix`, but matches complete path segments.
    */
  def pathSegments(prefix: Uri.Path): Directive0 =
    pathPrefix(matchSegments(prefix))

  /**
    * A `PathMatcher` for Directive `pathPrefix` matching complete segments.
    */
  private def matchSegments(prefix: Uri.Path): PathMatcher0 =
    import org.apache.pekko.http.scaladsl.server.PathMatcher.*
    if prefix.isEmpty then
      provide(())
    else
      new PathMatcher[Unit]:
        def apply(path: Uri.Path) =
          if path startsWithPath prefix then
            Matched(path drop prefix.length, ())
          else
            Unmatched

  def completeWithCheckedJsonStream[A: Encoder: Tag](chunkSize: Int, prefetch: Int = 0)
    (stream: IO[Checked[Stream[IO, A]]])
    (using IORuntime)
  : Route =
    completeWithCheckedStream(`application/x-ndjson`):
      stream.map(_.map:
        _.through(encodeParallel(httpChunkSize = chunkSize, prefetch = prefetch)))

  def completeWithCheckedStream(contentType: ContentType)
    (checkedStream: IO[Checked[Stream[IO, ByteString]]])
    (using IORuntime)
  : Route =
    ioRoute:
      checkedStream.flatMap:
        case Left(problem) => IO.pure(complete(problem))
        case Right(stream) => completeWithIOStream(contentType)(stream)

  def completeWithByteStream(contentType: ContentType)(stream: Stream[IO, Byte])
    (using IORuntime)
  : Route =
    completeWithStream(contentType):
      stream.chunks.map(_.toByteString)

  def completeWithStream(contentType: ContentType)(stream: Stream[IO, ByteString])
    (using IORuntime)
  : Route =
    accept(contentType.mediaType):
      ioRoute:
        completeWithIOStream(contentType)(stream)

  def completeWithIOStream(contentType: ContentType)(stream: Stream[IO, ByteString]): IO[Route] =
    stream.toPekkoSourceForHttpResponseX.map: source =>
      complete(HttpEntity(contentType, source))

  def completeIO[A: ToResponseMarshaller](io: IO[A])(using IORuntime): Route =
    optionalAttribute(WebLogDirectives.CorrelIdAttributeKey):
      case None =>
        complete:
          io.unsafeToFuture()

      case Some(correlId) =>
        complete:
          import js7.base.log.CanBindCorrelId.future
          correlId.bind {
            io.unsafeToFuture()
          } (future)

  val extractJs7RequestId: Directive1[Long] =
    new Directive1[Long]:
      def tapply(inner: Tuple1[Long] => Route) =
        extractRequest(_
          .headers
          .find(_.is(`x-js7-request-id`.lowercaseName))
          .match {
            case None => reject(MissingHeaderRejection(`x-js7-request-id`.toString))
            case Some(h) =>
              `x-js7-request-id`
                .parseNumber(h.value())
                .match {
                  case Left(problem) =>
                    reject(MalformedHeaderRejection(`x-js7-request-id`.toString, problem.toString))
                  case Right(n) =>
                    inner(Tuple1(n))
                }
          })

  // Was observableToResponseMarshallable in v2.6
  def encodeAndHeartbeat[A: Encoder : Tag](
    httpChunkSize: Int,
    keepAlive: FiniteDuration,
    prefetch: Int = 0)
    (using IORuntime)
  : Pipe[IO, A, ByteString] =
    _.through(encodeParallel(httpChunkSize = httpChunkSize, prefetch = prefetch))
      .keepAlive(keepAlive, IO.pure(HttpHeartbeatByteString))

  def encodeParallel[A: Encoder : Tag](httpChunkSize: Int, prefetch: Int = 0)
    (using IORuntime)
  : Pipe[IO, A, ByteString] =
    _.mapParallelBatch(/*responsive = true,*//* prefetch = prefetch*/)(_
        .asJson
        .toByteSequence[fs2.Chunk[Byte]] ++ fs2.Chunk.singleton('\n'.toByte))
      .unchunks
      .chunkLimit(httpChunkSize)
      .map(_.toByteSequence[ByteString])
