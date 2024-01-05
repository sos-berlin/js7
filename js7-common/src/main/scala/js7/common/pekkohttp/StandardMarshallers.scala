package js7.common.pekkohttp

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource}
import fs2.Stream
import io.circe.Encoder
import io.circe.syntax.*
import izumi.reflect.Tag
import js7.base.circeutils.CirceUtils.*
import js7.base.fs2utils.StreamExtensions.*
import js7.base.generic.GenericString
import js7.base.problem.{Checked, Problem}
import js7.common.http.StreamingSupport.*
import js7.common.pekkohttp.CirceJsonSupport.jsonMarshaller
import js7.common.pekkoutils.ByteStrings.syntax.*
import org.apache.pekko.NotUsed
import org.apache.pekko.http.scaladsl.marshalling.{Marshaller, Marshalling, ToEntityMarshaller, ToResponseMarshallable, ToResponseMarshaller}
import org.apache.pekko.http.scaladsl.model.ContentTypes.`application/json`
import org.apache.pekko.http.scaladsl.model.MediaTypes.`text/plain`
import org.apache.pekko.http.scaladsl.model.StatusCodes.OK
import org.apache.pekko.http.scaladsl.model.{ContentType, HttpEntity, HttpResponse, MediaType, StatusCode}
import org.apache.pekko.http.scaladsl.unmarshalling.{FromStringUnmarshaller, Unmarshaller}
import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.util.ByteString
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
object StandardMarshallers:

  private val Nl = ByteString("\n")

  val StringMarshaller: ToEntityMarshaller[String] =
    Marshaller.withOpenCharset(`text/plain`) { (string, charset) =>
      HttpEntity(`text/plain` withCharset charset, ByteString.fromString(string, charset.nioCharset))
    }

  implicit val finiteDurationParamMarshaller: FromStringUnmarshaller[FiniteDuration] =
    Unmarshaller.strict(stringToFiniteDuration)

  implicit def genericStringParamMarshaller[A <: GenericString](
    implicit A: GenericString.Companion[A])
  : FromStringUnmarshaller[A] =
    Unmarshaller.strict(A.apply(_))

  implicit val durationParamMarshaller: FromStringUnmarshaller[Duration] =
    Unmarshaller.strict:
      case "infinite" => Duration.Inf
      case o => stringToFiniteDuration(o)

  private def stringToFiniteDuration(string: String) =
    (BigDecimal(string) * 1000).toLong.millis

  def monixObservableToMarshallable[A: Tag](stream: Stream[IO, A])
    (implicit toMarshallable: Source[A, NotUsed] => ToResponseMarshallable)
  : Resource[IO, ToResponseMarshallable] =
    stream.toPekkoSourceForHttpResponse.map(toMarshallable)

  private def observableToJsonArrayHttpEntity[A: Encoder: Tag](stream: Stream[IO, A])
    (using IORuntime)
  : Resource[IO, HttpEntity.Chunked] =
    stream
      .parEvalMapUnbounded(o => IO:
        o.asJson.toByteSequence[ByteString])
      //??? Monix .mapParallelBatch()(o => o.asJson.toByteSequence[ByteString])
      .prependOne(ByteString("["))
      .appendOne(ByteString("]"))
      .intersperse(ByteString(","))
      .toPekkoSourceForHttpResponse
      .map(HttpEntity(`application/json`, _))

  implicit val problemToEntityMarshaller: ToEntityMarshaller[Problem] =
    Marshaller.oneOf(
      stringMarshaller[Problem](`text/plain`, _.toString),
      jsonMarshaller[Problem](Problem.typedJsonEncoder))  // Add "TYPE": "Problem"

  implicit val problemToResponseMarshaller: ToResponseMarshaller[Problem] =
    Marshaller(implicit ec => problem =>
      problemToEntityMarshaller(problem)
        .map(_.map(_.map(HttpResponse(problem.httpStatusCode, Nil, _)))))

  implicit def problemToResponseMarshallable(problem: Problem): ToResponseMarshallable =
    ToResponseMarshallable((problem.httpStatusCode: StatusCode) -> problem)

  def stringMarshaller[A](mediaType: MediaType.WithOpenCharset, toString: A => String): ToEntityMarshaller[A] =
    Marshaller.withOpenCharset(mediaType) { (a, charset) =>
      var byteString = ByteString(toString(a), charset.nioCharset)
      if !byteString.endsWith(Nl) then byteString ++= Nl   // Append \n to terminate curl output properly
      HttpEntity.Strict(ContentType(mediaType, charset), byteString)
    }

  implicit def checkedToResponseMarshaller[A: ToResponseMarshaller]: ToResponseMarshaller[Checked[A]] =
    Marshaller:
      implicit ec => {
        case Right(a) =>
          implicitly[ToResponseMarshaller[A]].apply(a)
        case Left(problem) =>
          problemToResponseMarshaller(problem)
      }

  implicit val unitToResponseMarshaller: ToResponseMarshaller[Unit] =
    Marshaller:
      _ => _ => Future.successful(List(Marshalling.Opaque(() => HttpResponse(OK))))
