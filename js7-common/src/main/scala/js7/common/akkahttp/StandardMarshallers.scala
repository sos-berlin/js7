package js7.common.akkahttp

import akka.NotUsed
import akka.http.scaladsl.marshalling.{Marshaller, Marshalling, ToEntityMarshaller, ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.model.ContentTypes.`application/json`
import akka.http.scaladsl.model.MediaTypes.`text/plain`
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.{ContentType, HttpEntity, HttpResponse, MediaType, StatusCode}
import akka.http.scaladsl.unmarshalling.{FromStringUnmarshaller, Unmarshaller}
import akka.stream.scaladsl.Source
import akka.util.ByteString
import io.circe.Encoder
import io.circe.syntax._
import js7.base.circeutils.CirceUtils._
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.{Checked, Problem}
import js7.common.akkahttp.CirceJsonSupport.jsonMarshaller
import js7.common.akkautils.ByteStrings.syntax._
import js7.common.http.StreamingSupport.AkkaObservable
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.implicitConversions
import scala.reflect.runtime.universe._

/**
  * @author Joacim Zschimmer
  */
object StandardMarshallers
{
  private val Nl = ByteString("\n")

  val StringMarshaller: ToEntityMarshaller[String] =
    Marshaller.withOpenCharset(`text/plain`) { (string, charset) =>
      HttpEntity(`text/plain` withCharset charset, ByteString.fromString(string, charset.nioCharset))
    }

  implicit val finiteDurationParamMarshaller: FromStringUnmarshaller[FiniteDuration] =
    Unmarshaller.strict(stringToFiniteDuration)

  implicit val durationParamMarshaller: FromStringUnmarshaller[Duration] =
    Unmarshaller.strict {
      case "infinite" => Duration.Inf
      case o => stringToFiniteDuration(o)
    }

  private def stringToFiniteDuration(string: String) =
    (BigDecimal(string) * 1000).toLong.millis

  def monixObservableToMarshallable[A: TypeTag](observable: Observable[A])
    (implicit s: Scheduler, q: Source[A, NotUsed] => ToResponseMarshallable)
  : ToResponseMarshallable =
    observable.toAkkaSourceForHttpResponse

  def observableToJsonArrayHttpEntity[A: Encoder: TypeTag](observable: Observable[A])(implicit s: Scheduler): HttpEntity.Chunked =
    HttpEntity(
      `application/json`,
      observable
        .mapParallelBatch()(o => o.asJson.toByteSequence[ByteString])
        .intersperse(ByteString("["), ByteString(","), ByteString("]"))
        .toAkkaSourceForHttpResponse)

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
      if (!byteString.endsWith(Nl)) byteString ++= Nl   // Append \n to terminate curl output properly
      HttpEntity.Strict(ContentType(mediaType, charset), byteString)
    }

  implicit def checkedToResponseMarshaller[A: ToResponseMarshaller]: ToResponseMarshaller[Checked[A]] =
    Marshaller {
      implicit ec => {
        case Right(a) =>
          implicitly[ToResponseMarshaller[A]].apply(a)
        case Left(problem) =>
          problemToResponseMarshaller(problem)
      }
    }

  implicit val unitToResponseMarshaller: ToResponseMarshaller[Unit] =
    Marshaller {
      _ => _ => Future.successful(List(Marshalling.Opaque(() => HttpResponse(OK))))
    }

}
