package js7.common.akkahttp

import akka.NotUsed
import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller, ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.model.MediaTypes.`text/plain`
import akka.http.scaladsl.model.{ContentType, HttpEntity, HttpResponse, MediaType, StatusCode}
import akka.http.scaladsl.unmarshalling.{FromStringUnmarshaller, Unmarshaller}
import akka.stream.scaladsl.Source
import akka.util.ByteString
import js7.base.monixutils.MonixBase.closeableIteratorToObservable
import js7.base.problem.{Checked, Problem}
import js7.base.utils.CloseableIterator
import js7.base.utils.ScalaUtils.RichThrowable
import js7.common.http.CirceJsonSupport.jsonMarshaller
import js7.common.http.StreamingSupport.AkkaObservable
import js7.common.scalautil.Logger
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.concurrent.duration._
import scala.language.implicitConversions
import scala.reflect.runtime.universe._

/**
  * @author Joacim Zschimmer
  */
object StandardMarshallers
{
  private val Nl = ByteString("\n")
  private val logger = Logger(getClass)

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

  def closeableIteratorToMarshallable[A: ToEntityMarshaller: TypeTag](closeableIterator: CloseableIterator[A])
    (implicit s: Scheduler, q: Source[A, NotUsed] => ToResponseMarshallable)
  : ToResponseMarshallable =
    // Detour through Monix Observable to handle stream close
    monixObservableToMarshallable(closeableIteratorToObservable(closeableIterator))

  def monixObservableToMarshallable[A: TypeTag](observable: Observable[A])
    (implicit s: Scheduler, q: Source[A, NotUsed] => ToResponseMarshallable)
  : ToResponseMarshallable =
    logAkkaStreamErrorToWebLogAndIgnore(observable.toAkkaSource)

  def logAkkaStreamErrorToWebLogAndIgnore[A: TypeTag](source: Source[A, NotUsed]): Source[A, NotUsed] =
    source.recoverWithRetries(1, { case throwable =>
      // These are the only messages logged
      val isDebug = throwable.isInstanceOf[akka.stream.AbruptTerminationException]
      val msg = s"Terminating stream Source[${implicitly[TypeTag[A]].tpe}] due to error: ${throwable.toStringWithCauses}"
      if (isDebug) ExceptionHandling.webLogger.debug(msg) else ExceptionHandling.webLogger.warn(msg)
      if (throwable.getStackTrace.nonEmpty) logger.debug(msg, throwable)

      // Letting the throwable pass would close the connection,
      // and the HTTP client sees only: The request's encoding is corrupt:
      // The connection closed with error: Connection reset by peer.
      // => So it seems best to end the stream silently.
      Source.empty
    })

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
}
