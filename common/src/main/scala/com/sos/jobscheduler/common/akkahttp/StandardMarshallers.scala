package com.sos.jobscheduler.common.akkahttp

import akka.NotUsed
import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller, ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.model.MediaTypes.`text/plain`
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.model.{ContentType, HttpEntity, HttpResponse, MediaType}
import akka.http.scaladsl.unmarshalling.{FromStringUnmarshaller, Unmarshaller}
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.http.CirceJsonSupport.jsonMarshaller
import com.sos.jobscheduler.common.http.StreamingSupport.AkkaObservable
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.closeableIteratorToObservable
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
  private val ProblemStatusCode = BadRequest
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
    logErrorToWebLog(observable.toAkkaSource)

  def logErrorToWebLog[A](source: Source[A, NotUsed]): Source[A, NotUsed] =
    source.mapError { case throwable =>
      val msg = s"Exception in Akka stream: ${throwable.toStringWithCauses}"
      // This area the only messages logged
      ExceptionHandling.webLogger.warn(msg)
      logger.debug(msg, throwable)
      throwable
      // HTTP client sees only: The request's encoding is corrupt: The connection closed with error: Connection reset by peer
    }

  implicit val problemToEntityMarshaller: ToEntityMarshaller[Problem] =
    Marshaller.oneOf(
      stringMarshaller[Problem](`text/plain`, _.toString),
      jsonMarshaller[Problem](Problem.typedJsonEncoder))  // Add "TYPE": "Problem"

  implicit val problemToResponseMarshaller: ToResponseMarshaller[Problem] =
    problemToEntityMarshaller map (entity => HttpResponse(ProblemStatusCode, Nil, entity))

  implicit def problemToResponseMarshallable(problem: Problem): ToResponseMarshallable =
    ToResponseMarshallable(ProblemStatusCode -> problem)

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
