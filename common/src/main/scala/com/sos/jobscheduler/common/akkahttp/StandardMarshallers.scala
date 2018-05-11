package com.sos.jobscheduler.common.akkahttp

import akka.NotUsed
import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller, ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.model.MediaTypes.`text/plain`
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.model.{ContentType, HttpEntity, HttpResponse, MediaType}
import akka.stream.scaladsl.Source
import akka.util.ByteString
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import com.sos.jobscheduler.common.akkahttp.StreamingSupport.AkkaObservable
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
object StandardMarshallers
{
  private val ProblemStatusCode = BadRequest
  private val Nl = ByteString("\n")

  val StringMarshaller: ToEntityMarshaller[String] =
    Marshaller.withOpenCharset(`text/plain`) { (string, charset) ⇒
      HttpEntity(`text/plain` withCharset charset, ByteString.fromString(string, charset.nioCharset))
    }

  implicit def monixTaskToResponseMarshallable[A: ToResponseMarshaller](task: Task[A])(implicit s: Scheduler): ToResponseMarshallable =
    task.runAsync

  implicit def monixObservableMarshallable[A: ToEntityMarshaller](observable: Observable[A])
    (implicit s: Scheduler, q: Source[A, NotUsed] ⇒ ToResponseMarshallable)
  : ToResponseMarshallable =
    observable.toAkkaSource

  implicit val problemToEntityMarshaller: ToEntityMarshaller[Problem] =
    Marshaller.oneOf(
      stringMarshaller[Problem](`text/plain`, _.toString),
      jsonOrYamlMarshaller[Problem])

  implicit val problemToResponseMarshaller: ToResponseMarshaller[Problem] =
    problemToEntityMarshaller map (entity ⇒ HttpResponse.apply(ProblemStatusCode, Nil, entity))

  def stringMarshaller[A](mediaType: MediaType.WithOpenCharset, toString: A ⇒ String): ToEntityMarshaller[A] =
    Marshaller.withOpenCharset(mediaType) { (a, charset) ⇒
      var byteString = ByteString(toString(a), charset.nioCharset)
      if (!byteString.endsWith(Nl)) byteString ++= Nl   // Append \n to terminate curl output properly
      HttpEntity.Strict(ContentType(mediaType, charset), byteString)
    }

  implicit def checkedToResponseMarshaller[A: ToResponseMarshaller]: ToResponseMarshaller[Checked[A]] =
    Marshaller {
      implicit ec ⇒ {
        case Valid(a) ⇒
          implicitly[ToResponseMarshaller[A]].apply(a)
        case Invalid(problem) ⇒
          problemToResponseMarshaller(problem)
      }
    }
}
