package com.sos.jobscheduler.common.event.collector

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route, ValidationRejection}
import akka.http.scaladsl.unmarshalling.{FromStringUnmarshaller, Unmarshaller}
import com.google.common.base.Splitter
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.common.akkahttp.AkkaHttpUtils._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.event._
import java.time.Duration
import scala.collection.JavaConverters._
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
object EventDirectives {

  private val ReturnSplitter = Splitter.on(',')

  def eventRequest[E <: Event: KeyedTypedEventJsonFormat: ClassTag]: Directive1[SomeEventRequest[E]] =
    eventRequest[E](None)

  def eventRequest[E <: Event: KeyedTypedEventJsonFormat: ClassTag](defaultReturnType: Option[String] = None): Directive1[SomeEventRequest[E]] =
    new Directive1[SomeEventRequest[E]] {
      def tapply(inner: Tuple1[SomeEventRequest[E]] ⇒ Route) =
        parameter("return".?) {
          _ orElse defaultReturnType match {
            case Some(returnType) ⇒
              val eventSuperclass = implicitClass[E]
              val returnTypeNames = ReturnSplitter.split(returnType).asScala.toSet
              val eventClasses = returnTypeNames flatMap { t ⇒
                implicitly[KeyedTypedEventJsonFormat[E]].typeNameToClass.get(t)
              } collect {
                case eventClass_ if eventSuperclass isAssignableFrom eventClass_ ⇒ eventClass_
                case eventClass_ if eventClass_ isAssignableFrom eventSuperclass ⇒ eventSuperclass
              }
              passIf(eventClasses.size == returnTypeNames.size) {  // if every return type is recognized
                eventRequestRoute[E](eventClasses, inner)
              }
            case None ⇒
              reject
          }
        }
    }

  private implicit val durationParamMarshaller: FromStringUnmarshaller[Duration] =
    Unmarshaller.strict(parseDuration)

  private def eventRequestRoute[E <: Event](eventClasses: Set[Class[_ <: E]], inner: Tuple1[SomeEventRequest[E]] ⇒ Route): Route = {
    parameter("limit" ? Int.MaxValue) {
      case 0 ⇒
        reject(ValidationRejection(s"Invalid limit=0"))
      case limit if limit > 0 ⇒
        parameter("after".as[EventId]) { after ⇒
          parameter("timeout" ? 0.s) { timeout ⇒
            inner(Tuple1(EventRequest[E](eventClasses, after = after, timeout, limit = limit)))
          }
        }
      case limit if limit < 0 ⇒
        parameter("after" ? EventId.BeforeFirst) { after ⇒
          inner(Tuple1(ReverseEventRequest[E](eventClasses, after = after, limit = -limit)))
        }
    }
  }
}
