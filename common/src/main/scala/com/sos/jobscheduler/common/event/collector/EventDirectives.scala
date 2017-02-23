package com.sos.scheduler.engine.common.event.collector

import com.google.common.base.Splitter
import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import com.sos.scheduler.engine.common.sprayutils.SprayUtils._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.data.event._
import scala.collection.JavaConversions._
import scala.reflect.ClassTag
import shapeless.{::, HNil}
import spray.routing.Directives._
import spray.routing._

/**
  * @author Joacim Zschimmer
  */
object EventDirectives {

  private val ReturnSplitter = Splitter.on(',')

  def eventRequest[E <: Event: KeyedTypedEventJsonFormat: ClassTag]: Directive1[SomeEventRequest[E]] =
    eventRequest[E](None)

  def eventRequest[E <: Event: KeyedTypedEventJsonFormat: ClassTag](defaultReturnType: Option[String] = None): Directive1[SomeEventRequest[E]] =
    new Directive1[SomeEventRequest[E]] {
      def happly(inner: SomeEventRequest[E] :: HNil ⇒ Route) =
        parameter("return".?) {
          _ orElse defaultReturnType match {
            case Some(returnType) ⇒
              val eventSuperclass = implicitClass[E]
              val returnTypeNames = ReturnSplitter.split(returnType).toSet
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

  private def eventRequestRoute[E <: Event](eventClasses: Set[Class[_ <: E]], inner: SomeEventRequest[E] :: HNil ⇒ Route): Route = {
    parameter("limit" ? Int.MaxValue) {
      case 0 ⇒
        reject(ValidationRejection(s"Invalid limit=0"))
      case limit if limit > 0 ⇒
        parameter("after".as[EventId]) { after ⇒
          parameter("timeout" ? 0.s) { timeout ⇒
            inner(EventRequest[E](eventClasses, after = after, timeout, limit = limit) :: HNil)
          }
        }
      case limit if limit < 0 ⇒
        parameter("after" ? EventId.BeforeFirst) { after ⇒
          inner(ReverseEventRequest[E](eventClasses, after = after, limit = -limit) :: HNil)
        }
    }
  }
}
