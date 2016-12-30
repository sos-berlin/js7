package com.sos.scheduler.engine.common.event.collector

import com.sos.scheduler.engine.common.sprayutils.SprayUtils._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.data.event._
import shapeless.{::, HNil}
import spray.routing.Directives._
import spray.routing._

/**
  * @author Joacim Zschimmer
  */
object EventDirectives {

  def eventRequest[E <: Event: KeyedTypedEventJsonFormat](eventSuperclass: Class[E], defaultReturnType: Option[String] = None): Directive1[SomeEventRequest[E]] =
    new Directive1[SomeEventRequest[E]] {
      def happly(inner: SomeEventRequest[E] :: HNil ⇒ Route) =
        parameter("return".?) {
          _ orElse defaultReturnType match {
            case Some(returnType) ⇒
              passSome(implicitly[KeyedTypedEventJsonFormat[E]].typeNameToClass.get(returnType)) {
                case eventClass_ if eventSuperclass isAssignableFrom eventClass_ ⇒ eventRequestRoute(eventClass_, inner)
                case eventClass_ if eventClass_ isAssignableFrom eventSuperclass ⇒ eventRequestRoute(eventSuperclass, inner)
                case _ ⇒ reject
              }
            case None ⇒
              reject
          }
        }
    }

  private def eventRequestRoute[E <: Event](eventClass: Class[_ <: E], inner: SomeEventRequest[E] :: HNil ⇒ Route): Route = {
    val eClass = eventClass.asInstanceOf[Class[E]]
    parameter("limit" ? Int.MaxValue) {
      case 0 ⇒
        reject(ValidationRejection(s"Invalid limit=0"))
      case limit if limit > 0 ⇒
        parameter("after".as[EventId]) { after ⇒
          parameter("timeout" ? 0.s) { timeout ⇒
            inner(EventRequest(eClass, after = after, timeout, limit = limit) :: HNil)
          }
        }
      case limit if limit < 0 ⇒
        parameter("after" ? EventId.BeforeFirst) { after ⇒
          inner(ReverseEventRequest(eClass, after = after, limit = -limit) :: HNil)
        }
    }
  }
}
