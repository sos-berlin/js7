package com.sos.jobscheduler.common.event.collector

import akka.http.scaladsl.model.headers.`Timeout-Access`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route, ValidationRejection}
import akka.http.scaladsl.unmarshalling.{FromStringUnmarshaller, Unmarshaller}
import com.google.common.base.Splitter
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.data.event._
import java.util.concurrent.TimeUnit
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
object EventDirectives {

  val DefaultTimeout = 0.seconds
  val DefaultDelay = 500.milliseconds
  val MinimumDelay = 100.milliseconds
  private val AkkaTimeoutTolerance = 5.seconds  // To let event reader timeout before Akka
  private val ReturnSplitter = Splitter.on(',')

  def eventRequest[E <: Event: KeyedEventTypedJsonCodec: ClassTag]: Directive1[EventRequest[E]] =
    eventRequest[E](None)

  def eventRequest[E <: Event](
    defaultAfter: Option[EventId] = None,
    defaultTimeout: FiniteDuration = DefaultTimeout,
    defaultDelay: FiniteDuration = DefaultDelay,
    defaultReturnType: Option[String] = None)
    (implicit keyedEventTypedJsonCodec: KeyedEventTypedJsonCodec[E],
      classTag: ClassTag[E])
  : Directive1[EventRequest[E]] =
    new Directive1[EventRequest[E]] {
      def tapply(inner: Tuple1[EventRequest[E]] ⇒ Route) =
        someEventRequest[E](defaultAfter, defaultTimeout, defaultDelay, defaultReturnType)(keyedEventTypedJsonCodec, classTag) {
          case request: EventRequest[E] ⇒ inner(Tuple1(request))
          case _ ⇒ complete(Problem("Parameter limit must be positive here"))
        }
    }

  def someEventRequest[E <: Event: KeyedEventTypedJsonCodec: ClassTag]: Directive1[SomeEventRequest[E]] =
    someEventRequest[E](None)

  def someEventRequest[E <: Event](
    defaultAfter: Option[EventId] = None,
    defaultTimeout: FiniteDuration = DefaultTimeout,
    defaultDelay: FiniteDuration = DefaultDelay,
    defaultReturnType: Option[String] = None)
    (implicit keyedEventTypedJsonCodec: KeyedEventTypedJsonCodec[E],
      classTag: ClassTag[E])
  : Directive1[SomeEventRequest[E]] =
    new Directive1[SomeEventRequest[E]] {
      def tapply(inner: Tuple1[SomeEventRequest[E]] ⇒ Route) =
        parameter("return".?) {
          _ orElse defaultReturnType match {
            case None ⇒ reject(ValidationRejection("Missing parameter return="))
            case Some(returnType) ⇒
              val eventSuperclass = implicitClass[E]
              val returnTypeNames = ReturnSplitter.split(returnType).asScala.toSet
              val eventClasses = returnTypeNames flatMap { t ⇒
                keyedEventTypedJsonCodec.typenameToClassOption(t)
              } collect {
                case eventClass_ if eventSuperclass isAssignableFrom eventClass_ ⇒ eventClass_
                case eventClass_ if eventClass_ isAssignableFrom eventSuperclass ⇒ eventSuperclass
              }
              if (eventClasses.size != returnTypeNames.size)
                reject(ValidationRejection(s"Unrecognized event type: return=$returnType"))
              else
                eventRequestRoute[E](eventClasses, defaultAfter, defaultTimeout, defaultDelay, inner)
          }
        }
    }

  private implicit val finiteDurationParamMarshaller: FromStringUnmarshaller[FiniteDuration] =
    Unmarshaller.strict(stringToFiniteDuration)

  private implicit val durationParamMarshaller: FromStringUnmarshaller[Duration] =
    Unmarshaller.strict {
      case "infinite" ⇒ Duration.Inf
      case "∞" ⇒ Duration.Inf
      case "-∞" ⇒ Duration.MinusInf
      case "undefined" ⇒ Duration.Undefined
      case o ⇒ stringToFiniteDuration(o)
    }

  private def stringToFiniteDuration(string: String) =
    new FiniteDuration((BigDecimal(string) * 1000).toLong, TimeUnit.MILLISECONDS)

  private def eventRequestRoute[E <: Event](
    eventClasses: Set[Class[_ <: E]],
    defaultAfter: Option[EventId],
    defaultTimeout: Duration,
    defaultDelay: FiniteDuration,
    inner: Tuple1[SomeEventRequest[E]] ⇒ Route)
  : Route =
    parameter("limit" ? Int.MaxValue) {
      case 0 ⇒
        reject(ValidationRejection("Invalid limit=0"))

      case limit if limit > 0 ⇒
        parameter("after".as[EventId].?) {
          _ orElse defaultAfter match {
            case None ⇒ reject(ValidationRejection("Missing parameter after="))
            case Some(after) ⇒
              parameter("timeout" ? defaultTimeout) { timeout ⇒
                parameter("delay" ? defaultDelay) { delay ⇒
                  parameter("tornOlder" ? (Duration.Inf: Duration)) { tornOlder ⇒
                    optionalHeaderValueByType[`Timeout-Access`](()) { h ⇒  // Setting akka.http.server.request-timeout
                      val akkaTimeout = h map (_.timeoutAccess.timeout) match {
                        case Some(o: FiniteDuration) ⇒ if (o > AkkaTimeoutTolerance) o - AkkaTimeoutTolerance else o
                        case _ ⇒ timeout
                      }
                      val eventRequest = EventRequest[E](eventClasses, after = after,
                        timeout = timeout min akkaTimeout, delay = delay max MinimumDelay, limit = limit, tornOlder = tornOlder)
                      inner(Tuple1(eventRequest))
                    }
                  }
                }
              }
          }
        }

      case limit if limit < 0 ⇒
        parameter("after" ? EventId.BeforeFirst) { after ⇒
          inner(Tuple1(ReverseEventRequest[E](eventClasses, after = after, limit = -limit)))
        }
    }
}
