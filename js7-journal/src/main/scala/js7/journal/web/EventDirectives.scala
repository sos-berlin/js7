package js7.journal.web

import akka.http.scaladsl.model.headers.`Timeout-Access`
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.{Directive, Directive1, Route, ValidationRejection}
import cats.syntax.option.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.implicitClass
import js7.common.akkahttp.StandardMarshallers.*
import js7.data.event.*
import scala.concurrent.duration.*
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
object EventDirectives:
  val DefaultTimeout = 0.s
  val DefaultDelay = 500.ms
  val MinimumDelay = 100.ms
  private val AkkaTimeoutTolerance = 1.s  // To let event reader timeout before Akka

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
    Directive(inner =>
      parameter("return".?) {
        _ orElse defaultReturnType match {
          case None => reject(ValidationRejection("Missing parameter return="))
          case Some(returnType) =>
            val eventSuperclass = implicitClass[E]
            val returnTypeNames = if returnType.isEmpty then Set.empty else returnType.split(',').toSet
            val eventClasses = returnTypeNames flatMap { t =>
              keyedEventTypedJsonCodec.typenameToClassOption(t)
            } collect {
              case eventClass_ if eventSuperclass isAssignableFrom eventClass_ => eventClass_
              case eventClass_ if eventClass_ isAssignableFrom eventSuperclass => eventSuperclass
            }
            if eventClasses.size != returnTypeNames.size then
              reject(ValidationRejection(s"Unrecognized event type: return=$returnType"))
            else
              eventRequestRoute[E](eventClasses, defaultAfter, defaultTimeout, defaultDelay, inner)
        }
      })

  private def eventRequestRoute[E <: Event](
    eventClasses: Set[Class[? <: E]],
    defaultAfter: Option[EventId],
    defaultTimeout: FiniteDuration,
    defaultDelay: FiniteDuration,
    inner: Tuple1[EventRequest[E]] => Route)
  : Route =
    parameter("limit" ? Int.MaxValue) { limit =>
      if limit <= 0 then
        reject(ValidationRejection(s"Invalid limit=$limit"))
      else
        parameter("after".as[EventId].?):
          _ orElse defaultAfter match
            case None => reject(ValidationRejection("Missing parameter after="))
            case Some(after) =>
              parameter("timeout" ? (defaultTimeout: Duration)) { timeout =>
                val maybeTimeout = timeout match
                  case o: FiniteDuration => Some(o)
                  case _/*Duration.Inf only*/ => None
                parameter("delay" ? defaultDelay) { delay =>
                  parameter("tornOlder" ? none[FiniteDuration]) { tornOlder =>
                    optionalHeaderValueByType(`Timeout-Access`) { timeoutAccess =>  // Setting akka.http.server.request-timeout
                      val eventRequest = EventRequest[E](eventClasses,
                        after = after,
                        timeout = timeoutAccess.map(_.timeoutAccess.timeout) match {
                          case Some(akkaTimeout: FiniteDuration) =>
                            maybeTimeout.map(t =>
                              if akkaTimeout > AkkaTimeoutTolerance && t > akkaTimeout - AkkaTimeoutTolerance then
                                t - AkkaTimeoutTolerance  // Requester's timeout before Akkas akka.http.server.request-timeout
                              else t)
                          case _ => maybeTimeout
                        },
                        delay = delay max (defaultDelay min MinimumDelay),
                        limit = limit, tornOlder = tornOlder)
                      inner(Tuple1(eventRequest))
                    }
                  }
                }
              }
    }
