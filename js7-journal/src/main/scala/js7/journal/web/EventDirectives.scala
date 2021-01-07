package js7.journal.web

import akka.http.scaladsl.model.headers.`Timeout-Access`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive, Directive1, Route, ValidationRejection}
import cats.syntax.option._
import com.google.common.base.Splitter
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.implicitClass
import js7.common.akkahttp.StandardMarshallers._
import js7.data.event._
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
object EventDirectives
{
  val DefaultTimeout = 0.s
  val DefaultDelay = 500.ms
  val MinimumDelay = 100.ms
  private val AkkaTimeoutTolerance = 1.s  // To let event reader timeout before Akka
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
    Directive(inner =>
      parameter("return".?) {
        _ orElse defaultReturnType match {
          case None => reject(ValidationRejection("Missing parameter return="))
          case Some(returnType) =>
            val eventSuperclass = implicitClass[E]
            val returnTypeNames = ReturnSplitter.split(returnType).asScala.toSet
            val eventClasses = returnTypeNames flatMap { t =>
              keyedEventTypedJsonCodec.typenameToClassOption(t)
            } collect {
              case eventClass_ if eventSuperclass isAssignableFrom eventClass_ => eventClass_
              case eventClass_ if eventClass_ isAssignableFrom eventSuperclass => eventSuperclass
            }
            if (eventClasses.size != returnTypeNames.size)
              reject(ValidationRejection(s"Unrecognized event type: return=$returnType"))
            else
              eventRequestRoute[E](eventClasses, defaultAfter, defaultTimeout, defaultDelay, inner)
        }
      })

  private def eventRequestRoute[E <: Event](
    eventClasses: Set[Class[_ <: E]],
    defaultAfter: Option[EventId],
    defaultTimeout: FiniteDuration,
    defaultDelay: FiniteDuration,
    inner: Tuple1[EventRequest[E]] => Route)
  : Route =
    parameter("limit" ? Int.MaxValue) { limit =>
      if (limit <= 0)
        reject(ValidationRejection(s"Invalid limit=$limit"))
      else
        parameter("after".as[EventId].?) {
          _ orElse defaultAfter match {
            case None => reject(ValidationRejection("Missing parameter after="))
            case Some(after) =>
              parameter("timeout" ? (defaultTimeout: Duration)) { timeout =>
                val maybeTimeout = timeout match {
                  case o: FiniteDuration => Some(o)
                  case _/*Duration.Inf only*/ => None
                }
                parameter("delay" ? defaultDelay) { delay =>
                  parameter("tornOlder" ? none[FiniteDuration]) { tornOlder =>
                    optionalHeaderValueByType(`Timeout-Access`) { timeoutAccess =>  // Setting akka.http.server.request-timeout
                      val eventRequest = EventRequest[E](eventClasses,
                        after = after,
                        timeout = timeoutAccess.map(_.timeoutAccess.timeout) match {
                          case Some(akkaTimeout: FiniteDuration) =>
                            maybeTimeout.map(t =>
                              if (akkaTimeout > AkkaTimeoutTolerance && t > akkaTimeout - AkkaTimeoutTolerance)
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
        }
    }
}
