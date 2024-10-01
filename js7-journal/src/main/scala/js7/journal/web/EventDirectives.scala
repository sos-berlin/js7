package js7.journal.web

import cats.syntax.option.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.implicitClass
import js7.common.pekkohttp.StandardMarshallers.*
import js7.data.event.*
import org.apache.pekko.http.scaladsl.model.headers.`Timeout-Access`
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.{Directive, Directive1, Route, ValidationRejection}
import scala.concurrent.duration.*
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
object EventDirectives:

  val MinimumDelay: FiniteDuration = 100.ms
  private val PekkoTimeoutTolerance = 1.s  // To let event reader timeout before Pekko

  def eventRequest[E <: Event](
    defaultAfter: Option[EventId] = None,
    defaultTimeout: FiniteDuration,
    minimumDelay: FiniteDuration,
    defaultReturnType: Option[String] = None)
    (implicit keyedEventTypedJsonCodec: KeyedEventTypedJsonCodec[E],
      classTag: ClassTag[E])
  : Directive1[EventRequest[E]] =
    Directive(inner =>
      parameter("return".?):
        _ orElse defaultReturnType match
          case None => reject(ValidationRejection("Missing parameter return="))
          case Some(returnType) =>
            val eventSuperclass = implicitClass[E]
            val returnTypeNames = if returnType.isEmpty then Set.empty else returnType.split(',').toSet
            val eventClasses =
              returnTypeNames.flatMap: t =>
                keyedEventTypedJsonCodec.typenameToClassOption(t)
              .collect:
                case eventClass_ if eventSuperclass isAssignableFrom eventClass_ => eventClass_
                case eventClass_ if eventClass_ isAssignableFrom eventSuperclass => eventSuperclass
            if eventClasses.size != returnTypeNames.size then
              reject(ValidationRejection(s"Unrecognized event type: return=$returnType"))
            else
              eventRequestRoute[E](eventClasses, defaultAfter, defaultTimeout, minimumDelay, inner))

  private def eventRequestRoute[E <: Event](
    eventClasses: Set[Class[? <: E]],
    defaultAfter: Option[EventId],
    defaultTimeout: FiniteDuration,
    minimumDelay: FiniteDuration,
    inner: Tuple1[EventRequest[E]] => Route)
  : Route =
    parameter("limit" ? Int.MaxValue): limit =>
      if limit <= 0 then
        reject(ValidationRejection(s"Invalid limit=$limit"))
      else
        parameter("after".as[EventId].?):
          _.orElse(defaultAfter) match
            case None => reject(ValidationRejection("Missing parameter after="))
            case Some(after) =>
              parameter("timeout" ? (defaultTimeout: Duration)): timeout =>
                val maybeTimeout = timeout match
                  case o: FiniteDuration => Some(o)
                  case _/*Duration.Inf only*/ => None
                parameter("delay" ? minimumDelay): delay =>
                  parameter("tornOlder" ? none[FiniteDuration]): tornOlder =>
                    optionalHeaderValueByType(`Timeout-Access`): timeoutAccess =>  // Setting pekko.http.server.request-timeout
                      inner(Tuple1:
                        EventRequest[E](eventClasses,
                          after = after,
                          timeout =
                            val maybePekko = timeoutAccess.map(_.timeoutAccess.timeout).collect:
                              case t: FiniteDuration => t
                            (maybeTimeout, maybePekko) match
                              case (Some(timeout), Some(pekko)) =>
                                Some(timeout min pekko - PekkoTimeoutTolerance)
                              case (None, Some(t)) => Some(t)
                              case (t, None) => t,
                          delay = delay max minimumDelay,
                          limit = limit,
                          tornOlder = tornOlder))
