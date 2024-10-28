package js7.journal.web

import cats.syntax.option.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.implicitClass
import js7.common.pekkohttp.StandardMarshallers.*
import js7.data.event.*
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.{Directive, Directive1, Route, ValidationRejection}
import scala.concurrent.duration.*
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
object EventDirectives:

  val MinimumDelay: FiniteDuration = 100.ms

  def eventRequest[E <: Event](
    defaultAfter: Option[EventId] = None,
    minimumDelay: FiniteDuration,
    defaultReturnType: Option[String] = None)
    (using keyedEventTypedJsonCodec: KeyedEventTypedJsonCodec[E],
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
                case eventClass_ if eventSuperclass.isAssignableFrom(eventClass_) => eventClass_
                case eventClass_ if eventClass_.isAssignableFrom(eventSuperclass) => eventSuperclass
            if eventClasses.size != returnTypeNames.size then
              reject(ValidationRejection(s"Unrecognized event type: return=$returnType"))
            else
              eventRequestRoute[E](eventClasses, defaultAfter, minimumDelay, inner))

  private def eventRequestRoute[E <: Event](
    eventClasses: Set[Class[? <: E]],
    defaultAfter: Option[EventId],
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
              parameter("timeout".as[Duration].?): timeout_ =>
                val timeout = timeout_.collect:
                  case o: FiniteDuration => o // Ignore Duration.Inf
                parameter("delay" ? minimumDelay): delay =>
                  parameter("tornOlder" ? none[FiniteDuration]): tornOlder =>
                    inner(Tuple1:
                      EventRequest[E](eventClasses,
                        after = after,
                        timeout = timeout,
                        delay = delay max minimumDelay,
                        limit = limit,
                        tornOlder = tornOlder))
