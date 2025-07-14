package js7.common.web.serviceprovider

import cats.effect.IO
import com.typesafe.config.Config
import js7.data.event.{Event, KeyedEvent, Stamped}
import org.apache.pekko.http.scaladsl.model.StatusCodes.NotFound
import org.apache.pekko.http.scaladsl.server.Directives.complete
import org.apache.pekko.http.scaladsl.server.{Directives, Route}

final case class RouteServiceContext(
  snapshotRoute: SnapshotFilter => Route,
  eventRoute: StampedEventFilter => Route,
  config: Config)


object RouteServiceContext:
  def apply(config: Config): RouteServiceContext =
    RouteServiceContext(_ => complete(NotFound), _ => Directives.complete(NotFound), config)

type SnapshotFilter = fs2.Stream[IO, Any] => fs2.Stream[IO, Any]

type StampedEventFilter =
  fs2.Stream[IO, Stamped[KeyedEvent[Event]]] =>
    fs2.Stream[IO, Stamped[KeyedEvent[Event]]]
