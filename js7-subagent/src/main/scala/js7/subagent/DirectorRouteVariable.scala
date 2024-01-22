package js7.subagent

import cats.effect.Resource
import js7.base.utils.AsyncLock
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkohttp.web.PekkoWebServer.RouteBinding
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.data.subagent.Problems.NoDirectorProblem
import js7.subagent.DirectorRouteVariable.*
import cats.effect.IO
import org.apache.pekko.http.scaladsl.server.Directives.complete
import org.apache.pekko.http.scaladsl.server.Route
import scala.collection.mutable

/** Store for the Agent Director's Route while running. */
private final class DirectorRouteVariable:
  private val lock = AsyncLock()
  private var _toRoute: ToRoute = noDirector
  private val cache = mutable.Map.empty[WebServerBinding, (Route, Int)]

  def registeringRouteResource(toRoute: ToRoute): Resource[IO, Unit] =
    Resource.make(
      acquire = lock.lock(IO {
        if _toRoute ne noDirector then throw new IllegalStateException(
          "registeringRouteResource called twice")
        _toRoute = toRoute
        cache.clear()
      }))(
      release = _ => lock.lock(IO {
        _toRoute = noDirector
        cache.clear()
      }))

  def route(routeBinding: RouteBinding): IO[Route] =
    lock.lock/*readlock!*/(IO.defer(
      cache.get(routeBinding.webServerBinding) match {
        case Some((route, routeBinding.revision)) =>
          IO.pure(route)

        case _ =>
          _toRoute(routeBinding)
            .flatTap(route => IO {
              cache(routeBinding.webServerBinding) = route -> routeBinding.revision
            })
      }))


object DirectorRouteVariable:
  type ToRoute = RouteBinding => IO[Route]

  private val noDirector: ToRoute =
    _ => IO.pure(complete(NoDirectorProblem))
