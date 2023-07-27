package js7.subagent

import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.Route
import cats.effect.Resource
import js7.base.utils.AsyncLock
import js7.common.akkahttp.StandardMarshallers.*
import js7.common.akkahttp.web.AkkaWebServer.RouteBinding
import js7.common.akkahttp.web.data.WebServerBinding
import js7.data.subagent.Problems.NoDirectorProblem
import js7.subagent.DirectorRouteVariable.*
import monix.eval.Task
import scala.collection.mutable

/** Store for the Agent Director's Route while running. */
private final class DirectorRouteVariable {
  private val lock = AsyncLock()
  private var _toRoute: ToRoute = noDirector
  private val cache = mutable.Map.empty[WebServerBinding, (Route, Int)]

  def registeringRouteResource(toRoute: ToRoute): Resource[Task, Unit] =
    Resource.make(
      acquire = lock.lock(Task {
        if (_toRoute ne noDirector) throw new IllegalStateException(
          "registeringRouteResource called twice")
        _toRoute = toRoute
        cache.clear()
      }))(
      release = _ => lock.lock(Task {
        _toRoute = noDirector
        cache.clear()
      }))

  def route(routeBinding: RouteBinding): Task[Route] =
    lock.lock/*readlock!*/(Task.defer(
      cache.get(routeBinding.webServerBinding) match {
        case Some((route, routeBinding.revision)) =>
          Task.pure(route)

        case _ =>
          _toRoute(routeBinding)
            .tapEval(route => Task {
              cache(routeBinding.webServerBinding) = route -> routeBinding.revision
            })
      }))
}

object DirectorRouteVariable {
  type ToRoute = RouteBinding => Task[Route]

  private val noDirector: ToRoute =
    _ => Task.pure(complete(NoDirectorProblem))
}
