package js7.subagent

import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.Route
import cats.effect.Resource
import js7.base.utils.AsyncLock
import js7.common.akkahttp.StandardMarshallers.*
import js7.common.akkahttp.web.data.WebServerBinding
import js7.data.subagent.Problems.NoDirectorProblem
import js7.subagent.DirectorRouteVariable.*
import monix.eval.Task
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration.Deadline

/** Store for the Agent Director's Route while running. */
private final class DirectorRouteVariable {
  private val lock = AsyncLock()
  private var _toRoute: ToRoute = noDirector
  private val cache = mutable.Map.empty[WebServerBinding, Route]

  def registeringRouteResource(toRoute: ToRoute): Resource[Task, Unit] =
    Resource.make(
      acquire = lock.lock(Task {
        if (_toRoute ne noDirector) throw new IllegalStateException(
          "registeringRouteResource called twice")
        _toRoute = toRoute
      }))(
      release = _ => lock.lock(Task {
        _toRoute = noDirector
        cache.clear()
      }))

  def route(binding: WebServerBinding, whenTerminated: Future[Deadline]): Task[Route] =
    lock.lock/*readlock!*/(Task.defer(
      cache.get(binding) match {
        case None =>
          _toRoute(binding, whenTerminated)
            .tapEval(route => Task {
              cache(binding) = route
            })

        case Some(route) => Task.pure(route)
      }))
}

object DirectorRouteVariable {
  type ToRoute = (WebServerBinding, Future[Deadline]) => Task[Route]

  private val noDirector: ToRoute =
    (_, _) => Task.pure(complete(NoDirectorProblem))
}
