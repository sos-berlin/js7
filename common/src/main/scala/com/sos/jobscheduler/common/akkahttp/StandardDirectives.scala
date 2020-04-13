package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.model.DateTime
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model.headers.CacheDirectives.{`max-age`, `public`, immutableDirective}
import akka.http.scaladsl.model.headers.{ETag, `Cache-Control`, `Last-Modified`}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.PathMatcher.{Matched, Unmatched}
import akka.http.scaladsl.server.{Directive0, PathMatcher1, Route}
import com.sos.jobscheduler.base.BuildInfo
import com.sos.jobscheduler.base.problem.{Checked, CheckedString}
import com.sos.jobscheduler.base.utils.Collections.implicits._
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
object StandardDirectives
{
  /**
    * A PathMatcher that matches a single segment or the whole remaining path,
    * treating encoded slashes (%2F) like unencoded ones.
    * "a/b" ~ "a%2Fb"
    */
  def remainingSegmentOrPath[P](implicit P: CheckedString[P]): PathMatcher1[P] =
    new PathMatcher1[P] {
      def apply(path: Path) =
        uriPathToTypedPath[P](path) match {
          case Right(typedPath) => Matched(Path.Empty, Tuple1(typedPath))
          case _ => Unmatched
        }
    }

  private def uriPathToTypedPath[P](uriPath: Path)(implicit P: CheckedString[P]): Checked[P] =
    uriPath match {
      case Path.Segment(segment, Path.Empty) if segment startsWith "/" =>
        P.checked(segment)  // Slashes encoded as %2F in a single path segment
      case _ =>
        P.checked("/" + uriPath.toString.stripPrefix("/"))   // Slashes not encoded, first slash optional (to avoid /api/xxx//path)
    }

  def combineRoutes(routes: Iterable[Route]): Route =
    routes.foldFast(reject)(_ ~ _)

  def lazyRoute(lazyRoute: => Route): Route =
    ctx => Future.successful(lazyRoute(ctx)).flatten

  def routeTask(routeTask: Task[Route])(implicit s: Scheduler): Route =
    routeFuture(routeTask.runToFuture)

  def routeFuture(routeFuture: Future[Route])(implicit ec: ExecutionContext): Route =
    ctx => routeFuture flatMap (_(ctx))

  private val removeEtag: Directive0 =
    mapResponse(r => r.withHeaders(r.headers filter {
      case _: ETag => false
      case _ => true
    }))

  private val resetLastModifiedToBuildTime: Directive0 = {
    val immutableLastModified = Some(`Last-Modified`(DateTime(BuildInfo.buildTime)))
    mapResponse(r => r.withHeaders(r.headers flatMap {
      case _: `Last-Modified` => immutableLastModified
      case o => Some(o)
    }))
  }

  val immutableResource: Directive0 =
    respondWithHeader(`Cache-Control`(`public`, `max-age`(365*24*3600), `immutableDirective`)) &
      removeEtag  // Assure client about immutability
}
