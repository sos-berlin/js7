package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.model.StatusCodes.TooManyRequests
import akka.http.scaladsl.server.{Directive0, RequestContext, Route, RouteResult}
import cats.syntax.show._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.whenResponseTerminated
import com.sos.jobscheduler.common.akkahttp.ConcurrentRequestLimiter._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.time.ScalaTime._
import java.util.concurrent.ConcurrentLinkedQueue
import monix.execution.{Cancelable, Scheduler}
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
final class ConcurrentRequestLimiter(limit: Int, rejectWithProblem: Problem, timeout: FiniteDuration = Duration.Zero, queueSize: Int = 0)
  (implicit scheduler: Scheduler)
extends Directive0
{
  @volatile private var busy = 0
  private val queue = new ConcurrentLinkedQueue[Entry]()

  def tapply(inner: Unit => Route) = requestContext => {
    def execute() = whenResponseTerminated(_ => onExecuted()).apply(inner(()))(requestContext)
    val maybeRouteResultFuture =
      queue.synchronized {
        if (busy < limit) {
          busy += 1
          None
        } else if (queue.size >= queueSize) {
          Some(completeWithTooManyRequests(requestContext))
        } else {
          busy += 1
          val promise = Promise[RouteResult]()
          val timer = scheduler.scheduleOnce(timeout)(onTimeout(requestContext, promise))
          queue.add(Entry(execute, promise, timer))
          Some(promise.future)
        }
      }
    maybeRouteResultFuture getOrElse execute()
  }

  private def onTimeout(requestContext: RequestContext, promise: Promise[RouteResult]): Unit = {
    queue.synchronized {
      busy -= 1
      queue.removeIf(_.promise == promise)
    }
    promise.tryCompleteWith(completeWithTooManyRequests(requestContext))
  }


  private def completeWithTooManyRequests(requestContext: RequestContext) =
    requestContext.complete(TooManyRequests -> rejectWithProblem)

  private def onExecuted(): Unit = {
    val maybeEntry =
      queue.synchronized {
        busy -= 1
        queue.poll() match {
          case null =>
            None
          case entry =>
            entry.timer.cancel()
            Some(entry)
        }
      }
    for (e <- maybeEntry) {
      e.promise.completeWith(e.execute())
    }
  }

  def isBusy = busy >= limit

  override def toString = s"ConcurrentRequestLimiter(limit=$limit,timeout=${timeout.show},busy=$busy)"
}

object ConcurrentRequestLimiter
{
  private case class Entry(
    execute: () => Future[RouteResult],
    promise: Promise[RouteResult],
    timer: Cancelable)
}
