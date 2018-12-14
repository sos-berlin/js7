package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.model.StatusCodes.TooManyRequests
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.whenResponseTerminated
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import monix.execution.atomic.AtomicInt
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
final class ConcurrentRequestsLimiter(limit: Int, rejectWithProblem: Problem)(implicit ec: ExecutionContext)
{
  private val busy = AtomicInt(0)

  def apply(route: Route): Route =
    if (busy.incrementAndGet() > limit) {
      busy -= 1
      complete(TooManyRequests → rejectWithProblem)
    } else
      whenResponseTerminated(_ ⇒ busy -= 1).apply {
        route
      }

  def isBusy = busy.get >= limit

  override def toString = s"ConcurrentRequestsLimiter(limit=$limit,busy=$busy)"
}
