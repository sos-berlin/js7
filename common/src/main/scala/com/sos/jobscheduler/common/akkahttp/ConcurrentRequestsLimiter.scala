package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.model.StatusCodes.TooManyRequests
import akka.http.scaladsl.server.{Directive0, Route}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.whenResponseTerminated
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import monix.execution.atomic.AtomicInt
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
final class ConcurrentRequestsLimiter(limit: Int, rejectWithProblem: Problem)(implicit ec: ExecutionContext) extends Directive0
{
  private val busy = AtomicInt(0)

  def tapply(inner: Unit ⇒ Route) = requestContext ⇒
    if (busy.incrementAndGet() > limit) {
      busy -= 1
      requestContext.complete(TooManyRequests → rejectWithProblem)
    } else
      whenResponseTerminated(_ ⇒ busy -= 1).apply(inner(()))(requestContext)

  def isBusy = busy.get >= limit

  override def toString = s"ConcurrentRequestsLimiter(limit=$limit,busy=$busy)"
}
