package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.model.StatusCodes.TooManyRequests
import akka.http.scaladsl.server.{Directive0, RequestContext, Route, RouteResult}
import cats.syntax.show._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.whenResponseTerminated
import com.sos.jobscheduler.common.akkahttp.ConcurrentRequestLimiter._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.scalautil.Logger
import java.util.concurrent.ConcurrentLinkedQueue
import monix.execution.atomic.AtomicInt
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
  private val queue = new ConcurrentLinkedQueue[Entry]
  private val counter = AtomicInt(0)

  def tapply(inner: Unit => Route) = requestContext => {
    def execute() = whenResponseTerminated(_ => onExecuted()).apply(inner(()))(requestContext)
    val number = counter.incrementAndGet()
    queue.synchronized {
      if (busy < limit) {
        busy += 1
        None  // Leave 'synchronized' then execute()
      } else if (queue.size >= queueSize) {
        Some(completeWithTooManyRequests(requestContext))
      } else {
        busy += 1
        logger.debug(s"limit=$limit busy=$busy Request #$number enqueued: ${requestContext.request.uri}")
        val promise = Promise[RouteResult]()
        val timer = scheduler.scheduleOnce(timeout)(onTimeout(number, requestContext, promise))
        queue.add(Entry(number, () => execute(), promise, timer))
        Some(promise.future)
      }
    }
    .getOrElse(execute())
  }

  private def onTimeout(number: Int, requestContext: RequestContext, promise: Promise[RouteResult]): Unit = {
    queue.synchronized {
      busy -= 1
      if (queue.removeIf(_.promise == promise)) {
        logger.debug(s"limit=$limit busy=$busy Request #$number timed out")
      }
    }
    promise.completeWith(completeWithTooManyRequests(requestContext))
  }

  private def completeWithTooManyRequests(requestContext: RequestContext) =
    requestContext.complete(TooManyRequests -> rejectWithProblem)

  private def onExecuted(): Unit = {
    val maybeEntry = queue.synchronized {
      busy -= 1
      for (entry <- Option(queue.poll())) yield {
        entry.timer.cancel()
        logger.debug(s"limit=$limit busy=$busy Request #${entry.number} dequeued, executes now")
        entry
      }
    }
    for (entry <- maybeEntry) {
      entry.promise.completeWith(entry.execute())
    }
  }

  def isBusy = busy >= limit

  override def toString = s"ConcurrentRequestLimiter(limit=$limit,timeout=${timeout.show},busy=$busy)"
}

object ConcurrentRequestLimiter
{
  private val logger = Logger(getClass)

  private case class Entry(
    number: Int,
    execute: () => Future[RouteResult],
    promise: Promise[RouteResult],
    timer: Cancelable)
}
