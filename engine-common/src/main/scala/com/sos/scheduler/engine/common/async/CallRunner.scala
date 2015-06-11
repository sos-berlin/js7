package com.sos.scheduler.engine.common.async

import CallRunner._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.time.Instant._
import scala.annotation.tailrec

final class CallRunner(val queue: PoppableCallQueue) extends Runnable {

  @volatile private var ended = false

  def run(): Unit = {
    while (!ended) {
      sleep(nextTime - now().toEpochMilli)
      executeMatureCalls()
    }
  }

  def end(): Unit = {
    queue add EndCall
  }

  def executeMatureCalls(n: Long = Long.MaxValue): Boolean = {
    val somethingDone = queue.isMature

    @tailrec def f(n: Long): Unit = {
      if (n == 0) {
        queue.matureHeadOption foreach { o => logger debug s"Interrupted after $n calls. next=$o" }
      } else {
        queue.popMature() match {
          case None =>
          case Some(EndCall) =>
            ended = true
          case Some(o) =>
            o.onApply()
            f(n - 1)
        }
      }
    }

    f(n)
    somethingDone
  }

  def nextTime =
    queue.nextTime

  override def toString =
    s"${getClass.getSimpleName} with $queue"
}


object CallRunner {
  private val logger = Logger(getClass)

  object EndCall extends ShortTermCall[Unit] {
    def call(): Unit = {
      throw new UnsupportedOperationException
    }
  }
}
