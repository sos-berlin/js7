package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{ActorContext, ActorRef, Cancellable, DeadLetterSuppression}
import com.sos.jobscheduler.agent.scheduler.order.StdouterrToEvent._
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.system.StdoutStderr.{Stderr, Stdout, StdoutStderrType}
import com.typesafe.config.Config
import java.io.Writer
import java.time.Instant.now
import java.time.{Duration, Instant}
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
private[order] class StdouterrToEvent(orderActorContext: ActorContext, config: Config, writeEvent: (StdoutStderrType, String) ⇒ Future[Completed]) {

  import orderActorContext.{dispatcher, self, system}

          val charBufferSize = config.getInt    ("jobscheduler.agent.task.stdouterr.char-buffer-size")
  private val chunkSize = config.getInt         ("jobscheduler.agent.task.stdouterr.chunk-size")
  private val delay = config.as[Duration]       ("jobscheduler.agent.task.stdouterr.delay")
  private val noDelayAfter = config.as[Duration]("jobscheduler.agent.task.stdouterr.no-delay-after")

  private var lastEventAt = Instant.ofEpochMilli(0)
  private var timer: Cancellable = null

  val writers = Map[StdoutStderrType, Writer](
    Stdout → new StdWriter(Stdout, self, size = chunkSize, passThroughSize = chunkSize / 2),
    Stderr → new StdWriter(Stderr, self, size = chunkSize, passThroughSize = chunkSize / 2))

  def onBufferingStarted(): Unit =
    if (timer == null) {
      val d = if (lastEventAt + noDelayAfter < now) 0.s else delay
      timer = system.scheduler.scheduleOnce(
        d.toFiniteDuration, self, Stdouterr.FlushStdoutStderr)
    }

  def finish(): Unit = {
    flushStdoutAndStderr()
    close()
  }

  def handle(msg: Stdouterr): Unit = msg match {
    case Stdouterr.BufferingStarted ⇒
      onBufferingStarted()

    case Stdouterr.FlushStdoutStderr ⇒
      flushStdoutAndStderr()

    case Stdouterr.StdoutStderrWritten(t, chunk, promise) ⇒
      promise.completeWith(writeEvent(t, chunk))
  }

  def flushStdoutAndStderr(): Unit = {
    for (o ← writers.values) o.flush()
    lastEventAt = now
    timer = null
  }

  def close(): Unit =
    if (timer != null) {
      timer.cancel()
      timer = null
    }
}

object StdouterrToEvent {
  private class StdWriter(stdoutOrStderr: StdoutStderrType, orderActorSelf: ActorRef, protected val size: Int, protected val passThroughSize: Int)
    extends BufferedStringWriter {

    def close() = flush()

    protected def onFlush(string: String) = {
      val promise = Promise[Completed]()
      orderActorSelf ! Stdouterr.StdoutStderrWritten(stdoutOrStderr, string, promise)
      promise.future
    }

    protected def onBufferingStarted() =
      orderActorSelf ! Stdouterr.BufferingStarted
  }


  private[order] sealed trait Stdouterr
  private object Stdouterr {
    final case object BufferingStarted extends Stdouterr
    final case class StdoutStderrWritten(typ: StdoutStderrType, chunk: String, completed: Promise[Completed]) extends Stdouterr
    final case object FlushStdoutStderr extends Stdouterr with DeadLetterSuppression  // May arrive after death, due to timer
  }
}

