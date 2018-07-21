package com.sos.jobscheduler.core.event.journal

import akka.actor.{Actor, ActorRef, Terminated}
import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.SnapshotWriter._
import com.sos.jobscheduler.core.event.journal.write.ParallelExecutingPipeline
import io.circe.Encoder
import scala.collection.immutable
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
private[journal] final class SnapshotWriter(write: ByteString ⇒ Unit, journalingActors: immutable.Iterable[ActorRef], jsonEncoder: Encoder[Any])
extends Actor {

  private var remaining = journalingActors.size
  private var snapshotCount = 0
  private val pipeline = new ParallelExecutingPipeline[ByteString](write)

  self ! Internal.Start

  def receive = {
    case Internal.Start ⇒
      if (journalingActors.isEmpty) {
        end()
      } else {
        for (a ← journalingActors) {
          context.watch(a)
          a ! JournalingActor.Input.GetSnapshot
        }
      }

    case Terminated(a) ⇒
      logger.debug(s"${a.path} terminated while taking snapshot")
      countDown()

    case JournalingActor.Output.GotSnapshot(snapshots) ⇒
      context.unwatch(sender())
      abortOnError {
        for (snapshot ← snapshots) {
          pipeline.blockingAdd { ByteString(jsonEncoder(snapshot).compactPrint) }
          logger.trace(s"Stored $snapshot")  // Without sync
          snapshotCount += 1
        }
        countDown()
      }
  }

  private def countDown(): Unit = {
    remaining -= 1
    if (remaining == 0) {
      end()
    }
  }

  private def end(): Unit = {
    pipeline.flush()
    context.parent ! Output.Finished(Success(snapshotCount))
    context.stop(self)
  }

  private def abortOnError[A](body: ⇒ A): Unit = {
    try body
    catch {
      case NonFatal(t) ⇒
        logger.error(t.toStringWithCauses)
        context.parent ! Output.Finished(Failure(t))
        context.stop(self)
    }
  }
}

private[journal] object SnapshotWriter {
  private val logger = Logger(getClass)

  object Output {
    final case class Finished(done: Try[Int])
  }

  private object Internal {
    final case object Start
  }
}
