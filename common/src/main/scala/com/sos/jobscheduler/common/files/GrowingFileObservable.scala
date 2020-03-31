package com.sos.jobscheduler.common.files

import java.nio.file.{Files, Path}
import monix.execution.Ack.{Continue, Stop}
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber
import scala.concurrent.duration.FiniteDuration
import scodec.bits.ByteVector

final class GrowingFileObservable(file: Path, pollDelay: Option[FiniteDuration] = None)(implicit scheduler: Scheduler)
extends Observable[ByteVector]
{
  def unsafeSubscribeFn(subscriber: Subscriber[ByteVector]): Cancelable = {
    @volatile var canceled = false
    val reader = new ByteVectorReader(file, fromEnd = pollDelay.isDefined)
    def continue(): Unit =
      if (canceled) {
        reader.close()
      } else {
        val chunk = reader.read()
        if (chunk.isEmpty) {
          // End of file reached
          pollDelay match {
            case Some(delay) if chunk.isEmpty =>
              if (Files.exists(file)) {
                scheduler.scheduleOnce(delay) {
                  continue()
                }
              }
            case _  =>
              complete()
          }
        } else {
          subscriber.onNext(chunk) map {
            case Continue => continue()
            case Stop => complete()
          }
        }
      }

    def complete() {
      try reader.close()
      finally subscriber.onComplete()
    }

    continue()

    () => canceled = true
  }
}
