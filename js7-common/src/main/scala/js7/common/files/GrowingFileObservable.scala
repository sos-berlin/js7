package js7.common.files

import java.nio.file.{Files, Path}
import js7.base.data.ByteArray
import monix.execution.Ack.{Continue, Stop}
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber
import scala.concurrent.duration.FiniteDuration

final class GrowingFileObservable(file: Path, pollDuration: Option[FiniteDuration] = None)(implicit scheduler: Scheduler)
extends Observable[ByteArray]
{
  def unsafeSubscribeFn(subscriber: Subscriber[ByteArray]): Cancelable = {
    @volatile var cancelled = false
    val reader = new ByteArrayReader(file, fromEnd = pollDuration.isDefined)

    def continue(): Unit =
      if cancelled then {
        reader.close()
      } else {
        val chunk = reader.read()
        if chunk.isEmpty then {
          // End of file reached
          pollDuration match {
            case Some(delay) if chunk.isEmpty =>
              if Files.exists(file) then {
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

    def complete(): Unit = {
      try reader.close()
      finally subscriber.onComplete()
    }

    continue()

    () => cancelled = true
  }
}
