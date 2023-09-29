/*
 * DERIVED FROM Monix BufferTimedObservable
 * Original copyright below.
 */
/*
 * Copyright (c) 2014-2020 by The Monix Project Developers.
 * See the project homepage at: https://monix.io
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package js7.base.monixutils

import js7.base.time.ScalaTime.*
import monix.execution.Ack.{Continue, Stop}
import monix.execution.cancelables.{CompositeCancelable, SerialCancelable}
import monix.execution.{Ack, Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

private final class BufferedObservable[+A](source: Observable[A], timespan: Option[FiniteDuration],
  maxWeight: Long, toWeight: A => Long)
extends Observable[Seq[A]]
{
  for t <- timespan do require(t.isPositive, "timespan must be strictly positive")
  require(maxWeight >= 0, "maxWeight must be positive")

  def unsafeSubscribeFn(out: Subscriber[Seq[A]]): Cancelable = {
    val timer = SerialCancelable()

    val connection = source.unsafeSubscribeFn(new Subscriber[A] with Runnable { self =>
      implicit val scheduler: Scheduler = out.scheduler

      private[this] val timespanMillis = timespan.map(_.toMillis max 1)
      // MUST BE synchronized by `self`
      private[this] var ack: Future[Ack] = Continue
      // MUST BE synchronized by `self`
      private[this] var buffer = ListBuffer.empty[A]
      // MUST BE synchronized by `self`
      private[this] var expiresAt = 0L
      // MUST BE synchronized by `self`
      private[this] var bufferWeight = 0L

      // Called by scheduleOnce via Runnable
      def run(): Unit = self.synchronized {
        val now = scheduler.clockMonotonic(MILLISECONDS)
        // Do we still have time remaining?
        if now < expiresAt then {
          // If we still have time remaining, it's either a scheduler
          // problem, or we rushed to signaling the bundle upon reaching
          // the maximum size in onNext. So we sleep some more.
          val remaining = expiresAt - now
          timer := scheduler.scheduleOnce(remaining, MILLISECONDS, self)
        } else if buffer != null && buffer.nonEmpty then {
          // The timespan has passed since the last signal so we need
          // to send the current bundle
          sendNext()
        }
        ()
      }

      // Must be synchronized by `self`
      private def sendNext(): Future[Ack] = {
        timer := Cancelable.empty
        val oldBuffer = buffer.toList
        // Reset
        buffer = ListBuffer.empty[A]
        bufferWeight = 0
        // Setting the time of the next scheduled tick
        ack = ack.syncTryFlatten.syncFlatMap {
          case Continue => out.onNext(oldBuffer)
          case Stop => Stop
        }
        ack
      }

      def onNext(elem: A): Future[Ack] = self.synchronized {
        val w = toWeight(elem)
        if buffer.nonEmpty && maxWeight < bufferWeight + w then
          sendNext().syncTryFlatten.syncFlatMap {
            case Continue => onNext2(elem, w)
            case Stop => Stop
          }
        else
          onNext2(elem, w)
      }

      private def onNext2(elem: A, weight: Long): Future[Ack] = self.synchronized {
        val now = scheduler.clockMonotonic(MILLISECONDS)
        if buffer.isEmpty then {
          for span <- timespanMillis do {
            expiresAt = now + span
            val remaining = expiresAt - now
            if remaining > 0 then {
              timer := scheduler.scheduleOnce(remaining, MILLISECONDS, self)
            }
          }
        }
        buffer.append(elem)
        bufferWeight += weight

        if expiresAt <= now || maxWeight <= bufferWeight then
          sendNext()
        else
          Continue
      }

      def onError(ex: Throwable): Unit = self.synchronized {
        timer.cancel()
        ack = Stop
        buffer = null
        out.onError(ex)
      }

      def onComplete(): Unit = self.synchronized {
        timer.cancel()

        if buffer.nonEmpty then {
          val bundleToSend = buffer.toList
          // In case the last onNext isn't finished, then
          // we need to apply back-pressure, otherwise this
          // onNext will break the contract.
          ack.syncOnContinue {
            out.onNext(bundleToSend)
            out.onComplete()
          }
        } else {
          // We can just stream directly
          out.onComplete()
        }

        // GC relief
        buffer = null
        // Ensuring that nothing else happens
        ack = Stop
      }
    })

    CompositeCancelable(connection, timer)
  }
}
