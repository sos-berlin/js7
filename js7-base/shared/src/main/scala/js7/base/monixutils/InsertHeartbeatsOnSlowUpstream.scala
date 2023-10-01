// Copy of Monix EchoObservable.scala to output a constant value instead of the last event
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

import java.util.concurrent.TimeUnit
import monix.execution.Ack.{Continue, Stop}
import monix.execution.cancelables.{CompositeCancelable, MultiAssignCancelable, SingleAssignCancelable}
import monix.execution.{Ack, Cancelable}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber
import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import scala.util.Success

private[monixutils] final class InsertHeartbeatsOnSlowUpstream[+A](source: Observable[A], timeout: FiniteDuration,
  onlyOnce: Boolean, intersperseValue: A)
  extends Observable[A]:

  private[this] val timeoutMillis = timeout.toMillis

  def unsafeSubscribeFn(out: Subscriber[A]): Cancelable =
    val task = MultiAssignCancelable()
    val mainTask = SingleAssignCancelable()
    val composite = CompositeCancelable(mainTask, task)

    mainTask := source.unsafeSubscribeFn(new Subscriber[A] with Runnable { self =>
      implicit val scheduler = out.scheduler

      private[this] var ack: Future[Ack] = Continue
      private[this] var lastEvent: A = _
      private[this] var lastTSInMillis: Long = 0L
      private[this] var isDone = false
      private[this] var hasValue = false

      locally {
        scheduleNext(timeoutMillis)
      }

      def scheduleNext(delayMillis: Long): Unit = {
        // No need to synchronize this assignment, since we have a
        // happens-before relationship between scheduleOnce invocations.
        task := scheduler.scheduleOnce(delayMillis, TimeUnit.MILLISECONDS, self)
      }

      def run(): Unit = {
        def cancelMainTask() =
          self.synchronized {
            isDone = true
            mainTask.cancel()
            Stop
          }

        self.synchronized {
          if isDone then
            () // do nothing else
          else if !ack.isCompleted then {
            // The consumer is still processing its last message,
            // and this processing time does not enter the picture.
            // Given that the lastTSInMillis is set after Continue
            // happens, it means that we'll wait for Continue plus
            // our period in order to get another chance to emit
            ack.onComplete {
              case Success(Continue) =>
                scheduleNext(timeoutMillis)
              case _ =>
                ()
            }
          } else if lastEvent == null || !hasValue then {
            // on this branch either the data source hasn't emitted anything
            // yet (lastEvent == null), or we don't have a new value since
            // the last time we've tried (!hasValue), so keep waiting
            scheduleNext(timeoutMillis)
          } else {
            val rightNow = scheduler.clockMonotonic(MILLISECONDS)
            val sinceLastOnNext = rightNow - lastTSInMillis

            if sinceLastOnNext >= timeoutMillis then {
              // hasValue is set to false only if the onlyOnce param is
              // set to true (otherwise we keep repeating our current
              // value until a new one happens)
              hasValue = !onlyOnce

              // this call is actually synchronous because we're testing
              // for ack.isCompleted above, but doing it nonetheless because
              // of safety and because last ack might have been a Stop
              ack = ack.syncTryFlatten.syncFlatMap {
                case Continue =>
                  out.onNext(intersperseValue).syncFlatMap {
                    case Continue =>
                      // the speed with which the downstream replied with Continue
                      // matters in this case, so we are measuring it and
                      // subtracting it from the period
                      val executionTime = scheduler.clockMonotonic(MILLISECONDS) - rightNow
                      val delay =
                        if timeoutMillis > executionTime then
                          timeoutMillis - executionTime
                        else 0L

                      scheduleNext(delay)
                      Continue

                    case Stop =>
                      cancelMainTask()
                  }
                case Stop =>
                  cancelMainTask()
              }
            } else {
              val remainingTime = timeoutMillis - sinceLastOnNext
              scheduleNext(remainingTime)
            }
          }
        }
      }

      def onNext(elem: A): Future[Ack] = {
        def unfreeze(): Ack = {
          hasValue = true
          lastTSInMillis = scheduler.clockMonotonic(MILLISECONDS)
          Continue
        }

        // Handling flattening manually to avoid
        // extra synchronization for synchronous onNext
        def signalNext(ack: Future[Ack]): Future[Ack] =
          ack match {
            case Continue =>
              if isDone then Stop
              else
                out.onNext(elem) match {
                  case Continue => unfreeze()
                  case Stop => Stop
                  case async =>
                    async.flatMap {
                      case Continue => self.synchronized(unfreeze())
                      case Stop => Stop
                    }
                }
            case Stop => Stop
            case async =>
              async.flatMap(signalNext)
          }

        self.synchronized {
          if isDone then Stop
          else {
            lastEvent = elem
            ack = signalNext(ack.syncTryFlatten)
            ack
          }
        }
      }

      def onError(ex: Throwable): Unit =
        self.synchronized {
          if !isDone then {
            isDone = true
            task.cancel()
            out.onError(ex)
          }
        }

      def onComplete(): Unit = {
        def signal(): Ack = {
          if !isDone then {
            isDone = true
            task.cancel()
            out.onComplete()
          }
          Stop
        }

        self.synchronized {
          ack = ack.syncTryFlatten match {
            case Stop => Stop
            case Continue => signal()
            case async =>
              async.flatMap {
                case Continue => self.synchronized(signal())
                case Stop => Stop
              }
          }
        }
      }
    })

    composite
