package com.sos.jobscheduler.master.cluster

import com.sos.jobscheduler.common.scalautil.Logger
import monix.reactive.{Observable, OverflowStrategy}
import scala.concurrent.duration.FiniteDuration

private[cluster] object ObservablePauseDetector
{
  private val logger = Logger(getClass)
  private implicit val overflowStrategy = OverflowStrategy.BackPressure(bufferSize = 2/*minimum*/)

  implicit final class RichPauseObservable[A](private val underlying: Observable[A]) extends AnyVal
  {
    /** Returns Some[A], or None for each pause (only one None per pause). */
    def detectPauses(notShorterThan: FiniteDuration): Observable[Option[A]] =
      Observable[Observable[Ticking]](
        underlying map Data.apply,
        Observable.intervalWithFixedDelay(notShorterThan, notShorterThan).map(_ => Tick)
      ).merge
        .scan[Element[A]](Tick) {
          case (Tick | Expired, Tick) => Expired
          case (_, Tick) => Tick
          case (_, data: Data[A @unchecked]) => data
        }
        .collect {
          case Expired => None
          case Data(data: A @unchecked) => Some(data)
        }
  }

  private sealed trait Element[+A]
  private sealed trait Ticking extends Element[Nothing] with Product
  private sealed trait Expirable extends Element[Nothing]

  private case object Tick extends Ticking
  private case object Expired extends Expirable
  private final case class Data[A](event: A) extends Ticking with Expirable with Element[A]
}
