package js7.base.monixutils

import js7.base.monixutils.MonixBase.syntax._
import js7.base.monixutils.MonixDeadline.now
import monix.reactive.{Observable, OverflowStrategy}
import scala.concurrent.duration.FiniteDuration

object ObservablePauseDetector
{
  private implicit val overflowStrategy = OverflowStrategy.BackPressure(bufferSize = 2/*minimum*/)

  implicit final class RichPauseObservable[A](private val underlying: Observable[A]) extends AnyVal
  {
    // TODO Left should contain the timestamp of the last A, not the first Tick after the last A
    // TODO May parameterize the current Either return type with two functions for less allocs
    /** Returns Right[A], or Left for each pause (only one Left per pause). */
    def detectPauses(delay: FiniteDuration): Observable[Either[MonixDeadline, A]] =
      Observable.deferAction(implicit scheduler =>
        Observable[Observable[Ticking]](
          underlying map Data.apply,
          Observable.intervalWithFixedDelay(delay, delay).map(_ => Tick(now))
        ).merge
          .scan[Element[A]](Tick(now)) {
            case (Tick(o), Tick(_)) => Expired(o)
            case (Expired(o), Tick(_)) => Expired(o)
            case (_, tick: Tick) => tick
            case (_, data: Data[A]) => data
          }
          .collect {
            case Expired(since) => Left(since)
            case Data(a: A @unchecked) => Right(a)
          })
  }

  private sealed trait Element[+A]
  private sealed trait Ticking extends Element[Nothing] with Product
  private sealed trait Expirable extends Element[Nothing]

  private case class Tick(since: MonixDeadline) extends Ticking
  private case class Expired(since: MonixDeadline) extends Expirable
  private final case class Data[A](event: A) extends Ticking with Expirable with Element[A]
}
