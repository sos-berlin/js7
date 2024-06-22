package js7.base.utils

import cats.data.{NonEmptyList, NonEmptySeq}
import cats.effect.Async
import cats.syntax.flatMap.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import scala.concurrent.duration.*

final case class DelayConf(
  delays: NonEmptySeq[FiniteDuration],
  resetWhen: FiniteDuration):

  def iterator(): Iterator[FiniteDuration] =
    delays.iterator ++ Iterator.continually(delays.last)

  def run[F[_]](using Async[F]): Run[F] =
    new Run[F]

  final class Run[F[_]](using Async[F]):
    def apply[A](body: Delayer[F] => F[A]) =
      start[F].flatMap(body)

  def start[F[_]](using Async[F]): F[Delayer[F]] =
    Delayer.start(this)

  override def toString = s"DelayConf($argString)"

  def argString: String =
    delays.toSeq.view.map(_.pretty).mkString(" ") +
      ((resetWhen != FiniteDuration.MaxValue) ?? " resetWhen=%s".format(resetWhen.pretty) )


object DelayConf:
  /* Delay up to 10s.*/
  val default: DelayConf = DelayConf(1.s, 3.s, 6.s, 10.s)

  def apply(delays: NonEmptySeq[FiniteDuration]): DelayConf =
    DelayConf(delays, delays.last)

  def apply(delays: NonEmptyList[FiniteDuration]): DelayConf =
    DelayConf(delays.head, delays.tail*)

  def apply(delay: FiniteDuration, moreDelayes: FiniteDuration*): DelayConf =
    DelayConf(NonEmptySeq(delay, moreDelayes))

  def maybe(delays: Seq[FiniteDuration]): Option[DelayConf] =
    for delays <- NonEmptySeq.fromSeq(delays) yield
      DelayConf(delays)
