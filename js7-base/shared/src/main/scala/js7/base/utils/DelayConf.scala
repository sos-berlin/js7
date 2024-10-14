package js7.base.utils

import cats.data.NonEmptyList
import cats.effect.{Async, IO}
import cats.syntax.flatMap.*
import fs2.{Pure, Stream}
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.*
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import scala.concurrent.duration.*

final case class DelayConf(
  delays: NonEmptyList[FiniteDuration],
  resetWhen: FiniteDuration = FiniteDuration.MaxValue):

  def stream: Stream[Pure, FiniteDuration] =
    Stream.iterable(delays.toList) ++ Stream.constant(delays.last)

  def lazyList: LazyList[FiniteDuration] =
    repeatLast(delays.toList)

  /** Like run[IO] — only because Intellij does not detect body's type of run[IO]. */
  def runIO[A](body: Delayer[IO] => IO[A]): IO[A] =
    run[IO].apply(body)

  def run[F[_]](using Async[F]): Run[F] =
    Run[F]

  final class Run[F[_]] private[DelayConf](using Async[F]):
    def apply[A](body: Delayer[F] => F[A]): F[A] =
      start[F].flatMap(body)

  def start[F[_]](using Async[F]): F[Delayer[F]] =
    Delayer.start(this)

  override def toString = s"DelayConf($argString)"

  private def argString: String =
    delays.toList.view.map(_.pretty).mkString(" ") +
      ((resetWhen != FiniteDuration.MaxValue) ?? " resetWhen=%s".format(resetWhen.pretty))


object DelayConf:

  /* Delay up to 10s.*/
  val default: DelayConf = DelayConf(1.s, 3.s, 6.s, 10.s)

  def apply(delay: FiniteDuration, moreDelayes: FiniteDuration*): DelayConf =
    DelayConf(NonEmptyList.of(delay, moreDelayes*))

  def maybe(delays: Seq[FiniteDuration]): Option[DelayConf] =
    NonEmptyList.fromSeq(delays).map(DelayConf(_))
