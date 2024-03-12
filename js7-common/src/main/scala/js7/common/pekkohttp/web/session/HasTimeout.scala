package js7.common.pekkohttp.web.session

import cats.effect.IO
import cats.syntax.option.*
import js7.base.catsutils.CatsDeadline
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.time.ScalaTime.*
import js7.common.pekkohttp.web.session.HasTimeout.*
import scala.concurrent.duration.*

private[session] trait HasTimeout:

  @volatile private var state = none[State]

  final def isAlive: IO[Boolean] =
    state.fold(IO.True)(_.timeoutAt.hasTimeLeft)

  /** Only untouched ones are eternal! */
  private[session] final def touch(timeout: FiniteDuration): IO[Unit] =
    CatsDeadline.now.map: now =>
      state = Some(State(
        touchedAt = now,
        timeoutAt = now + timeout))

  private[session] final def isEternal: Boolean =
    state.isEmpty

  final def touchedBefore: IO[FiniteDuration] =
    state.fold(IO.pure(0.s/*???*/))(_.touchedAt.elapsed)


private[session] object HasTimeout:

  private final case class State(touchedAt: CatsDeadline, timeoutAt: CatsDeadline)
