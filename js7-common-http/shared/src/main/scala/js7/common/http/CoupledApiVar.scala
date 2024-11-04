package js7.common.http

import cats.effect.IO
import cats.syntax.flatMap.*
import cats.syntax.option.*
import fs2.concurrent.SignallingRef
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.session.SessionApi
import js7.base.time.ScalaTime.*
import js7.common.http.CoupledApiVar.*
import js7.common.http.RecouplingStreamReader.TerminatedProblem

/** Resembles Monix MVar with but may contain `Left(TerminatedProblem)` to indicate termination.
  * With `Left(TerminatedProblem)` the read operations fail with `ProblemException`.
  * Commands (`executeCommand`) waiting for coupling will be completed with `ProblemException`.
  */
private[http] final class CoupledApiVar[Api <: SessionApi]:

  // The only Left value is TerminatedProblem
  private val coupledApi = memoize(SignallingRef[IO].of(none[Checked[Api]]))
  @volatile private var stopped = false

  def isStopped = stopped

  def terminate: IO[Unit] =
    ZeroDuration.tailRecM: delay =>
      IO.defer:
        logger.trace("stopped = true")
        stopped = true
        IO.delay(delay) *>
          invalidate *>
          coupledApi.flatMap:
            _.tryUpdate(_ => Some(Left(TerminatedProblem)))
          .map:
            if _ then Right(()) else Left(10.ms /*just in case this loop is endless*/)

  def invalidate: IO[Completed] =
    coupledApi.flatMap(_.set(none))
      .as(Completed)

  def tryRead: IO[Option[Api]] =
    coupledApi.flatMap(_
      .get
      .map(_.map(_.orThrow)))

  def tryTake: IO[Option[Api]] =
    coupledApi.flatMap(_
      .modify(none -> _)
      .map(_.map(_.orThrow)))

  def isTerminated: IO[Boolean] =
    coupledApi.flatMap(_.get).map:
      case Some(Left(TerminatedProblem)) => true
      case _ => false

  def put(api: Api): IO[Unit] =
    coupledApi.flatMap(_.set(Some(Right(api))))

object CoupledApiVar:
  private val logger = Logger[this.type]
