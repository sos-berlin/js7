package js7.common.http

import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.session.SessionApi
import js7.base.time.ScalaTime.*
import js7.common.http.RecouplingStreamReader.TerminatedProblem
import cats.effect.IO
import js7.base.utils.MVar

/** Resembles Monix MVar with but may contain `Left(TerminatedProblem)` to indicate termination.
  * With `Left(TerminatedProblem)` the read operations fail with `ProblemException`.
  * Commands (`executeCommand`) waiting for coupling will be completed with `ProblemException`.
  */
private[http] final class CoupledApiVar[Api <: SessionApi]:

  // The only Left value is TerminatedProblem
  private val coupledApiMVar = MVar.empty[IO, Checked[Api]]().memoize
  @volatile private var stopped = false

  def isStopped = stopped

  def terminate: IO[Unit] =
    IO.tailRecM(ZeroDuration)(delay =>
      IO {
        stopped = true
      } *>
        IO.delay(delay) *>
        invalidate *>
        coupledApiMVar.flatMap(_.tryPut(Left(TerminatedProblem)))
          .map(if _ then Right(()) else Left(10.ms/*just in case this loop is endless*/)))

  def invalidate: IO[Completed] =
    coupledApiMVar.flatMap(_.tryTake)
      .map(_ => Completed)

  def read: IO[Api] =
    coupledApiMVar.flatMap(_
      .read
      .map(_.orThrow))

  def tryRead: IO[Option[Api]] =
    coupledApiMVar.flatMap(_
      .tryRead
      .map(_.map(_.orThrow)))

  def take: IO[Api] =
    coupledApiMVar.flatMap(_
      .take
      .map(_.orThrow))

  def tryTake: IO[Option[Api]] =
    coupledApiMVar.flatMap(_
      .tryTake
      .map(_.map(_.orThrow)))

  def isTerminated: IO[Boolean] =
    coupledApiMVar.flatMap(_.tryRead).map:
      case Some(Left(TerminatedProblem)) => true
      case _ => false

  def put(api: Api): IO[Unit] =
    coupledApiMVar.flatMap(_.put(Right(api)))
