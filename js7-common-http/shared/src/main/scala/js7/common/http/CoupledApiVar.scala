package js7.common.http

import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.session.SessionApi
import js7.base.time.ScalaTime._
import js7.common.http.RecouplingStreamReader.TerminatedProblem
import monix.catnap.MVar
import monix.eval.Task
import scala.concurrent.duration.Duration

/** Resembles Monix MVar with but may contain `Left(TerminatedProblem)` to indicate termination.
  * With `Left(TerminatedProblem)` the read operations fail with `ProblemException`.
  * Commands (`executeCommand`) waiting for coupling will be completed with `ProblemException`.
  */
private[http] final class CoupledApiVar[Api <: SessionApi]
{
  // The only Left value is TerminatedProblem
  private val coupledApiMVar = MVar.empty[Task, Checked[Api]]().memoize
  @volatile private var stopped = false

  def isStopped = stopped

  def terminate: Task[Completed] =
    Task.tailRecM(Duration.Zero)(delay =>
      Task {
        stopped = true
      } >>
      (Task.delay(delay) >>
        invalidate >>
        coupledApiMVar.flatMap(_.tryPut(Left(TerminatedProblem)))
          .map(if (_) Right(Completed) else Left(10.ms/*just in case this loop is endless*/))))

  def invalidate: Task[Completed] =
    coupledApiMVar.flatMap(_.tryTake)
      .map(_ => Completed)

  def read: Task[Api] =
    coupledApiMVar.flatMap(_
      .read
      .map(_.orThrow))

  def tryRead: Task[Option[Api]] =
    coupledApiMVar.flatMap(_
      .tryRead
      .map(_.map(_.orThrow)))

  def take: Task[Api] =
    coupledApiMVar.flatMap(_
      .take
      .map(_.orThrow))

  def tryTake: Task[Option[Api]] =
    coupledApiMVar.flatMap(_
      .tryTake
      .map(_.map(_.orThrow)))

  def put(api: Api): Task[Unit] =
    coupledApiMVar.flatMap(_.put(Right(api)))
}

