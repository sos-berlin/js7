package js7.base.log

import cats.effect.{Resource, Sync}
import cats.syntax.functor.*
import implicitbox.Not
import js7.base.log.CorrelId.generate
import monix.eval.Task
import monix.execution.CancelableFuture
import monix.execution.misc.Local
import monix.execution.schedulers.TrampolineExecutionContext
import scala.annotation.{implicitNotFound, unused}
import scala.concurrent.Future

// Inspired by Monix 3.4 CanBindLocals
@implicitNotFound("""Cannot find an implicit value for CanBindCorrelId[${R}].
If ${R} is the result of a synchronous action, either build an implicit with
CanBindCorrelId.synchronous or import CanBindCorrelId.Implicits.synchronousAsDefault.""")
trait CanBindCorrelId[R] {
  private[log] def bind(correlId: CorrelId)(body: => R): R
  private[log] def bindNewIfNoCurrent(body: => R): R
}

object CanBindCorrelId
{
  private var _bindCorrelIdCount = 0L

  private[log] def bindCorrelIdCount = _bindCorrelIdCount

  implicit def task[R]: CanBindCorrelId[Task[R]] =
    TaskCan.asInstanceOf[CanBindCorrelId[Task[R]]]

  implicit def future[R]: CanBindCorrelId[Future[R]] =
    FutureCan.asInstanceOf[CanBindCorrelId[Future[R]]]

  implicit def cancelableFuture[R]: CanBindCorrelId[CancelableFuture[R]] =
    CancelableFutureCan.asInstanceOf[CanBindCorrelId[CancelableFuture[R]]]

  @inline implicit def forUnit: CanBindCorrelId[Unit] =
    synchronous[Unit]

  def synchronous[R]: CanBindCorrelId[R] =
    SynchronousCan.asInstanceOf[CanBindCorrelId[R]]

  private object TaskCan extends CanBindCorrelId[Task[Any]] {
    def bind(correlId: CorrelId)(task: => Task[Any]): Task[Any] =
      if (!CorrelId.isEnabled)
        Task.defer(task)
      else
        Task.defer {
          val saved = Local.getContext()
          _bindCorrelIdCount += 1
          Task.defer {
            Local.setContext(saved.bind(CorrelId.local.key, Some(correlId)))
            task
          }.guarantee(Task(Local.setContext(saved)))
        }

    def bindNewIfNoCurrent(task: => Task[Any]): Task[Any] =
      if (!CorrelId.isEnabled)
        Task.defer(task)
      else
        Task.defer {
          if (CorrelId.current.nonEmpty)
            task
          else
            bind(generate())(task)
        }
  }

  private sealed trait FutureCan[F[r] <: Future[r], R] extends CanBindCorrelId[F[R]] {
    def bind(correlId: CorrelId)(future: => F[R]): F[R] =
      if (!CorrelId.isEnabled)
        future
      else {
        _bindCorrelIdCount += 1
        val saved = Local.getContext()
        Local.setContext(saved.bind(CorrelId.local.key, Some(correlId)))
        try
          future.transform(result => {
            CorrelId.local.clear()
            result
          })(TrampolineExecutionContext.immediate).asInstanceOf[F[R]]
        finally
          Local.setContext(saved)
      }

    def bindNewIfNoCurrent(future: => F[R]): F[R] =
      if (!CorrelId.isEnabled || CorrelId.current.nonEmpty)
        future
      else
        bind(generate())(future)
  }

  private object FutureCan extends FutureCan[Future, Any]

  private object CancelableFutureCan extends FutureCan[CancelableFuture, Any]

  implicit def resourceCan[F[_], A]
    (implicit F: Sync[F], canBind: CanBindCorrelId[F[?]])
  : CanBindCorrelId[Resource[F, A]] =
    new CanBindCorrelId[Resource[F, A]] {
      private implicit val x = canBind.asInstanceOf[CanBindCorrelId[F[(A, F[Unit])]]]
      private implicit val y = canBind.asInstanceOf[CanBindCorrelId[F[Unit]]]

      def bind(correlId: CorrelId)(resource: => Resource[F, A]): Resource[F, A] =
        if (!CorrelId.isEnabled)
          Resource.suspend(F.delay(resource))
        else
          Resource.suspend(
            correlId.bind(resource.allocated)
              .map { case (acquiredThing, release) =>
                Resource.make(
                  acquire = F.pure(acquiredThing))(
                  release = _ => correlId.bind(release))
              })

    def bindNewIfNoCurrent(resource: => Resource[F, A]): Resource[F, A] =
      if (!CorrelId.isEnabled)
        Resource.suspend(F.delay(resource))
      else
        Resource.suspend(F.delay(
          if (CorrelId.current.nonEmpty)
            resource
          else
            bind(generate())(resource)))
    }

  private object SynchronousCan extends CanBindCorrelId[Any]
  {
    def bind(correlId: CorrelId)(body: => Any): Any =
      if (!CorrelId.isEnabled)
        body
      else {
        _bindCorrelIdCount += 1
        val saved = Local.getContext()
        Local.setContext(saved.bind(CorrelId.local.key, Some(correlId)))
        try body
        finally Local.setContext(saved)
      }

    def bindNewIfNoCurrent(body: => Any): Any =
      if (!CorrelId.isEnabled || CorrelId.current.nonEmpty)
        body
      else
        bind(generate())(body)
  }

  object implicits {
    /** Implicit instance for all things synchronous.
     *
     * Needs to be imported explicitly in scope. Will NOT override
     * other `CanBindCorrelId` implicits that are already visible.
     */
    @inline implicit def synchronousAsDefault[R](implicit @unused ev: Not[CanBindCorrelId[R]])
    : CanBindCorrelId[R] =
      CanBindCorrelId.synchronous[R]
  }
}
