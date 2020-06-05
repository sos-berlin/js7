package js7.common.scalautil

import java.util.concurrent.TimeoutException
import js7.base.time.ScalaTime._
import js7.base.utils.StackTraces._
import scala.collection.BuildFrom
import scala.concurrent._
import scala.concurrent.duration._
import scala.reflect.runtime.universe._
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
 * @author Joacim Zschimmer
 */
object Futures {

  private val logger = Logger(getClass)

  /**
   * Like [[Await]]`.result` - in case of exception, own stack trace is added.
   */
  def awaitResult[A](future: Future[A], atMost: FiniteDuration): A = {
    import implicits.SuccessFuture
    Await.ready[A](future, atMost.toCoarsest).successValue
  }

  /**
   * Maps an exception of body to Future.failed.
   */
  def catchInFuture[A](body: => Future[A]): Future[A] =
    try body
    catch {
      case NonFatal(t) => Future.failed(t)
    }

  /**
    * A Future for blocking code.
    * A big number of blocking Futures may be started, each consuming a thread.
    * This differs from ExecutionContext.global, which limits the number of threads to (number of cores) + 256.
    */
  def blockingThreadFuture[A](body: => A): Future[A] =
    namedThreadFuture("Blocking")(body)

  def namedThreadFuture[A](name: String)(body: => A): Future[A] = {
    val promise = Promise[A]()
    new Thread {
      if (name.nonEmpty) setName(name)
      override def run() =
        try promise.success(body)
        catch { case t: Throwable =>  // Not only NonFatal (or what should we do else?)
          promise.failure(t)
        }
    } .start()
    promise.future
  }

  def promiseFuture[A](body: Promise[A] => Unit): Future[A] = {
    val promise = Promise[A]()
    body(promise)
    promise.future
  }

  object implicits
  {
    implicit final class SuccessFuture[A](private val delegate: Future[A]) extends AnyVal {
      /**
       * Like .value.get.get - in case of exception own stack trace is added.
       */
      def successValue: A = delegate.value match {
        case Some(Success(o)) => o
        case Some(Failure(t)) => throw t.appendCurrentStackTrace
        case None => throw new FutureNotSucceededException
      }

      /**
       * Returns a new Future with the current stack trace added in case of failure of the original Future.
       */
      def appendCurrentStackTrace: Future[A] = {
        val callersStackTrace = new Exception().getStackTrace
        delegate.recoverWith {
          case t => Future.failed[A](t.appendStackTrace(callersStackTrace))
        } (SynchronousExecutionContext)
      }

      /**
        * Awaits the futures completion for the duration or infinite.
        */
      def await(duration: Option[FiniteDuration])(implicit A: TypeTag[A]): A =
        duration match {
          case Some(o) => await(o)
          case None => awaitInfinite
        }

      // Separate implementation to differentiate calls to awaitInfinite and await(Duration)
      def awaitInfinite: A =
        Await.ready(delegate, Duration.Inf).value.get match {
          case Success(o) => o
          case Failure(t) => throw t.appendCurrentStackTrace
        }

      def await(duration: FiniteDuration)(implicit A: WeakTypeTag[A]): A = {
        try Await.ready(delegate, duration)
        catch { case _: TimeoutException =>
          throw new TimeoutException(s"await(${duration.pretty}): Future[${A.tpe.toString}] has not been completed in time")
        }
        delegate.value.get match {
          case Success(o) => o
          case Failure(t) => throw t.appendCurrentStackTrace
        }
      }
    }

    implicit final class RichFutures[A, M[X] <: IterableOnce[X]](private val delegate: M[Future[A]]) extends AnyVal {
      /**
        * Awaits the futures completion for the duration or infinite.
        */
      def await(duration: Option[FiniteDuration])(implicit ec: ExecutionContext, cbf: BuildFrom[M[Future[A]], A, M[A]], MA: TypeTag[M[A]]): M[A] =
        duration match {
          case Some(o) => await(o)
          case None => awaitInfinite
        }

      def await(duration: FiniteDuration)(implicit ec: ExecutionContext, cbf: BuildFrom[M[Future[A]], A, M[A]], MA: TypeTag[M[A]]): M[A] =
        Future.sequence(delegate)(cbf, ec) await duration

      def awaitInfinite(implicit ec: ExecutionContext, cbf: BuildFrom[M[Future[A]], A, M[A]]): M[A] =
        Future.sequence(delegate)(cbf, ec).awaitInfinite
    }

    implicit final class SuccessPromise[A](private val delegate: Promise[A]) extends AnyVal {
      def successValue: A = delegate.future.successValue
    }
  }

  final class FutureNotSucceededException extends NoSuchElementException("Future has not been succeeded")

  object SynchronousExecutionContext extends ExecutionContext {
    def execute(runnable: Runnable) = runnable.run()
    def reportFailure(cause: Throwable) = logger.error(s"$cause", cause)
  }
}
