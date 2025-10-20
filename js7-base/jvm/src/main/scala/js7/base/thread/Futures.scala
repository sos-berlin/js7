package js7.base.thread

import izumi.reflect.Tag
import java.util.concurrent.TimeoutException
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.scalasource.ScalaSourceLocation
import js7.base.utils.StackTraces.*
import scala.collection.BuildFrom
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
 * @author Joacim Zschimmer
 */
object Futures:

  private val logger = Logger[this.type]

  /**
   * Maps an exception of body to Future.failed.
   */
  def catchInFuture[A](body: => Future[A]): Future[A] =
    try body
    catch
      case NonFatal(t) => Future.failed(t)

  /**
    * A Future for blocking code.
    * A big number of blocking Futures may be started, each consuming a thread.
    * This differs from ExecutionContext.global, which limits the number of threads to (number of cores) + 256.
    */
  def blockingThreadFuture[A](body: => A): Future[A] =
    namedThreadFuture("Blocking")(body)

  def namedThreadFuture[A](name: String)(body: => A): Future[A] =
    val promise = Promise[A]()
    new Thread {
      if name.nonEmpty then setName(name)
      override def run(): Unit =
        try promise.success(body)
        catch { case t: Throwable =>  // Not only NonFatal (or what should we do else?)
          promise.failure(t)
        }
    } .start()
    promise.future

  def promiseFuture[A](body: Promise[A] => Unit): Future[A] =
    val promise = Promise[A]()
    body(promise)
    promise.future

  object syntax:
    implicit final class RichFuture[A](private val future: Future[A]) extends AnyVal:
      def onFailure(pf: PartialFunction[Throwable, Unit])(implicit ec: ExecutionContext): Unit =
        future onComplete:
          case Failure(throwable) => pf.applyOrElse(throwable, (_: Throwable) => ())
          case Success(_) =>

  object implicits:
    implicit final class SuccessFuture[A](private val delegate: Future[A]) extends AnyVal:
      /**
       * Like .value.get.get - in case of exception own stack trace is added.
       */
      def successValue: A = delegate.value match
        case Some(Success(o)) => o
        case Some(Failure(t)) => throw t.appendCurrentStackTrace
        case None => throw new FutureNotSucceededException

      /**
       * Returns a new Future with the current stack trace added in case of failure of the original Future.
       */
      def appendCurrentStackTrace: Future[A] =
        val callersStackTrace = new Exception().getStackTrace
        delegate.recoverWith {
          case t => Future.failed[A](t.appendStackTrace(callersStackTrace))
        } (using SynchronousExecutionContext)

      /**
        * Awaits the futures completion for the duration or infinite.
        */
      def await(duration: Option[FiniteDuration])
        (using A: Tag[A],
          src: sourcecode.Enclosing, loc: ScalaSourceLocation)
      : A =
        duration match
          case Some(o) => await(o)
          case None => awaitInfinite

      // Separate implementation to differentiate calls to awaitInfinite and await(Duration)
      def awaitInfinite(using Tag[A], sourcecode.Enclosing, ScalaSourceLocation)
      : A =
        logger.traceCall[A](makeBlockingWaitingString[A]):
          Await.ready(delegate, Duration.Inf).value.get match
            case Success(o) => o
            case Failure(t) => throw t.appendCurrentStackTrace


      def await(duration: FiniteDuration)
        (using A: Tag[A], enc: sourcecode.Enclosing, loc: ScalaSourceLocation)
      : A =
        inline def name = makeBlockingWaitingString[A]
        logger.traceCall[A](name):
          try Await.ready(delegate, duration)
          catch case _: TimeoutException =>
            throw new TimeoutException(name + " timed out")
          delegate.value.get match
            case Success(o) => o
            case Failure(t) => throw t.appendCurrentStackTrace

    implicit final class RichFutures[A, M[X] <: IterableOnce[X]](private val future: M[Future[A]]) extends AnyVal:
      /**
        * Awaits the futures completion for the duration or infinite.
        */
      def await(duration: Option[FiniteDuration])(implicit ec: ExecutionContext, cbf: BuildFrom[M[Future[A]], A, M[A]], MA: Tag[M[A]]): M[A] =
        duration match
          case Some(o) => await(o)
          case None => awaitInfinite

      def await(duration: FiniteDuration)(implicit ec: ExecutionContext, cbf: BuildFrom[M[Future[A]], A, M[A]], MA: Tag[M[A]]): M[A] =
        Future.sequence(future)(using cbf, ec).await(duration)

      def awaitInfinite(using
        ec: ExecutionContext,
        bf: BuildFrom[M[Future[A]], A, M[A]],
        A: Tag[M[A]], src: sourcecode.Enclosing, loc: ScalaSourceLocation)
      : M[A] =
        Future.sequence(future)(using bf, ec).awaitInfinite

    implicit final class SuccessPromise[A](private val delegate: Promise[A]) extends AnyVal:
      def successValue: A = delegate.future.successValue

  def makeBlockingWaitingString[A](using
    A: Tag[A],
    src: sourcecode.Enclosing,
    loc: ScalaSourceLocation)
  : String =
    s"await[${A.tag}] in ${src.value} at $loc"

  final class FutureNotSucceededException extends NoSuchElementException("Future has not been succeeded")

  object SynchronousExecutionContext extends ExecutionContext:
    def execute(runnable: Runnable): Unit =
      runnable.run()

    def reportFailure(cause: Throwable): Unit =
      logger.error(s"$cause", cause)
