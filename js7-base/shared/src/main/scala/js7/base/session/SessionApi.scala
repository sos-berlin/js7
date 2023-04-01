package js7.base.session

import cats.effect.{ExitCase, Resource}
import js7.base.auth.UserAndPassword
import js7.base.generic.Completed
import js7.base.log.Logger.syntax.*
import js7.base.log.{Logger, WaitSymbol}
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.session.SessionApi.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.HttpClient
import js7.base.web.HttpClient.HttpException
import monix.eval.Task
import scala.concurrent.duration.*

// Test in SessionRouteTest
/**
  * @author Joacim Zschimmer
  */
trait SessionApi
{
  private val tryLogoutLock = AsyncLock("SessionApi.tryLogout")

  def login_(userAndPassword: Option[UserAndPassword], onlyIfNotLoggedIn: Boolean = false)
  : Task[Completed]

  def logout(): Task[Completed]

  def clearSession(): Unit

  // overrideable
  def retryIfSessionLost[A]()(body: Task[A])
  : Task[A] =
    body

  // overrideable
  def retryUntilReachable[A](onError: Throwable => Task[Boolean] = onErrorTryAgain)
    (body: => Task[A])
  : Task[A] =
    body

  final def tryLogout: Task[Completed] =
    logger.traceTask(s"$toString: tryLogout")(
      tryLogoutLock.lock(
        logout()
          .onErrorRecover { case t =>
            logger.debug(s"$toString: logout failed: ${t.toStringWithCauses}")
            clearSession()
            Completed
          }))

  private[SessionApi] final def onErrorTryAgain(throwable: Throwable): Task[Boolean] =
    SessionApi.onErrorTryAgain(throwable, toString)
}

object SessionApi
{
  private val logger = Logger[this.type]

  /** Logs out when the resource is being released. */
  def resource[A <: SessionApi](api: Task[A]): Resource[Task, A] =
    Resource.make(
      acquire = api)(
      release = _.tryLogout.void)

  def warn(throwable: Throwable, myToString: String): Unit = {
    logger.warn(s"$myToString: ${throwable.toStringWithCauses}")
    throwable match {
      case _: javax.net.ssl.SSLException =>
      case _ =>
        if (throwable.getStackTrace.nonEmpty
          && throwable.getClass.scalaName != "akka.stream.StreamTcpException"
          && Option(throwable.getCause).forall(_.getClass.scalaName != "akka.stream.StreamTcpException")) {
          logger.debug(s"$myToString: ${throwable.toString}", throwable)
        }
    }
  }

  def onErrorTryAgain(throwable: Throwable, myToString: String): Task[Boolean] =
    Task.pure(true)

  trait NoSession extends SessionApi
  {
    final def login_(userAndPassword: Option[UserAndPassword], onlyIfNotLoggedIn: Boolean = false) =
      Task.completed

    final def logout() =
      Task.completed

    final def clearSession() = {}
  }

  trait LoginUntilReachable extends SessionApi
  {
    self =>

    protected def isTemporaryUnreachable(throwable: Throwable): Boolean =
      HttpClient.isTemporaryUnreachable(throwable)

    def hasSession: Boolean

    final def loginUntilReachable_(
      userAndPassword: Option[UserAndPassword],
      delays: Iterator[FiniteDuration] = defaultLoginDelays(),
      onError: Throwable => Task[Boolean] = onErrorTryAgain,
      onlyIfNotLoggedIn: Boolean = false)
    : Task[Completed] =
      Task.defer(
        if (onlyIfNotLoggedIn && hasSession)
          Task.completed
        else {
          val waitSymbol = new WaitSymbol
          login_(userAndPassword)
            .onErrorRestartLoop(()) { (throwable, _, retry) =>
              val isTemporary = isTemporaryUnreachable(throwable)
              val sym = isTemporary ?? {
                waitSymbol.onWarn()
                s"$waitSymbol "
              }
              warn(throwable, s"$sym$self")
              onError(throwable).flatMap(continue =>
                if (continue && delays.hasNext && isTemporary)
                  retry(()) delayExecution delays.next()
                else
                  Task.raiseError(throwable))
            }
            .guaranteeCase {
              case ExitCase.Completed => Task(
                if (waitSymbol.called) logger.info(s"🟢 $self logged-in"))
              case ExitCase.Canceled => Task(
                if (waitSymbol.called) logger.info(s"⚫️ $self Canceled"))
              case _ => Task.unit
            }
        })
  }

  trait HasUserAndPassword extends LoginUntilReachable
  {
    self =>
    protected def userAndPassword: Option[UserAndPassword]

    protected val loginDelays: () => Iterator[FiniteDuration] =
      () => defaultLoginDelays()

    override def retryIfSessionLost[A]()(body: Task[A]): Task[A] =
      Task.defer {
        val delays = Iterator(0.s) ++ loginDelays()
        login(onlyIfNotLoggedIn = true) *>
          body
            .onErrorRestartLoop(()) {
              case (HttpException.HasProblem(problem), _, retry)
                if problem.is(InvalidSessionTokenProblem) && delays.hasNext =>
                clearSession()
                // Race condition with a parallel operation,
                // which after the same error has already logged-in again successfully.
                // Should be okay if login is delayed like here
                logger.debug(s"$toString: Login again due to: $problem")
                Task.sleep(delays.next()) *>
                  login() *>
                  retry(())

              case (throwable, _, _) =>
                Task.raiseError(throwable)
            }
      }

    override final def retryUntilReachable[A](onError: Throwable => Task[Boolean] = onErrorTryAgain)
      (body: => Task[A])
    : Task[A] =
      Task.defer {
        val delays = loginDelays()
        val waitSymbol = new WaitSymbol
        loginUntilReachable(delays, onError = onError, onlyIfNotLoggedIn = true)
          .flatMap((_: Completed) =>
            body.onErrorRestartLoop(()) { (throwable, _, retry) =>
              throwable
                .match_ {
                  case HttpException.HasProblem(problem)
                    if problem.is(InvalidSessionTokenProblem) && delays.hasNext =>
                    // Do not call onError on this minor problem
                    logger.debug(s"⟲ $toString: $problem")
                    loginUntilReachable(delays, onError = onError)

                  case e: HttpException if isTemporaryUnreachable(e) && delays.hasNext =>
                    waitSymbol.onWarn()
                    warn(e, s"🔴 $toString")
                    onError(e).flatMap(continue =>
                      if (continue)
                        loginUntilReachable(delays, onError = onError, onlyIfNotLoggedIn = true)
                      else
                        Task.raiseError(e))

                  case _ =>
                    Task.raiseError(throwable)
                }
                .*>(Task.sleep(delays.next()))
                .*>(retry(()))
            })
          .guaranteeCase {
            case ExitCase.Completed => Task(
              if (waitSymbol.called) logger.info(s"🟢 $self reached"))
            case ExitCase.Canceled => Task(
              if (waitSymbol.called) logger.info(s"⚫️ $self Canceled"))
            case _ => Task.unit
          }
      }

    final def login(onlyIfNotLoggedIn: Boolean = false): Task[Completed] =
      login_(userAndPassword, onlyIfNotLoggedIn = onlyIfNotLoggedIn)

    final def loginUntilReachable(
      delays: Iterator[FiniteDuration] = defaultLoginDelays(),
      onError: Throwable => Task[Boolean] = onErrorTryAgain,
      onlyIfNotLoggedIn: Boolean = false)
    : Task[Completed] =
      loginUntilReachable_(userAndPassword, delays, onError, onlyIfNotLoggedIn = onlyIfNotLoggedIn)
  }

  private val initialLoginDelays = {
    val seq = Seq(
      100.ms, 900.ms, 1.s, 1.s, 1.s, 2.s, 2.s, 2.s, // 10s
      5.s, 5.s, // 20s
      5.s, 5.s, // 30s
      5.s, 5.s, // 40s
      5.s, 5.s, // 50s
      5.s, 5.s) // 60s
    assert(seq.reduce(_ + _) == 1.minute)
    seq
  }

  def defaultLoginDelays(): Iterator[FiniteDuration] =
    initialLoginDelays.iterator ++ Iterator.continually(10.s)
}
