package js7.base.session

import cats.effect.{ExitCase, Resource}
import js7.base.auth.UserAndPassword
import js7.base.generic.Completed
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, Logger}
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Problem
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
          .timeout(tryLogoutTimeout)
          .onErrorRecover { case t =>
            logger.debug(s"$toString: logout failed: ${t.toStringWithCauses}")
            clearSession()
            Completed
          }))

  private[SessionApi] final def onErrorTryAgain(throwable: Throwable): Task[Boolean] =
    SessionApi.onErrorTryAgain(toString, throwable)
}

object SessionApi
{
  private val logger = Logger[this.type]

  /** Logs out when the resource is being released. */
  def resource[A <: SessionApi](api: Task[A]): Resource[Task, A] =
    Resource.make(
      acquire = api)(
      release = _.tryLogout.void)

  private def warn(myToString: String, throwable: Throwable): Unit = {
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

  def onErrorTryAgain(myToString: String, throwable: Throwable): Task[Boolean] =
    Task.pure(true)

  trait LoginUntilReachable extends SessionApi
  {
    self =>

    protected def isTemporaryUnreachable(throwable: Throwable): Boolean =
      HttpClient.isTemporaryUnreachable(throwable)

    def hasSession: Boolean

    final def loginUntilReachable_(
      userAndPassword: Option[UserAndPassword],
      delays: Iterator[FiniteDuration] = defaultLoginDelays(),
      onError: Throwable => Task[Boolean] = this.onErrorTryAgain,
      onlyIfNotLoggedIn: Boolean = false)
    : Task[Completed] =
      Task.defer(
        if (onlyIfNotLoggedIn && hasSession)
          Task.completed
        else {
          val sym = new BlockingSymbol
          login_(userAndPassword)
            .onErrorRestartLoop(()) { (throwable, _, retry) =>
              val isTemporary = isTemporaryUnreachable(throwable)
              val prefix = isTemporary ?? {
                sym.onWarn()
                s"$sym "
              }
              warn(s"$prefix$self", throwable)
              onError(throwable).flatMap(continue =>
                if (continue/*normally true*/ && delays.hasNext && isTemporary)
                  retry(()) delayExecution delays.next()
                else
                  Task.raiseError(throwable))
            }
            .guaranteeCase {
              case ExitCase.Completed => Task(
                if (sym.called) logger.info(s"🟢 $self logged-in"))
              case ExitCase.Canceled => Task(
                if (sym.called) logger.info(s"⚫️ $self Canceled"))
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
          Task.tailRecM(())(_ =>
            body
              .onErrorRestartLoop(()) {
                case (HttpException.HasProblem(problem), _, retry)
                  if problem.is(InvalidSessionTokenProblem) && delays.hasNext =>
                  renewSession(problem, delays) *>
                    retry(())

                case (throwable, _, _) =>
                  Task.raiseError(throwable)
              }
              .flatMap { // A may be a Checked[Problem]
                case Left(problem: Problem)
                  if problem.is(InvalidSessionTokenProblem) && delays.hasNext =>
                  renewSession(problem, delays).as(Left(()))
                case o => Task.right(o)
              }
          )
      }

    private def renewSession(problem: Problem, delays: Iterator[FiniteDuration])
    : Task[Completed] =
      Task.defer {
        clearSession()
        // Race condition with a parallel operation,
        // which after the same error has already logged-in again successfully.
        // Should be okay if login is delayed like here
        logger.debug(s"$toString: Login again due to: $problem")
        Task.sleep(delays.next()) *>
          login()
      }

    override final def retryUntilReachable[A](
      onError: Throwable => Task[Boolean] = this.onErrorTryAgain)
      (body: => Task[A])
    : Task[A] =
      Task.defer {
        val delays = loginDelays()
        val sym = new BlockingSymbol
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
                    sym.onWarn()
                    warn(s"$sym $toString", e)
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
              if (sym.called) logger.info(s"🟢 $self reached"))
            case ExitCase.Canceled => Task(
              if (sym.called) logger.info(s"⚫️ $self Canceled"))
            case _ => Task.unit
          }
      }

    final def login(onlyIfNotLoggedIn: Boolean = false): Task[Completed] =
      login_(userAndPassword, onlyIfNotLoggedIn = onlyIfNotLoggedIn)

    final def loginUntilReachable(
      delays: Iterator[FiniteDuration] = defaultLoginDelays(),
      onError: Throwable => Task[Boolean] = this.onErrorTryAgain,
      onlyIfNotLoggedIn: Boolean = false)
    : Task[Completed] =
      loginUntilReachable_(userAndPassword, delays, onError, onlyIfNotLoggedIn = onlyIfNotLoggedIn)
  }

  trait Dummy extends SessionApi.HasUserAndPassword {
    protected def userAndPassword = None

    def hasSession = true

    def login_(userAndPassword: Option[UserAndPassword], onlyIfNotLoggedIn: Boolean) =
      Task.completed

    def logout() = Task.completed

    def clearSession() = {}
  }

  private val tryLogoutTimeout = 5.s

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
