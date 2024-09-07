package js7.base.session

import cats.effect.{IO, Outcome, Resource, ResourceIO}
import cats.syntax.flatMap.*
import js7.base.auth.UserAndPassword
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.generic.Completed
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, Logger}
import js7.base.problem.Problem
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.session.SessionApi.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.HttpClient
import js7.base.web.HttpClient.HttpException
import scala.concurrent.duration.*

// Test in SessionRouteTest
/**
  * @author Joacim Zschimmer
  */
trait SessionApi:
  private val tryLogoutLock = AsyncLock("SessionApi.tryLogout")

  def login_(userAndPassword: Option[UserAndPassword], onlyIfNotLoggedIn: Boolean = false)
  : IO[Completed]

  def logout(): IO[Completed]

  def clearSession(): Unit

  // overrideable
  def retryIfSessionLost[A]()(body: IO[A])
  : IO[A] =
    body

  // overrideable
  def retryUntilReachable[A](onError: Throwable => IO[Boolean] = onErrorTryAgain)
    (body: => IO[A])
  : IO[A] =
    body

  final def tryLogout: IO[Completed] =
    logger.traceIO(s"$toString: tryLogout")(
      tryLogoutLock.lock(
        logout()
          .timeout(tryLogoutTimeout)
          .handleError { t =>
            logger.debug(s"$toString: logout failed: ${t.toStringWithCauses}")
            clearSession()
            Completed
          }))

  private[SessionApi] final def onErrorTryAgain(throwable: Throwable): IO[Boolean] =
    SessionApi.onErrorTryAgain(toString, throwable)


object SessionApi:
  private val logger = Logger[this.type]

  /** Logs out when the resource is being released. */
  def resource[A <: SessionApi](api: IO[A]): ResourceIO[A] =
    Resource.make(
      acquire = api)(
      release = _.tryLogout.void)

  private def warn(myToString: String, throwable: Throwable): Unit =
    logger.warn(s"$myToString: ${throwable.toStringWithCauses}")
    throwable match
      case _: javax.net.ssl.SSLException =>
      case _ =>
        if throwable.getStackTrace.nonEmpty
          && throwable.getClass.scalaName != "org.apache.pekko.stream.StreamTcpException"
          && Option(throwable.getCause).forall(_.getClass.scalaName != "org.apache.pekko.stream.StreamTcpException") then
          logger.debug(s"$myToString: ${throwable.toString}", throwable)

  def onErrorTryAgain(myToString: String, throwable: Throwable): IO[Boolean] =
    IO.pure(true)

  trait LoginUntilReachable extends SessionApi:
    self =>

    protected def isTemporaryUnreachable(throwable: Throwable): Boolean =
      HttpClient.isTemporaryUnreachable(throwable)

    def hasSession: Boolean

    final def loginUntilReachable_(
      userAndPassword: Option[UserAndPassword],
      delays: Iterator[FiniteDuration] = defaultLoginDelays(),
      onError: Throwable => IO[Boolean] = this.onErrorTryAgain,
      onlyIfNotLoggedIn: Boolean = false)
    : IO[Completed] =
      IO.defer(
        if onlyIfNotLoggedIn && hasSession then
          IO.completed
        else {
          val sym = new BlockingSymbol
          login_(userAndPassword)
            .onErrorRestartLoop(()) { (throwable, _, retry) =>
              val isTemporary = isTemporaryUnreachable(throwable)
              onError(throwable).flatMap(continue =>
                if continue/*normally true*/ && delays.hasNext && isTemporary then
                  val prefix = isTemporary ?? {
                    sym.onWarn()
                    s"$sym "
                  }
                  warn(s"$prefix$self", throwable)
                  retry(()).delayBy(delays.next())
                else
                  IO.raiseError(throwable))
            }
            .guaranteeCase {
              case Outcome.Succeeded(_) => IO(
                logger.log(sym.relievedLogLevel, s"🟢 $self logged-in"))
              case Outcome.Canceled() => IO(
                logger.log(sym.relievedLogLevel, s"◼️ $self Canceled"))
              case _ => IO.unit
            }
        })

  trait HasUserAndPassword extends LoginUntilReachable:
    self =>
    protected def userAndPassword: Option[UserAndPassword]

    protected val loginDelays: () => Iterator[FiniteDuration] =
      () => defaultLoginDelays()

    override def retryIfSessionLost[A]()(body: IO[A]): IO[A] =
      IO.defer:
        val delays = Iterator(0.s) ++ loginDelays()
        login(onlyIfNotLoggedIn = true) *>
          ().tailRecM(_ =>
            body
              .onErrorRestartLoop(()):
                case (HttpException.HasProblem(problem), _, retry)
                  if problem.is(InvalidSessionTokenProblem) && delays.hasNext =>
                  renewSession(problem, delays) *>
                    retry(())
                case (throwable, _, _) =>
                  IO.raiseError(throwable)
              .flatMap: // A may be a Checked[Problem]
                case Left(problem: Problem)
                  if problem.is(InvalidSessionTokenProblem) && delays.hasNext =>
                  renewSession(problem, delays).as(Left(()))
                case o => IO.right(o)
          )

    private def renewSession(problem: Problem, delays: Iterator[FiniteDuration])
    : IO[Completed] =
      IO.defer:
        clearSession()
        // Race condition with a parallel operation,
        // which after the same error has already logged-in again successfully.
        // Should be okay if login is delayed like here
        logger.debug(s"$toString: Login again due to: $problem")
        IO.sleep(delays.next()) *>
          login()

    override final def retryUntilReachable[A](
      onError: Throwable => IO[Boolean] = this.onErrorTryAgain)
      (body: => IO[A])
    : IO[A] =
      IO.defer:
        val delays = loginDelays()
        val sym = new BlockingSymbol
        loginUntilReachable(delays, onError = onError, onlyIfNotLoggedIn = true)
          .flatMap((_: Completed) =>
            body.onErrorRestartLoop(()) { (throwable, _, retry) =>
              throwable
                .match {
                  case HttpException.HasProblem(problem)
                    if problem.is(InvalidSessionTokenProblem) && delays.hasNext =>
                    // Do not call onError on this minor problem
                    logger.debug(s"⟲ $toString: $problem")
                    loginUntilReachable(delays, onError = onError)

                  case e: HttpException if isTemporaryUnreachable(e) && delays.hasNext =>
                    onError(e).flatMap(continue =>
                    if continue then
                      sym.onWarn()
                      warn(s"$sym $toString", e)
                        loginUntilReachable(delays, onError = onError, onlyIfNotLoggedIn = true)
                    else
                      IO.raiseError(e))

                  case _ =>
                    IO.raiseError(throwable)
                }
                .*>(IO.sleep(delays.next()))
                .*>(retry(()))
            })
          .guaranteeCase:
            case Outcome.Succeeded(_) => IO(
              if sym.used then logger.info(s"🟢 $self reached"))
            case Outcome.Canceled() => IO(
              if sym.used then logger.info(s"◼️ $self Canceled"))
            case _ => IO.unit

    final def login(onlyIfNotLoggedIn: Boolean = false): IO[Completed] =
      login_(userAndPassword, onlyIfNotLoggedIn = onlyIfNotLoggedIn)

    final def loginUntilReachable(
      delays: Iterator[FiniteDuration] = defaultLoginDelays(),
      onError: Throwable => IO[Boolean] = this.onErrorTryAgain,
      onlyIfNotLoggedIn: Boolean = false)
    : IO[Completed] =
      loginUntilReachable_(userAndPassword, delays, onError, onlyIfNotLoggedIn = onlyIfNotLoggedIn)

  trait Dummy extends SessionApi.HasUserAndPassword:
    protected def userAndPassword = None

    def hasSession = true

    def login_(userAndPassword: Option[UserAndPassword], onlyIfNotLoggedIn: Boolean): IO[Completed] =
      IO.completed

    def logout(): IO[Completed] =
      IO.completed

    def clearSession(): Unit = {}

  private val tryLogoutTimeout = 1.s

  private val initialLoginDelays =
    val seq = Seq(
      100.ms, 900.ms, 1.s, 1.s, 1.s, 2.s, 2.s, 2.s, // 10s
      5.s, 5.s, // 20s
      5.s, 5.s, // 30s
      5.s, 5.s, // 40s
      5.s, 5.s, // 50s
      5.s, 5.s) // 60s
    assert(seq.reduce(_ + _) == 1.minute)
    seq

  def defaultLoginDelays(): Iterator[FiniteDuration] =
    initialLoginDelays.iterator ++ Iterator.continually(10.s)
